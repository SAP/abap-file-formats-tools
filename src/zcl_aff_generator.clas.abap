class zcl_aff_generator definition
  public
  final
  create public.

  public section.
    methods constructor
      importing
        writer type ref to Zif_aff_writer.

    methods generate_type
      importing
        data          type data
      returning
        value(result) type rswsourcet
      raising
        cx_aff_root.

    methods get_log
      returning
        value(log) type ref to if_aff_log.

  private section.
    data:
      writer type ref to Zif_aff_writer,
      log    type ref to if_aff_log.

    methods:
      check_input
        importing
          type_description type ref to cl_abap_typedescr,
      process_type_description
        importing
          type_description type ref to cl_abap_typedescr
          type_name        type string optional
        raising
          cx_aff_root,
      process_element
        importing
          element_description type ref to cl_abap_elemdescr
          element_name        type string optional
        raising
          cx_aff_root,
      process_structure
        importing
          structure_description type ref to cl_abap_structdescr
          structure_name        type string
        raising
          cx_aff_root,
      process_table
        importing
          table_description type ref to cl_abap_tabledescr
          table_name        type string
        raising
          cx_aff_root,
      process_include
        importing
          structure_description type ref to cl_abap_structdescr
        raising
          cx_aff_root,
      process_components
        importing
          components type cl_abap_structdescr=>component_table
        raising
          cx_aff_root,
      check_mandatory_fields
        importing
          structure_description type ref to cl_abap_structdescr.

endclass.


class zcl_aff_generator implementation.

  method constructor.
    me->writer = writer.
    me->log = cl_aff_factory=>create_log( ).
  endmethod.

  method generate_type.
    data(type_description) = cl_abap_typedescr=>describe_by_data( data ).
    check_input( type_description ).
    me->process_type_description( type_description ).
    result = me->writer->get_output( ).
    me->log->join( log_to_join = me->writer->get_log( ) ).
  endmethod.

  method check_input.
    try.
        data(structure_description) = cast cl_abap_structdescr( type_description ).
        check_mandatory_fields( structure_description ).
      catch cx_sy_move_cast_error.
        message w123(saff_core) into data(message) ##NEEDED.
        log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
    endtry.

  endmethod.

  method check_mandatory_fields.
    data(components) = structure_description->get_components( ).
    if not ( line_exists( components[ name = 'HEADER' ] ) and line_exists( components[ name = 'FORMAT_VERSION' ] ) ).
      message w124(saff_core) into data(message)  ##NEEDED.
      log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
    endif.
  endmethod.

  method process_type_description.
    case type_description->kind.
      when cl_abap_typedescr=>kind_elem.
        process_element(
          element_name        = type_name
          element_description = cast #( type_description ) ).
      when cl_abap_typedescr=>kind_struct.
        process_structure(
          structure_name        = type_name
          structure_description = cast #( type_description ) ).
      when cl_abap_typedescr=>kind_table.
        process_table(
          table_name        = type_name
          table_description = cast #( type_description ) ).
      when others.
        raise exception type cx_aff_root message e100(saff_core) with type_description->kind.
    endcase.
  endmethod.

  method process_element.
    data(name) = cond #( when element_name is not initial then element_name
                         else element_description->get_relative_name( ) ).
    me->writer->write_element(
      element_name        = name
      element_description = element_description ).
  endmethod.

  method process_structure.
    data(name) = cond #( when structure_name is not initial then structure_name
                         else structure_description->get_relative_name( ) ).
    me->writer->open_node(
      node_name        = name
      node_description = structure_description ).
    data(components) = structure_description->get_components( ).
    process_components( components ).
    me->writer->close_node(
      node_name        = name
      node_description = structure_description ).
  endmethod.

  method process_include.
    data(components) = structure_description->get_components( ).
    me->writer->open_include( structure_description ).
    process_components( components ).
    me->writer->close_include(  ).
  endmethod.

  method process_components.
    loop at components assigning field-symbol(<component>).
      if <component>-as_include = abap_true.
        process_include( cast #( <component>-type ) ).
      else.
        process_type_description(
          type_name        = <component>-name
          type_description = <component>-type ).
      endif.
    endloop.
  endmethod.

  method process_table.
    data(name) = cond #( when table_name is not initial then table_name
                         else table_description->get_relative_name( ) ).
    me->writer->open_node(
      node_name        = name
      node_description = table_description ).
    data(line_description) = table_description->get_table_line_type( ).
    process_type_description( line_description ).
    me->writer->close_node(
      node_name        = name
      node_description = table_description ).
  endmethod.


  method get_log.
    log = me->log.
  endmethod.

endclass.

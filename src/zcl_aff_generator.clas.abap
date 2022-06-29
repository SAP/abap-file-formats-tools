CLASS zcl_aff_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        writer TYPE REF TO zif_aff_writer.

    METHODS generate_type
      IMPORTING
        data          TYPE data
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        zcx_aff_tools.

    METHODS get_log
      RETURNING
        VALUE(log) TYPE REF TO zif_aff_log.

  PRIVATE SECTION.
    DATA:
      writer TYPE REF TO zif_aff_writer,
      log    TYPE REF TO zif_aff_log.

    METHODS:
      check_input
        IMPORTING
          type_description TYPE REF TO cl_abap_typedescr,
      process_type_description
        IMPORTING
          type_description TYPE REF TO cl_abap_typedescr
          type_name        TYPE string OPTIONAL
        RAISING
          zcx_aff_tools,
      process_element
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
          element_name        TYPE string OPTIONAL
        RAISING
          zcx_aff_tools,
      process_structure
        IMPORTING
          structure_description TYPE REF TO cl_abap_structdescr
          structure_name        TYPE string
        RAISING
          zcx_aff_tools,
      process_table
        IMPORTING
          table_description TYPE REF TO cl_abap_tabledescr
          table_name        TYPE string
        RAISING
          zcx_aff_tools,
      process_include
        IMPORTING
          structure_description TYPE REF TO cl_abap_structdescr
        RAISING
          zcx_aff_tools,
      process_components
        IMPORTING
          components TYPE cl_abap_structdescr=>component_table
        RAISING
          zcx_aff_tools,
      check_mandatory_fields
        IMPORTING
          structure_description TYPE REF TO cl_abap_structdescr.

ENDCLASS.


CLASS zcl_aff_generator IMPLEMENTATION.

  METHOD constructor.
    me->writer = writer.
    log = NEW zcl_aff_log( ).
  ENDMETHOD.

  METHOD generate_type.
    DATA(type_description) = cl_abap_typedescr=>describe_by_data( data ).
    check_input( type_description ).
    process_type_description( type_description ).
    result = writer->get_output( ).
    log->join( log_to_join = writer->get_log( ) ).
  ENDMETHOD.

  METHOD check_input.
    TRY.
        DATA(structure_description) = CAST cl_abap_structdescr( type_description ).
        check_mandatory_fields( structure_description ).
      CATCH cx_sy_move_cast_error.
        log->add_warning( message_text = zif_aff_log=>co_msg123 component_name = type_description->get_relative_name( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD check_mandatory_fields.
    DATA(components) = structure_description->get_components( ).
    IF NOT ( line_exists( components[ name = 'HEADER' ] ) AND line_exists( components[ name = 'FORMAT_VERSION' ] ) ).
      log->add_warning( message_text = zif_aff_log=>co_msg124 component_name = structure_description->get_relative_name( ) ).
    ENDIF.
  ENDMETHOD.

  METHOD process_type_description.
    CASE type_description->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        process_element(
          element_name        = type_name
          element_description = CAST #( type_description ) ).
      WHEN cl_abap_typedescr=>kind_struct.
        process_structure(
          structure_name        = type_name
          structure_description = CAST #( type_description ) ).
      WHEN cl_abap_typedescr=>kind_table.
        process_table(
          table_name        = type_name
          table_description = CAST #( type_description ) ).
      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_aff_tools( ).
    ENDCASE.
  ENDMETHOD.

  METHOD process_element.
    DATA(name) = COND #( WHEN element_name IS NOT INITIAL THEN element_name
                         ELSE element_description->get_relative_name( ) ).
    writer->write_element(
      element_name        = name
      element_description = element_description ).
  ENDMETHOD.

  METHOD process_structure.
    DATA(name) = COND #( WHEN structure_name IS NOT INITIAL THEN structure_name
                         ELSE structure_description->get_relative_name( ) ).
    writer->open_node(
      node_name        = name
      node_description = structure_description ).
    DATA(components) = structure_description->get_components( ).
    process_components( components ).
    writer->close_node(
      node_name        = name
      node_description = structure_description ).
  ENDMETHOD.

  METHOD process_include.
    DATA(components) = structure_description->get_components( ).
    writer->open_include( structure_description ).
    process_components( components ).
    writer->close_include( ).
  ENDMETHOD.

  METHOD process_components.
    LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).
      IF <component>-as_include = abap_true.
        process_include( CAST #( <component>-type ) ).
      ELSE.
        process_type_description(
          type_name        = <component>-name
          type_description = <component>-type ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD process_table.
    DATA(name) = COND #( WHEN table_name IS NOT INITIAL THEN table_name
                         ELSE table_description->get_relative_name( ) ).
    writer->open_node(
      node_name        = name
      node_description = table_description ).
    DATA(line_description) = table_description->get_table_line_type( ).
    process_type_description( line_description ).
    writer->close_node(
      node_name        = name
      node_description = table_description ).
  ENDMETHOD.


  METHOD get_log.
    log = me->log.
  ENDMETHOD.

ENDCLASS.

class zcl_aff_writer definition
  public
  abstract
  create public .

  public section.
    interfaces Zif_aff_writer
      final methods open_node close_node write_element get_output.

    methods constructor.

  protected section.
    types:
      begin of ty_stack_entry,
        operation type Zif_aff_writer=>enum_operation,
        name      type string,
      end of ty_stack_entry.

    types:
      begin of ty_structure_stack,
        name                       type string,
        absolute_name              type abap_abstypename,
      end of ty_structure_stack,
      tt_structure_stack type standard table of ty_structure_stack.

    data:
      output                     type rswsourcet,
      formatting_option          type Zif_aff_writer=>enum_formatting_option,
      name_mappings              type Zif_aff_writer=>ty_name_mappings,
      abap_value_mappings        type Zif_aff_writer=>ty_abap_value_mappings,
      content                    type rswsourcet,
      stack_of_structure         type tt_structure_stack,
      stack                      type standard table of ty_stack_entry,
      indent_level               type i value 0,
      c_indent_number_characters type i value 2,
      log                        type ref to if_aff_log,
      abap_doc_parser            type ref to cl_aff_abap_doc_parser,
      ignore_til_indent_level    type i.

    methods: get_value_mapping_for_element
      importing abap_element  type string
      returning value(result) type Zif_aff_writer=>ty_abap_value_mapping,
      map_and_format_name
        importing name          type string
        returning value(result) type string,
      get_json_type_from_description
        importing element_description type ref to cl_abap_elemdescr
        returning value(result)       type Zif_aff_writer=>enum_type_info
        raising   cx_aff_root,

      write_open_tag final
        importing
          line type string,
      write_tag_with_variable_indent final
        importing
          line         type string
          indent_level type i,
      write_closing_tag final
        importing
          line type string,
      add_to_stack final
        importing
          entry type ty_stack_entry,
      last_operation final
        returning value(result) type Zif_aff_writer=>enum_operation,
      append_to_previous_line final
        importing
          string type string,
      append_before_output,
      append_after_output,

      write_tag abstract
        importing
          line type string,

      write_element abstract
        importing
                  element_name        type string
                  element_description type ref to cl_abap_elemdescr
        raising   cx_aff_root,

      open_structure abstract
        importing
                  structure_name        type string
                  structure_description type ref to cl_abap_typedescr
        raising   cx_aff_root,

      close_structure abstract
        importing
                  structure_name        type string
                  structure_description type ref to cl_abap_typedescr
        raising   cx_aff_root,

      open_table abstract
        importing
                  table_name        type string
                  table_description type ref to cl_abap_typedescr
        raising   cx_aff_root,

      close_table abstract
        importing
                  table_name        type string
                  table_description type ref to cl_abap_typedescr
        raising   cx_aff_root,

      apply_formatting
        importing name          type string
        returning value(result) type string,

      call_reader_and_decode
        importing
          name_of_source       type string
          element_name         type string
        returning
          value(read_abap_doc) type cl_aff_abap_doc_parser=>abap_doc,

      delete_first_of_struc_stack,

      get_all_path_information
        importing
          name                    type string
        exporting
          value(source_type)      type string
          value(source)           type string
          value(fullname_of_type) type string,

      get_structure_of_enum_values
        importing
          link_to_values             type string
          fullname_of_type           type string
        exporting
          value(structure_of_values) type ref to cl_abap_structdescr
          value(name_of_source)      type string
          value(name_of_constant)    type string,


      get_abap_doc_for_absolute_name
        importing
          absolute_name   type abap_abstypename
        returning
          value(abap_doc) type cl_aff_abap_doc_parser=>abap_doc,

      compare_abap_doc
        importing
          abap_doc_additional type cl_aff_abap_doc_parser=>abap_doc
        changing
          abap_doc_base       type cl_aff_abap_doc_parser=>abap_doc,

      get_splitted_absolute_name
        importing
          absolute_name type abap_abstypename
        returning
          value(result) type rswsourcet,

      get_default_from_link
        importing
          link                 type string
          fullname_of_type     type string
          element_type         type abap_typekind
        returning
          value(default_value) type string,

      remove_leading_trailing_spaces
        changing
          string_to_work_on type string,

      is_callback_class_valid
        importing
          class_name      type string
          component_name  type string
        returning
          value(is_valid) type abap_boolean,

      is_default_value_valid
        importing
                  element_description type ref to cl_abap_elemdescr
                  default_value       type string
                  fullname_of_type    type string
        returning value(is_valid)     type abap_boolean
        raising
                  cx_aff_root,

      is_sy_langu
        importing
          element_description type ref to cl_abap_elemdescr
        returning
          value(result)       type abap_bool.

  private section.
    constants:
      begin of c_abap_types,
        boolean   type string value `ABAP_BOOLEAN;ABAP_BOOL;BOOLEAN;BOOLE_D;XFELD;XSDBOOLEAN;FLAG`,
        timestamp type string value `TIMESTAMP;TIMESTAMPL`,
      end of c_abap_types.


    methods:
      get_mapped_name
        importing name          type string
        returning value(result) type string,

      is_type_timestamp
        importing element_description type ref to cl_abap_elemdescr
        returning value(result)       type abap_boolean,

      is_type_boolean
        importing element_description type ref to cl_abap_elemdescr
        returning value(result)       type abap_boolean,

      get_constant_as_struc
        importing
          name_of_source           type string
          name_of_constant         type string
          fullname_of_type         type string
        returning
          value(constant_as_struc) type ref to cl_abap_structdescr,

      get_infos_of_values_link
        importing
          values_link             type string
        exporting
          value(name_of_source)   type string
          value(name_of_constant) type string,

      validate_default_link
        importing
          splitted_link    type rswsourcet
          fullname_of_type type string
          element_type     type abap_typekind
        returning
          value(is_valid)  type abap_boolean.



endclass.

class zcl_aff_writer implementation.

  method constructor.
    log = cl_aff_factory=>create_log( ).
    abap_doc_parser = new cl_aff_abap_doc_parser( ).
  endmethod.


  method Zif_aff_writer~set_abap_value_mappings.
    me->abap_value_mappings = abap_value_mappings.
  endmethod.


  method Zif_aff_writer~set_name_mappings.
    me->name_mappings = name_mappings.
  endmethod.


  method Zif_aff_writer~set_formatting_option.
    me->formatting_option = formatting_option.
  endmethod.


  method get_value_mapping_for_element.
    data(abap_element_upper) = to_upper( abap_element ).
    data(abap_value_mappings_upper) = value Zif_aff_writer=>ty_abap_value_mappings(
      for abap_value_mapping in me->abap_value_mappings (
        abap_element   = to_upper( abap_value_mapping-abap_element )
        target_type    = abap_value_mapping-target_type
        value_mappings = abap_value_mapping-value_mappings
      )
    ).
    result = value #( abap_value_mappings_upper[ abap_element = abap_element_upper ] optional ) ##WARN_OK.
  endmethod.


  method map_and_format_name.
    data(mapped_name) = me->get_mapped_name( name ).
    if mapped_name is not initial.
      result = mapped_name.
    else.
      result = me->apply_formatting( name ).
    endif.
  endmethod.


  method get_mapped_name.
    data(name_upper) = to_upper( name ).
    data(name_mappings_upper) = value Zif_aff_writer=>ty_name_mappings(
      for name_mapping in me->name_mappings (
        abap = to_upper( name_mapping-abap )
        json = name_mapping-json
      )
    ).
    result = value #( name_mappings_upper[ abap = name_upper ]-json optional ) ##WARN_OK.
  endmethod.


  method apply_formatting.
    case me->formatting_option.
      when Zif_aff_writer=>formatting_option-camel_case.
        data(lower_name) = to_lower( name ).
        result = to_mixed( lower_name ).
      when others.
        result = name.
    endcase.
  endmethod.


  method get_json_type_from_description.
    case element_description->type_kind.
      when cl_abap_typedescr=>typekind_string or cl_abap_typedescr=>typekind_csequence or
           cl_abap_typedescr=>typekind_clike or cl_abap_typedescr=>typekind_char or
           cl_abap_typedescr=>typekind_w or cl_abap_typedescr=>typekind_xstring or
           cl_abap_typedescr=>typekind_hex or cl_abap_typedescr=>typekind_num or cl_abap_typedescr=>typekind_enum .
        result = cond #( when is_type_boolean( element_description ) then Zif_aff_writer=>type_info-boolean
                         else Zif_aff_writer=>type_info-string ).
      when cl_abap_typedescr=>typekind_float or cl_abap_typedescr=>typekind_int or
           cl_abap_typedescr=>typekind_int1 or cl_abap_typedescr=>typekind_int2 or
           cl_abap_typedescr=>typekind_int8 or cl_abap_typedescr=>typekind_decfloat or
           cl_abap_typedescr=>typekind_decfloat16 or cl_abap_typedescr=>typekind_decfloat34  or cl_abap_typedescr=>typekind_numeric.
        result = Zif_aff_writer=>type_info-numeric.
      when cl_abap_typedescr=>typekind_packed.
        result = cond #( when is_type_timestamp( element_description ) then Zif_aff_writer=>type_info-date_time
                         else Zif_aff_writer=>type_info-numeric ).
      when cl_abap_typedescr=>typekind_date or cl_abap_typedescr=>typekind_time or
           cl_abap_typedescr=>typekind_utclong.
        result = Zif_aff_writer=>type_info-date_time.
      when others.
        raise exception type cx_aff_root message e100(saff_core) with element_description->type_kind.
    endcase.
  endmethod.


  method is_type_boolean.
    data(type_name) = element_description->get_relative_name( ).
    result = xsdbool( element_description->output_length = 1 and ( type_name is not initial and c_abap_types-boolean cs type_name ) ).
  endmethod.


  method is_type_timestamp.
    data(type_name) = element_description->get_relative_name( ).
    result = xsdbool( type_name is not initial and c_abap_types-timestamp cs type_name ).
  endmethod.


  method Zif_aff_writer~write_element.
    write_element( element_name = element_name element_description = element_description ).
    add_to_stack( value #( operation = Zif_aff_writer=>operation-write_element name = element_name ) ).
  endmethod.


  method Zif_aff_writer~open_node.
    case node_description->kind.
      when cl_abap_typedescr=>kind_struct.
        open_structure( structure_name = node_name  structure_description = node_description ).
        add_to_stack( value #( operation = Zif_aff_writer=>operation-open_structure name = node_name ) ).

      when cl_abap_typedescr=>kind_table.
        open_table( table_name = node_name  table_description = node_description ).
        add_to_stack( value #( operation = Zif_aff_writer=>operation-open_table name = node_name ) ).
      when others.
        raise exception type cx_aff_root message e101(saff_core) with node_description->kind.
    endcase.
  endmethod.


  method Zif_aff_writer~close_node.
    case node_description->kind.
      when cl_abap_typedescr=>kind_struct.
        close_structure( structure_name = node_name  structure_description = node_description ).
        add_to_stack( value #( operation = Zif_aff_writer=>operation-close_structure name = node_name ) ).

      when cl_abap_typedescr=>kind_table.
        close_table( table_name = node_name  table_description = node_description ).
        add_to_stack( value #( operation = Zif_aff_writer=>operation-close_table name = node_name ) ).

      when others.
        raise exception type cx_aff_root message e101(saff_core) with node_description->kind.
    endcase.
  endmethod.


  method Zif_aff_writer~get_output.
    append_before_output( ).
    append lines of content to output.
    append_after_output( ).
    result = output.
  endmethod.


  method write_tag_with_variable_indent.
    append |{ repeat( val = ` `  occ = indent_level * c_indent_number_characters ) }{ line }| to content.
  endmethod.


  method write_open_tag.
    write_tag( line ).
    indent_level += 1.
  endmethod.


  method write_closing_tag.
    indent_level -= 1.
    write_tag( line ).
  endmethod.


  method add_to_stack.
    insert entry into stack index 1.
  endmethod.


  method last_operation.
    result = value #( stack[ 1 ]-operation optional ).
  endmethod.


  method append_to_previous_line.
    data(index) = lines( me->content ).
    if index > 0.
      me->content[ index ] = me->content[ index ] && string.
    else.
      insert string into table me->content.
    endif.
  endmethod.


  method append_after_output ##NEEDED.

  endmethod.


  method append_before_output ##NEEDED.

  endmethod.


  method call_reader_and_decode.
    data(reader) = new cl_oo_abap_doc_reader( ).
    try.
        reader->get_abap_doc_for_element(
          exporting
            clif_name    = conv #( name_of_source )
            element_name = element_name
          receiving
            result       = data(result)
        ).
        read_abap_doc = abap_doc_parser->parse(
          exporting
            component_name = element_name
            to_parse       = result
          changing
            log            = log
        ).
      catch cx_root ##NO_HANDLER ##CATCH_ALL.
    endtry.
  endmethod.


  method remove_leading_trailing_spaces.
    shift string_to_work_on right deleting trailing space.
    shift string_to_work_on left deleting leading space.
  endmethod.


  method delete_first_of_struc_stack.
    if stack_of_structure is not initial.
      delete stack_of_structure index 1.
    endif.
  endmethod.


  method get_all_path_information.
    data previous_absolute_name type abap_abstypename.
    data splitted_prev_name type  rswsourcet.
    data(index) = 0.
    while lines( splitted_prev_name ) <= 2.
      if index >= lines( stack_of_structure ).
        return.
      endif.
      index = index + 1.
      previous_absolute_name = stack_of_structure[ index ]-absolute_name.
      splitted_prev_name = get_splitted_absolute_name( previous_absolute_name ).
    endwhile.
    data(name_of_prev) = splitted_prev_name[ lines( splitted_prev_name ) ].
    source_type = splitted_prev_name[ 1 ].
    source = splitted_prev_name[ 2 ].
    fullname_of_type = name_of_prev && '-'.
    index = index - 1.
    while index > 0.
      fullname_of_type = fullname_of_type  && stack_of_structure[ index ]-name && '-'.
      index = index - 1.
    endwhile.
    fullname_of_type = fullname_of_type && name.
  endmethod.


  method get_splitted_absolute_name.
    data(place_of_type) = absolute_name.
    split place_of_type at '\' into table data(splitted_in_componets).
    loop at splitted_in_componets assigning field-symbol(<component>).
      if <component> is not initial.
        split <component> at '=' into table data(splitted_in_details).
        append lines of splitted_in_details to result.
      endif.
    endloop.
  endmethod.

  method get_structure_of_enum_values.
    get_infos_of_values_link(
      exporting
        values_link      = link_to_values
      importing
        name_of_source   = name_of_source
        name_of_constant = name_of_constant
    ).

    structure_of_values = get_constant_as_struc(
      name_of_source   = name_of_source
      name_of_constant = name_of_constant
      fullname_of_type = fullname_of_type
    ).
  endmethod.

  method get_constant_as_struc.
    data clstype type  seoclstype.
    call function 'SEO_CLIF_EXISTENCE_CHECK'
      exporting
        cifkey        = conv seoclskey( name_of_source )
      importing
        clstype       = clstype
      exceptions
        not_specified = 1
        not_existing  = 2
        others        = 3.
    if sy-subrc <> 0.
*    class or interface doesn't exist
      message w103(saff_core) with name_of_source fullname_of_type into data(message) ##NEEDED.
      log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
    else.
      data(constant_descr) = cl_abap_typedescr=>describe_by_name( name_of_source ).

      if clstype = seoc_clstype_interface.
        data(constant_descr_intf) = cast cl_abap_intfdescr( constant_descr ).
        constant_descr_intf->get_attribute_type(
          exporting
            p_name              = name_of_constant
          receiving
            p_descr_ref         = data(constant)
          exceptions
            attribute_not_found = 1
            others              = 2
        ).
        if sy-subrc <> 0.
*      constant in interface does not exist
          message w104(saff_core) with name_of_source && '=>' && name_of_constant fullname_of_type into message ##NEEDED.
          log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
        endif.
      elseif clstype = seoc_clstype_class.
        data(constant_descr_clas) = cast cl_abap_classdescr( constant_descr ).
        constant_descr_clas->get_attribute_type(
          exporting
            p_name              = name_of_constant
          receiving
            p_descr_ref         = constant
          exceptions
            attribute_not_found = 1
            others              = 2
        ).
        if sy-subrc <> 0.
*      constant in class does not exits
          message w104(saff_core) with name_of_source && '=>' && name_of_constant fullname_of_type into message ##NEEDED.
          log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
        endif.
      endif.
      constant_as_struc = cast cl_abap_structdescr( constant ).
    endif.
  endmethod.


  method get_infos_of_values_link.
    data(link) = values_link.
    replace all occurrences of pcre `[\s]` in link with ``.
    replace all occurrences of `data:` in link with ``.
    split link at '.' into table data(split_at_point).
    if lines( split_at_point ) = 2.
      name_of_source = to_upper( split_at_point[ 1 ] ).
      name_of_constant = to_upper( split_at_point[ 2 ] ).
    endif.
  endmethod.


  method get_abap_doc_for_absolute_name.
    data(splitted_prev_name) = get_splitted_absolute_name( absolute_name ).
    if lines( splitted_prev_name ) >= 4.
      data(source_type) = splitted_prev_name[ 1 ].
      data(source) = splitted_prev_name[ 2 ].
      data(fullname_of_type) = splitted_prev_name[ 4 ].
      if source_type = 'CLASS' or source_type = 'INTERFACE'.
        abap_doc = call_reader_and_decode( name_of_source = source element_name   = fullname_of_type ).
      endif.
    endif.
  endmethod.


  method compare_abap_doc.
    if abap_doc_base-enumvalues_link is initial.
      abap_doc_base-enumvalues_link = abap_doc_additional-enumvalues_link.
    endif.
    if abap_doc_base-title is initial and abap_doc_base-description is initial.
      abap_doc_base-title = abap_doc_additional-title.
      abap_doc_base-description = abap_doc_additional-description.
    endif.
    if abap_doc_base-minimum is initial and abap_doc_base-maximum is initial and abap_doc_base-exclusive_maximum is initial and abap_doc_base-exclusive_minimum is initial.
      abap_doc_base-minimum = abap_doc_additional-minimum.
      abap_doc_base-maximum = abap_doc_additional-maximum.
      abap_doc_base-exclusive_minimum = abap_doc_additional-exclusive_minimum.
      abap_doc_base-exclusive_maximum = abap_doc_additional-exclusive_maximum.
    endif.
    if abap_doc_base-multiple_of is initial.
      abap_doc_base-multiple_of = abap_doc_additional-multiple_of.
    endif.
    if abap_doc_base-max_length is initial and abap_doc_base-min_length is initial.
      abap_doc_base-min_length = abap_doc_additional-min_length.
      abap_doc_base-max_length = abap_doc_additional-max_length.
    endif.
    if abap_doc_base-default is initial.
      abap_doc_base-default = abap_doc_additional-default.
    endif.
    if abap_doc_base-callback_class is initial.
      abap_doc_base-callback_class = abap_doc_additional-callback_class.
    endif.
  endmethod.


  method get_default_from_link.
    data(link_to_work_on) = link.
    replace all occurrences of pcre `(@link|data:)` in link_to_work_on with ``.
    replace all occurrences of pcre `[\s]` in link_to_work_on with ``.
    split link_to_work_on at '.' into table data(splitted).
    if validate_default_link( splitted_link = splitted fullname_of_type = fullname_of_type element_type = element_type ) = abap_true.
      data(default_abap) = splitted[ lines( splitted ) ].
      default_value = apply_formatting( default_abap ).
    endif.
  endmethod.

  method Zif_aff_writer~get_log.
    log = me->log.
  endmethod.

  method is_callback_class_valid.
    data(name_of_callback_class) = to_upper( class_name ).
    select single @abap_true from seoclass where clsname = @name_of_callback_class into @data(callback_class_exists).
    if callback_class_exists = abap_true.
      select single @abap_true from seocompo where clsname = @name_of_callback_class and cmpname = 'GET_SUBSCHEMA' and cmptype = 1 into @data(get_subschema_exists).
      select single @abap_true from seocompo where clsname = @name_of_callback_class and cmpname = 'SERIALIZE' and cmptype = 1 into @data(serialize_exists).
      select single @abap_true from seocompo where clsname = @name_of_callback_class and cmpname = 'DESERIALIZE' and cmptype = 1 into @data(deserialize_exists).
      is_valid = xsdbool( get_subschema_exists = abap_true and serialize_exists = abap_true and deserialize_exists = abap_true ).
    endif.
    if is_valid = abap_false.
      message w106(saff_core) with component_name into data(message) ##NEEDED.
      log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
    endif.
  endmethod.

  method validate_default_link.
    if lines( splitted_link ) = 3.
      data(source_name) = to_upper( splitted_link[ 1 ] ).
      data(constant_name) = to_upper( splitted_link[ 2 ] ).
      data(component_name) = to_upper( splitted_link[ 3 ] ).
      data(constant_description) = get_constant_as_struc(
        name_of_source   = source_name
        name_of_constant = constant_name
        fullname_of_type = fullname_of_type
      ).
      if constant_description is not initial.
        data(components) = constant_description->get_components( ).
        data(row) = value #( components[ name = component_name ] optional ).
        if row is not initial.
          if row-type->type_kind = element_type.
            is_valid = abap_true.
          else.
            message w122(saff_core) with constant_name fullname_of_type into data(message) ##NEEDED.
            log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
          endif.
        else.
          message w105(saff_core) with component_name constant_name fullname_of_type into message ##NEEDED.
          log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
        endif.
      endif.
    endif.
  endmethod.


  method is_default_value_valid.
    data(default) = default_value.
    replace all occurrences of `"` in default with ``.
    data(type) = get_json_type_from_description( element_description ).
    data r_field type ref to data.
    field-symbols <field> type any.
    create data r_field type handle element_description.
    assign r_field->* to <field>.
    if element_description->type_kind = cl_abap_typedescr=>typekind_utclong.
*      No support for default with utclong
      message w117(saff_core) with 'UTCLONG'  fullname_of_type into data(message) ##NEEDED.
      log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
      is_valid = abap_false.
      return.
    elseif type = Zif_aff_writer=>type_info-boolean.
      default = to_lower( default ).
      if default = 'abap_true' or default = 'x' or default = 'abap_false' or default = ''.
        is_valid = abap_true.
      endif.
    elseif type = Zif_aff_writer=>type_info-string or type = Zif_aff_writer=>type_info-date_time.
      data string type string.
      try.
          <field> = default.
          string = <field>.
          if element_description->type_kind = cl_abap_typedescr=>typekind_num or element_description->type_kind = cl_abap_typedescr=>typekind_numeric.
            shift string left deleting leading '0'.
          endif.
          if element_description->type_kind = cl_abap_typedescr=>typekind_time.
            default = default && repeat( val = '0' occ = 6 - strlen( default ) ).
          endif.
          if element_description->type_kind = cl_abap_typedescr=>typekind_utclong.
            replace pcre `T|t` in default with ` `.
          endif.
          remove_leading_trailing_spaces( changing string_to_work_on = string ).
          remove_leading_trailing_spaces( changing string_to_work_on = default ).
          if string = default.
            is_valid = abap_true.
          else.
            is_valid = abap_false.
          endif.
        catch cx_root.
          is_valid = abap_false.
      endtry.
    elseif type = Zif_aff_writer=>type_info-numeric.
      try.
          <field> = default.
          if <field> - default = 0.
            is_valid = abap_true.
          else.
            is_valid = abap_false.
          endif.
        catch cx_root.
          is_valid = abap_false.
      endtry.
    endif.
    if is_valid = abap_false.
      message w114(saff_core) with fullname_of_type into message ##NEEDED.
      log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
    endif.
  endmethod.


  method Zif_aff_writer~validate.
    result = abap_true.
  endmethod.


  method Zif_aff_writer~close_include.
    delete_first_of_struc_stack( ).
  endmethod.


  method Zif_aff_writer~open_include.
    insert value #( absolute_name = include_description->absolute_name ) into me->stack_of_structure index 1.
  endmethod.


  method is_sy_langu.
    data(sy_langu_description) = cast cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( value sy-langu( ) ) ).
    result = xsdbool( sy_langu_description->edit_mask = element_description->edit_mask ).
  endmethod.

endclass.

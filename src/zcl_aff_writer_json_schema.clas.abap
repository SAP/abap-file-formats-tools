"! Writer for a JSON schema. This is just a utility class helping to create a JSON schema.
"! The generated schema must be reviewed and adapted!
class zcl_aff_writer_json_schema definition
  public
  final
  create public
  inheriting from zcl_aff_writer.

  public section.

    constants:
      c_schema_specification type string value 'https://json-schema.org/draft/2020-12/schema' ##NO_TEXT,
      c_link_to_repository   type string value 'https://github.com/SAP/abap-file-formats' ##NO_TEXT.

    methods:
      "! Creates a JSON schema writer<br><br>
      "! Example for schema_id: http://sap.com/schema/nrob.json
      "!
      "! @parameter schema_id | The schema that should be written in the $id field of the schema
      "! @parameter format_version | The version of the ABAP file format as integer
      constructor
        importing schema_id      type string
                  format_version type i default 1,

      zif_aff_writer~validate redefinition.

  protected section.
    methods:
      write_element redefinition,
      open_structure redefinition,
      close_structure redefinition,
      open_table redefinition,
      write_tag redefinition,
      close_table redefinition,
      append_after_output redefinition.


  private section.

    types:
      begin of ty_buffer,
        name            type string,
        number_brackets type i,
      end of ty_buffer,
      tt_buffer type standard table of ty_buffer.

    constants:
      c_format_version            type string value 'FORMAT_VERSION',
      c_max_length_of_description type i value 253.

    data:
      schema_id              type string,
      structure_buffer       type tt_buffer,
      table_buffer           type tt_buffer,
      ignore_next_elements   type abap_boolean,
      enum_values            type rswsourcet,
      enum_titles            type rswsourcet,
      enum_descriptions      type rswsourcet,
      stack_of_required_tabs type standard table of stringtab,
      format_version         type i.

    methods: append_comma_to_prev_line,

      get_json_schema_type
        importing element_name        type string
                  element_description type ref to cl_abap_elemdescr
                  json_type           type zif_aff_writer=>enum_type_info
        returning value(result)       type string
        raising   cx_aff_root,

      open_json_schema_for_structure
        importing structure_name        type string
                  structure_description type ref to cl_abap_typedescr
        raising   cx_aff_root,

      open_json_schema_for_table
        importing table_name        type string
                  table_description type ref to cl_abap_tabledescr
        raising   cx_aff_root,

      open_json_schema_for_element,

      get_description
        importing type_description type ref to cl_abap_typedescr optional
        returning value(result)    type string,

      get_enum_values
        importing element_name        type string
                  element_description type ref to cl_abap_elemdescr
        returning value(result)       type rswsourcet
        raising
                  cx_aff_root,

      get_enum_descriptions
        importing element_name        type string
                  element_description type ref to cl_abap_elemdescr
        returning value(result)       type rswsourcet,

      type_is_integer
        importing
          element_description type ref to cl_abap_elemdescr
        returning
          value(result)       type abap_bool,

      set_enum_properties
        importing
          enum_type type abap_typekind
        raising
          cx_aff_root,

      add_required_table_to_stack,

      delete_first_tab_of_req_stack,

      write_req_and_add_props,

      get_format
        importing
          element_description type ref to cl_abap_elemdescr
        returning
          value(result)       type string,

      date_time_from_abap_to_json
        importing
          date_time_abap        type string
          element_description   type ref to cl_abap_elemdescr
        returning
          value(date_time_json) type string,

      handle_default
        importing
          element_description type ref to cl_abap_elemdescr
          json_type           type zif_aff_writer=>enum_type_info
        raising
          cx_aff_root,

      handle_extrema
        importing
          element_description type ref to cl_abap_elemdescr
          element_name        type string,

      handle_string
        importing
          element_description type ref to cl_abap_elemdescr,

      handle_language_field,

      handle_enums
        importing
          element_description type ref to cl_abap_elemdescr
          element_name        type string
          enums               type rswsourcet,

      write_subschema
        importing
          callback_class type string,

      reset_indent_level_tag,

      write_enum_properties
        importing
          property_table type rswsourcet,

      check_title_and_description
        importing abap_doc_to_check        type cl_aff_abap_doc_parser=>abap_doc
                  fullname_of_checked_type type string,


      write_title_and_description
        importing
          type_description type ref to cl_abap_typedescr
          check_not_needed type abap_boolean default abap_false,
      set_abapdoc_fullname_element
        importing
          element_description type ref to cl_abap_elemdescr
          element_name        type string
          splitted_prev_name  type rswsourcet,
      set_abapdoc_fullname_struc_tab
        importing
          type_description type ref to cl_abap_typedescr
          type_name        type string.

endclass.


class zcl_aff_writer_json_schema implementation.


  method constructor.
    super->constructor( ).
    me->formatting_option = zif_aff_writer=>formatting_option-camel_case.
    me->schema_id = schema_id.
    me->format_version = format_version.
    zif_aff_writer~set_name_mappings(  value #( ( abap = 'schema' json = '$schema' ) ) ) ##NO_TEXT.
  endmethod.


  method write_element.
    if ignore_next_elements = abap_true.
      return.
    endif.

    clear_type_specifics( ).
    clear enum_titles.
    clear enum_descriptions.
    clear enum_values.

    append_comma_to_prev_line( ).
    data(json_type) = get_json_type_from_description( element_description ).
    data(mapped_and_formatted_name) = map_and_format_name( element_name ).

    data(splitted_prev_name) = get_splitted_absolute_name( element_description->absolute_name ).
    set_abapdoc_fullname_element( element_description = element_description element_name = element_name splitted_prev_name = splitted_prev_name ).

    if abap_doc-required = abap_true and lines( stack_of_required_tabs ) >= 1.
      field-symbols <table1> type stringtab.
      assign stack_of_required_tabs[ 1 ] to <table1> .
      append mapped_and_formatted_name to <table1>.
    endif.

    data(callback_class) =  to_upper( abap_doc-callback_class ).
    if callback_class is not initial and is_callback_class_valid( class_name = callback_class component_name = fullname_of_type ).
      if last_operation( ) = zif_aff_writer=>operation-initial.
        open_json_schema_for_element( ).
      endif.
      write_subschema( callback_class = callback_class ).
      if last_operation( ) = zif_aff_writer=>operation-initial.
        write_closing_tag( `}` ).
      endif.
      clear ignore_til_indent_level.
      return.
    endif.


    if last_operation( ) = zif_aff_writer=>operation-initial.
      open_json_schema_for_element( ).
    elseif last_operation( ) <> zif_aff_writer=>operation-open_table.
      write_open_tag( |"{ mapped_and_formatted_name }": \{| ).
    endif.

    data(enums) = get_enum_values( element_name = element_name element_description = element_description ).
    if enums is not initial.
      json_type = zif_aff_writer=>type_info-string.
    endif.

    data(check_not_needed) = abap_false.

    if last_operation( ) = zif_aff_writer=>operation-open_table and lines( splitted_prev_name ) = 2 and splitted_prev_name[ 2 ] = element_name.
      check_not_needed = abap_true.
    endif.

    write_title_and_description( type_description = element_description check_not_needed = check_not_needed ).

    if element_name = c_format_version.
      write_tag( `"type": "string",`).
      write_tag( |"const": "{ format_version }",| ).
    else.
      write_tag( |"type": "{ get_json_schema_type( element_name = element_name element_description = element_description json_type = json_type ) }",| ).
      data(format) = get_format( element_description ).
      if format is not initial.
        write_tag( |"format": "{ format }",| ).
      endif.

      if enums is not initial.
        handle_enums( element_description = element_description element_name = element_name enums = enums ).
      else. "non- enum
        if json_type = zif_aff_writer=>type_info-numeric.
          handle_extrema( element_description = element_description element_name = element_name ).
        elseif json_type = zif_aff_writer=>type_info-string and not ( element_description->type_kind = cl_abap_typedescr=>typekind_date or element_description->type_kind = cl_abap_typedescr=>typekind_time or
             element_description->type_kind = cl_abap_typedescr=>typekind_utclong ).
          if is_sy_langu( element_description ).
            handle_language_field( ).
          else.
            handle_string( element_description = element_description ).
          endif.
        endif.
      endif.

      if abap_doc-default is not initial.
        handle_default( element_description = element_description json_type = json_type ).
      endif.
    endif.

*    remove "," in last line
    if ignore_til_indent_level > indent_level or ignore_til_indent_level is initial.
      data(last_line) = content[ lines( content ) ].
      content[ lines( content ) ] = substring( val = last_line off = 0 len = strlen( last_line ) - 1 ).
    endif.

    if last_operation( ) <> zif_aff_writer=>operation-open_table.
      write_closing_tag( `}` ).
    endif.
  endmethod.


  method write_title_and_description.
    if check_not_needed = abap_false.
      check_title_and_description( abap_doc_to_check = abap_doc fullname_of_checked_type = fullname_of_type ).
    endif.
    data(title) = escape( val = abap_doc-title format = cl_abap_format=>e_json_string ).
    data(description) = escape( val = get_description( type_description = type_description ) format = cl_abap_format=>e_json_string ).
    if title is not initial.
      write_tag( |"title": "{ title }",| ).
    endif.
    if description is not initial.
      write_tag( |"description": "{  description }",|  ).
    endif.
  endmethod.


  method handle_enums.
    write_tag( `"enum": [`) .
    write_enum_properties( enums ).

    if enum_titles is not initial.
      write_tag( `"enumTitles": [`) .
      write_enum_properties( enum_titles ).
    endif.

    data(enum_descr) = get_enum_descriptions( element_name = element_name element_description = element_description ).
    write_tag( `"enumDescriptions": [`) .
    write_enum_properties( enum_descr ).
  endmethod.


  method write_enum_properties.
    indent_level = indent_level + 1.
    loop at property_table assigning field-symbol(<value>).
      if sy-tabix < lines( property_table ).
        write_tag( |"{ <value> }",| ) .
      else.
        write_tag( |"{ <value> }"| ) .
      endif.
    endloop.
    indent_level = indent_level - 1.
    write_tag( `],` ).
  endmethod.


  method handle_string.
    if abap_doc-max_length is not initial.
      data(max_length) = abap_doc-max_length.
    else.
      max_length = new cl_aff_extreme_values( )->get_max_length( element_description ).
    endif.
    if abap_doc-min_length is not initial.
      write_tag( |"minLength": { abap_doc-min_length },| ).
    endif.
    if max_length is not initial.
      write_tag( |"maxLength": { max_length },| ).
      if element_description->type_kind = cl_abap_typedescr=>typekind_num.
        write_tag( `"pattern": "^[0-9]+$",` ).
      endif.
    endif.
  endmethod.


  method handle_extrema.
    if get_value_mapping_for_element( element_name ) is initial.
      new cl_aff_extreme_values( )->get_extrema(
        exporting
          element_description = element_description
        importing
          max                 = data(max_value)
          min                 = data(min_value)
      ).
    endif.
    data(multiple_of) = abap_doc-multiple_of.

    if multiple_of is initial and element_description->type_kind = cl_abap_typedescr=>typekind_packed.
      data(decimals) = element_description->decimals.
      if decimals > 0.
        multiple_of = |0.{ repeat( val = `0`  occ = decimals - 1 ) }1|.
      endif.
    endif.

    data(exclusive_minimum) = abap_doc-exclusive_minimum.
    data(exclusive_maximum) = abap_doc-exclusive_maximum.

    if exclusive_minimum is not initial.
      clear min_value.
    elseif abap_doc-minimum is not initial.
      min_value = abap_doc-minimum.
    endif.

    if exclusive_maximum is not initial.
      clear max_value.
    elseif abap_doc-maximum is not initial.
      max_value = abap_doc-maximum.
    endif.

    if min_value is not initial.
      write_tag( |"minimum": { min_value },| ).
    endif.
    if exclusive_minimum is not initial.
      write_tag( |"exclusiveMinimum": { exclusive_minimum },| ).
    endif.
    if max_value is not initial.
      write_tag( |"maximum": { max_value },| ).
    endif.
    if exclusive_maximum is not initial.
      write_tag( |"exclusiveMaximum": { exclusive_maximum },| ).
    endif.

    if multiple_of is not initial.
      write_tag( |"multipleOf": { multiple_of },| ).
    endif.
  endmethod.


  method handle_default.
    data(default) = abap_doc-default.
    if abap_doc-default cs '@link'.
      default = get_default_from_link( link = abap_doc-default fullname_of_type = fullname_of_type element_type = element_description->type_kind ).
      if default is initial.
        return.
      endif.
      default = |"{ default }"|.
    elseif is_default_value_valid( element_description = element_description default_value = default fullname_of_type = fullname_of_type ).
      if json_type =  zif_aff_writer=>type_info-numeric or json_type = zif_aff_writer=>type_info-boolean.
        replace all occurrences of `"` in default with ``.
      elseif json_type = zif_aff_writer=>type_info-date_time.
        default = `"` && date_time_from_abap_to_json( date_time_abap = default element_description = element_description ) && `"`.
      endif.
      if json_type = zif_aff_writer=>type_info-numeric.
        replace `E` in default with `e`.
      endif.
      if json_type = zif_aff_writer=>type_info-boolean.
        if default = 'X' or default = 'abap_true'.
          default = 'true' ##NO_TEXT.
        else.
          default = 'false' ##NO_TEXT.
        endif.
      endif.
    else.
      return.
    endif.

    write_tag( |"default": { default },| ).
  endmethod.


  method open_structure.
    clear_type_specifics( ).
*  add a new empty required_table to the stack
    if last_operation( ) = zif_aff_writer=>operation-initial.
      insert value #( name = structure_name absolute_name = structure_description->absolute_name ) into me->stack_of_structure index 1.
      add_required_table_to_stack(  ).
      open_json_schema_for_structure( structure_name = structure_name structure_description = structure_description ).
      insert value #( name = structure_name number_brackets = 2 ) into me->structure_buffer index 1.
      return.
    endif.

    append_comma_to_prev_line( ).

    data(mapped_and_formatted_name) = map_and_format_name( structure_name ).

    set_abapdoc_fullname_struc_tab( type_description = structure_description type_name  = structure_name ).

    data(callback_class) =  to_upper( abap_doc-callback_class ).
    if callback_class is not initial and is_callback_class_valid( class_name = callback_class component_name = fullname_of_type ).
      write_subschema( callback_class = callback_class ).
    endif.

    insert value #( name = structure_name absolute_name = structure_description->absolute_name ) into me->stack_of_structure index 1.

    if last_operation( ) <> zif_aff_writer=>operation-open_table.
      write_open_tag( |"{ mapped_and_formatted_name }": \{| ).
      insert value #( name = structure_name number_brackets = 2 ) into me->structure_buffer index 1.
    else.
      insert value #( name = structure_name number_brackets = 1 ) into me->structure_buffer index 1.
    endif.

    write_title_and_description( structure_description ).

    if abap_doc-required = abap_true.
      field-symbols <table1> type stringtab.
      assign stack_of_required_tabs[ 1 ] to <table1> .
      append mapped_and_formatted_name to <table1>.
    endif.
    write_tag( `"type": "object",` ).
    write_open_tag( `"properties": {` ).
    add_required_table_to_stack(  ).
  endmethod.


  method close_structure.
    delete_first_of_struc_stack( ).
    do me->structure_buffer[ 1 ]-number_brackets times.
      if me->structure_buffer[ 1 ]-number_brackets = 2 and sy-index = 2.
        write_req_and_add_props( ).
      endif.
      write_closing_tag( `}` ).
      if me->structure_buffer[ 1 ]-number_brackets = 1.
        write_req_and_add_props( ).
      endif.
    enddo.
    delete me->structure_buffer index 1.
    reset_indent_level_tag( ).
  endmethod.


  method open_table.
    clear_type_specifics( ).
    if last_operation( ) = zif_aff_writer=>operation-initial.
      open_json_schema_for_table( table_name = table_name  table_description = cast cl_abap_tabledescr( table_description ) ).
      insert value #( name = table_name number_brackets = 2 ) into table me->table_buffer.
      return.
    endif.
    append_comma_to_prev_line( ).
    data(mapped_and_formatted_name) = map_and_format_name( table_name ).

    set_abapdoc_fullname_struc_tab( type_description = table_description type_name = table_name ).

    if abap_doc-required = abap_true and lines( stack_of_required_tabs ) >= 1.
      field-symbols <table1> type stringtab.
      assign stack_of_required_tabs[ 1 ] to <table1> .
      append mapped_and_formatted_name to <table1>.
    endif.

    data(callback_class) = to_upper( abap_doc-callback_class ).
    if callback_class is not initial and is_callback_class_valid( class_name = callback_class component_name = fullname_of_type ).
      write_subschema( callback_class = callback_class ).
    endif.

    if last_operation( ) <> zif_aff_writer=>operation-open_table.
      write_open_tag( |"{ mapped_and_formatted_name }": \{| ).
      insert value #( name = table_name number_brackets = 2 ) into table me->table_buffer.
    else.
      insert value #( name = table_name number_brackets = 1 ) into table me->table_buffer.
    endif.

    write_title_and_description( table_description ).

    write_tag( `"type": "array",` ).
    if cast cl_abap_tabledescr( table_description )->has_unique_key = abap_true.
      write_tag( `"uniqueItems": true,`).
    endif.
    write_open_tag( `"items": {` ).
  endmethod.


  method close_table.
    do me->table_buffer[ name = table_name ]-number_brackets times.
      write_closing_tag( `}` ).
    enddo.
    delete me->table_buffer where name = table_name.
    reset_indent_level_tag( ).
  endmethod.


  method append_comma_to_prev_line.
    if ( last_operation( ) = zif_aff_writer=>operation-write_element or
       last_operation( ) = zif_aff_writer=>operation-close_structure or
       last_operation( ) = zif_aff_writer=>operation-close_table ) and ( ignore_til_indent_level > indent_level or ignore_til_indent_level is initial ).
      append_to_previous_line( `,` ).
    endif.
  endmethod.

  method set_abapdoc_fullname_struc_tab.
    if last_operation( ) = zif_aff_writer=>operation-open_table.
      data(absolute_name) = type_description->absolute_name.
      data(splitted_absolute_name) = get_splitted_absolute_name( absolute_name ).
      data(source_type) = splitted_absolute_name[ 1 ].
      data(source) = splitted_absolute_name[ 2 ].
      fullname_of_type =     type_name.
      data(already_found) = abap_true.
    else.
      get_all_path_information(
        exporting
          name             = type_name
        importing
          source_type      = source_type
          source           = source
          fullname_of_type = fullname_of_type
      ).
    endif.

    if source_type = 'CLASS' or source_type = 'INTERFACE'.
      abap_doc = call_reader_and_decode( name_of_source = source element_name = fullname_of_type ).
    endif.
    if already_found = abap_false.
      data(abap_doc_second) = get_abap_doc_for_absolute_name( type_description->absolute_name ).
      compare_abap_doc(
        exporting
          abap_doc_additional = abap_doc_second
        changing
          abap_doc_base       = abap_doc
      ).
    endif.
    check_redundant_annotations( ).
  endmethod.


  method set_abapdoc_fullname_element.
* Simple Component of a structure, defined in the structure definition
    if  lines( stack_of_structure ) > 0.
      get_all_path_information(
        exporting
          name             = element_name
        importing
          source_type      = data(source_type)
          source           = data(source)
          fullname_of_type = fullname_of_type
      ).

* Element which is in no structure
    elseif lines( stack_of_structure ) = 0.
      fullname_of_type = element_name.
      source_type =     splitted_prev_name[ 1 ].
      source =     splitted_prev_name[ 2 ].
      data(already_searched) = abap_true.
    endif.

    if source_type = 'CLASS' or source_type = 'INTERFACE'.
      abap_doc = call_reader_and_decode( name_of_source = source element_name   = fullname_of_type ).
    endif.

    if already_searched = abap_false.
      data(abap_doc_second) = get_abap_doc_for_absolute_name( absolute_name = element_description->absolute_name ).
      compare_abap_doc(
        exporting
          abap_doc_additional = abap_doc_second
        changing
          abap_doc_base       = abap_doc
      ).
    endif.
    check_redundant_annotations( ).
  endmethod.


  method get_json_schema_type.
    data(value_mapping) = get_value_mapping_for_element( element_name ).
    if value_mapping is not initial.
      data(type) = value_mapping-target_type.
    else.
      type = json_type.
    endif.
    if type = zif_aff_writer=>type_info-numeric.
      result = 'number' ##NO_TEXT.
      if type_is_integer( element_description ) = abap_true.
        result = 'integer'  ##NO_TEXT.
      endif.
    elseif type = zif_aff_writer=>type_info-date_time.
      result = 'string' ##NO_TEXT.
    else.
      result = to_lower( type ).
    endif.
  endmethod.


  method open_json_schema_for_structure.
    data(absolute_name) = stack_of_structure[ 1 ]-absolute_name.
    data(splitted_absolute_name) = get_splitted_absolute_name( absolute_name ).
    data(source_type) = splitted_absolute_name[ 1 ].
    if source_type = 'CLASS' or source_type = 'INTERFACE'.
      data(source) = splitted_absolute_name[ 2 ].
      abap_doc = call_reader_and_decode( name_of_source = source element_name = structure_name ).
    endif.
    fullname_of_type = structure_name.
    check_redundant_annotations( ).
    write_open_tag( '{' ).
    write_tag( |"$comment": "This file is autogenerated, do not edit manually, see { c_link_to_repository } for more information.",| ) ##NO_TEXT.
    write_tag( |"$schema": "{ c_schema_specification }",| ).
    write_tag( |"$id": "{ me->schema_id }",| ).
    data(callback_class) = to_upper( abap_doc-callback_class ).
    if callback_class is not initial and is_callback_class_valid( class_name = callback_class component_name = structure_name ).
      write_subschema( callback_class = callback_class ).
    endif.

    write_title_and_description( structure_description ).

    write_tag( '"type": "object",' ).
    write_open_tag( '"properties": {' ).
  endmethod.


  method open_json_schema_for_table.
    data(absolute_name) = table_description->absolute_name.
    data(splitted_absolute_name) = get_splitted_absolute_name( absolute_name ).
    data(source_type) = splitted_absolute_name[ 1 ].
    if source_type = 'CLASS' or source_type = 'INTERFACE'.
      data(source) = splitted_absolute_name[ 2 ].
      abap_doc = call_reader_and_decode( name_of_source = source element_name   = table_name ).
    endif.
    fullname_of_type = table_name.
    check_redundant_annotations( ).
    write_open_tag( '{' ).
    write_tag( |"$comment": "This file is autogenerated, do not edit manually, see { c_link_to_repository } for more information.",| ) ##NO_TEXT.
    write_tag( |"$schema": "{ c_schema_specification }",| ).
    write_tag( |"$id": "{ me->schema_id }",| ).

    data(callback_class) = to_upper( abap_doc-callback_class ).
    if callback_class is not initial and is_callback_class_valid( class_name = callback_class component_name = table_name ).
      write_subschema( callback_class = callback_class ).
    endif.

    write_title_and_description( table_description ).

    write_tag( '"type": "array",' ).
    if table_description->has_unique_key = abap_true.
      write_tag( '"uniqueItems": true,').
    endif.
    write_open_tag( '"items": {' ).
  endmethod.


  method write_subschema.
    try.
        data subschema type rswsourcet.
        call method (callback_class)=>get_subschema
          receiving
            subschema = subschema.
        loop at subschema assigning field-symbol(<line>).
          write_tag( <line> ).
        endloop.
        ignore_til_indent_level = indent_level.
      catch cx_sy_dyn_call_error ##NO_HANDLER.
    endtry.
  endmethod.


  method open_json_schema_for_element.
    write_open_tag( '{' ).
    write_tag( |"$comment": "This file is autogenerated, do not edit manually, see { c_link_to_repository } for more information.",| ) ##NO_TEXT.
    write_tag( |"$schema": "{ c_schema_specification }",| ).
    write_tag( |"$id": "{ me->schema_id }",| ).
  endmethod.


  method get_description.
    if abap_doc-description is not initial.
      result = abap_doc-description.
    elseif type_description is supplied.
      if type_description is instance of cl_abap_elemdescr.
        data element_description type ref to cl_abap_elemdescr.
        element_description = cast cl_abap_elemdescr( type_description ).
        element_description->get_ddic_field(
          exporting
            p_langu    = 'E'
          receiving
            p_flddescr = data(ddic_field)
          exceptions
            others     = 1 ) ##SUBRC_OK.
        if ddic_field is not initial and ddic_field-fieldtext is not initial.
          result = ddic_field-fieldtext.
        endif.
      endif.
    endif.
  endmethod.


  method get_enum_values.
    data(value_mapping) = get_value_mapping_for_element( element_name ).
    if value_mapping is not initial.
      loop at value_mapping-value_mappings assigning field-symbol(<mapping>).
        append <mapping>-json  to result.
      endloop.
    elseif abap_doc-enumvalues_link is not initial.
      set_enum_properties( element_description->type_kind ).
      result = enum_values.
    elseif element_description is instance of cl_abap_enumdescr.
      data(enum_description) = cast cl_abap_enumdescr( element_description ).
      loop at enum_description->members assigning field-symbol(<member>).
        data(formatted_name) = apply_formatting( conv #( <member>-name ) ).
        append formatted_name to result.
      endloop.
    else.
      if get_json_type_from_description( element_description ) = zif_aff_writer=>type_info-boolean.
        return.
      endif.
      element_description->get_ddic_fixed_values(
        receiving
          p_fixed_values = data(ddic_fixed_values)
        exceptions
          others         = 1 ) ##SUBRC_OK.
      if ddic_fixed_values is initial.
        return.
      endif.
      loop at ddic_fixed_values assigning field-symbol(<value>).
        data text type string.
        text = <value>-ddtext.
        replace all occurrences of pcre '\s' in text with '_'.
        append apply_formatting( text ) to result.
      endloop.
    endif.
  endmethod.


  method get_enum_descriptions.
    data(value_mapping) = get_value_mapping_for_element( element_name ).
    if value_mapping is not initial.
      loop at value_mapping-value_mappings assigning field-symbol(<mapping>).
        append <mapping>-json to result.
      endloop.
    elseif abap_doc-enumvalues_link is not initial.
      result = enum_descriptions.
    elseif element_description is instance of cl_abap_enumdescr.
      data(enum_description) = cast cl_abap_enumdescr( element_description ).
      loop at enum_description->members assigning field-symbol(<member>).
        data(description) = map_and_format_name( conv #( <member>-name ) ).
        append description to result.
      endloop.
    else.
      element_description->get_ddic_fixed_values(
        receiving
          p_fixed_values = data(ddic_fixed_values)
        exceptions
          others         = 1 ) ##SUBRC_OK.
      if ddic_fixed_values is not initial.
        loop at ddic_fixed_values assigning field-symbol(<value>).
          append <value>-ddtext to result.
        endloop.
      endif.
    endif.
  endmethod.


  method type_is_integer.
    result = abap_false.
    if element_description->type_kind = cl_abap_typedescr=>typekind_int or
       element_description->type_kind = cl_abap_typedescr=>typekind_int1 or
       element_description->type_kind = cl_abap_typedescr=>typekind_int2 or
       element_description->type_kind = cl_abap_typedescr=>typekind_int8.
      result = abap_true.
    endif.
  endmethod.


  method set_enum_properties.
    get_structure_of_enum_values(
      exporting
        link_to_values      = abap_doc-enumvalues_link
        fullname_of_type    = fullname_of_type
      importing
        structure_of_values = data(structure_of_values)
        name_of_source      = data(name_of_source)
        name_of_constant    = data(name_of_constant)
    ).

    if structure_of_values is not initial.
      data(has_initial_component) = abap_false.
      field-symbols:
        <attr>    type data,
        <fs_data> type any.
      assign (name_of_source)=>(name_of_constant) to <attr>.
      loop at structure_of_values->components assigning field-symbol(<component>).
        if <component>-type_kind <> enum_type.
          raise exception type cx_aff_root message e122(saff_core) with name_of_constant fullname_of_type.
        endif.

        assign component <component>-name of structure <attr> to <fs_data>.
        if <fs_data> is initial.
          has_initial_component = abap_true.
        endif.
        data(fullname_of_value) = name_of_constant && '-' && <component>-name.
        data(abap_doc_of_component) = call_reader_and_decode( name_of_source = name_of_source element_name   = fullname_of_value ).
        data(enum_value) = apply_formatting( conv #( <component>-name ) ).

        append enum_value to enum_values.
        append abap_doc_of_component-description to enum_descriptions.
        append abap_doc_of_component-title to enum_titles.

        check_title_and_description( abap_doc_to_check = abap_doc_of_component fullname_of_checked_type = fullname_of_value ).
      endloop.
      if has_initial_component = abap_false and abap_doc-required = abap_false and abap_doc-default is initial.
        message w127(saff_core) with fullname_of_type into data(message) ##NEEDED ##NO_TEXT.
        log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
      endif.
    endif.
  endmethod.


  method add_required_table_to_stack.
    insert value #( ) into stack_of_required_tabs index 1.
  endmethod.


  method delete_first_tab_of_req_stack.
    if stack_of_required_tabs is not initial.
      delete stack_of_required_tabs index 1.
    endif.
  endmethod.


  method write_req_and_add_props.
    if ignore_til_indent_level > indent_level or ignore_til_indent_level is initial.
      content[ lines( content ) ] = content[ lines( content ) ] && `,`.
      write_tag( `"additionalProperties": false` ).
      if stack_of_required_tabs[ 1 ] is not initial.
        content[ lines( content ) ] = content[ lines( content ) ] && `,`.
        write_tag( `"required": [` ).
        indent_level = indent_level + 1.
        loop at stack_of_required_tabs[ 1 ] assigning field-symbol(<required_comp>).
          if sy-tabix < lines( stack_of_required_tabs[ 1 ] ).
            write_tag( |"{ <required_comp> }",| ).
          else.
            write_tag( |"{ <required_comp> }"| ).
          endif.
        endloop.
        indent_level = indent_level - 1.
        write_tag( `]` ).
      endif.
    endif.
    delete_first_tab_of_req_stack( ).
  endmethod.


  method get_format.
    if element_description->type_kind = cl_abap_typedescr=>typekind_date or
    element_description->type_kind = cl_abap_typedescr=>typekind_time or
    element_description->type_kind = cl_abap_typedescr=>typekind_utclong.
      result = `date-time` ##NO_TEXT.
    endif.
  endmethod.


  method write_tag.
    if ignore_til_indent_level is initial or ignore_til_indent_level > indent_level.
      append |{ repeat( val = ` `  occ = indent_level * c_indent_number_characters ) }{ line }| to content.
    endif.
  endmethod.


  method date_time_from_abap_to_json.
    data(abap_date) = date_time_abap.
    replace all occurrences of `"` in abap_date with ``.
    if element_description->type_kind = cl_abap_typedescr=>typekind_date.
      if strlen( abap_date ) = 8.
        date_time_json = abap_date+0(4) && `-` && abap_date+4(2) && `-` && abap_date+6(2).
      elseif strlen( abap_date ) = 6.
        date_time_json = abap_date+0(4) && `-` && abap_date+4(2).
      else.
        date_time_json = abap_date.
      endif.
    elseif element_description->type_kind = cl_abap_typedescr=>typekind_utclong.
      date_time_json = abap_date+0(19) && `+00:00`.
    elseif element_description->type_kind = cl_abap_typedescr=>typekind_time.
      data(difference) = 6 - strlen( abap_date ).
      if difference > 0.
        abap_date = abap_date && repeat( val = '0' occ = difference ).
      endif.
      date_time_json = abap_date+0(2) && `:` && abap_date+2(2) && `:` && abap_date+4(2).
    endif.
  endmethod.


  method reset_indent_level_tag.
    if ignore_til_indent_level = indent_level.
      clear ignore_til_indent_level.
    endif.
  endmethod.


  method append_after_output.
    append `` to output.
  endmethod.


  method check_title_and_description.
    if ignore_til_indent_level is initial or ignore_til_indent_level > indent_level. "Only write message if no callback class provided
      if abap_doc_to_check-title is initial.
        message i119(saff_core) with 'Title' fullname_of_checked_type into data(message) ##NEEDED ##NO_TEXT.
        log->add_info( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
      endif.

      if abap_doc_to_check-description is initial.
        message i119(saff_core) with 'Description' fullname_of_checked_type into message ##NEEDED ##NO_TEXT.
        log->add_info( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
      elseif strlen( abap_doc_to_check-description ) > c_max_length_of_description.
        message w125(saff_core) with fullname_of_checked_type c_max_length_of_description into message ##NEEDED ##NO_TEXT.
        log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
      endif.
    endif.
  endmethod.


  method zif_aff_writer~validate.
    try.
        data(json_as_xstring) = cl_aff_content_handler_factory=>get_handler_for_plain_text( )->serialize( source ).
        data(json_reader) = cl_sxml_string_reader=>create( json_as_xstring ).
        json_reader->next_node( ).
        json_reader->skip_node( ).
      catch cx_aff_root cx_sxml_parse_error into data(exception).
        log->add_exception( exception = exception object = value #( ) ).
        return.
    endtry.
    result = abap_true.
  endmethod.


  method handle_language_field.
    write_tag( `"minLength": 2,` ).
    write_tag( `"maxLength": 2,` ).
    write_tag( `"pattern": "^[a-z]+$",` ).
  endmethod.

endclass.

"! Writer for a JSON schema. This is just a utility class helping to create a JSON schema.
"! The generated schema must be reviewed and adapted!
CLASS zcl_aff_writer_json_schema DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM zcl_aff_writer.

  PUBLIC SECTION.

    CONSTANTS:
      c_schema_specification TYPE string VALUE 'https://json-schema.org/draft/2020-12/schema' ##NO_TEXT,
      c_link_to_repository   TYPE string VALUE 'https://github.com/SAP/abap-file-formats' ##NO_TEXT.

    METHODS:
      "! Creates a JSON schema writer<br><br>
      "! Example for schema_id: http://sap.com/schema/nrob.json
      "!
      "! @parameter schema_id | The schema that should be written in the $id field of the schema
      "! @parameter format_version | The version of the ABAP file format as integer
      constructor
        IMPORTING schema_id      TYPE string
                  format_version TYPE i DEFAULT 1,

      zif_aff_writer~validate REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      write_element REDEFINITION,
      open_structure REDEFINITION,
      close_structure REDEFINITION,
      open_table REDEFINITION,
      write_tag REDEFINITION,
      close_table REDEFINITION,
      append_after_output REDEFINITION.


  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_buffer,
        name            TYPE string,
        number_brackets TYPE i,
      END OF ty_buffer,
      tt_buffer TYPE STANDARD TABLE OF ty_buffer.

    CONSTANTS:
      c_format_version            TYPE string VALUE 'FORMAT_VERSION',
      c_max_length_of_description TYPE i VALUE 253.

    DATA:
      schema_id              TYPE string,
      structure_buffer       TYPE tt_buffer,
      table_buffer           TYPE tt_buffer,
      ignore_next_elements   TYPE abap_boolean,
      enum_values            TYPE rswsourcet,
      enum_titles            TYPE rswsourcet,
      enum_descriptions      TYPE rswsourcet,
      stack_of_required_tabs TYPE STANDARD TABLE OF stringtab,
      format_version         TYPE i.

    METHODS: append_comma_to_prev_line,

      get_json_schema_type
        IMPORTING element_name        TYPE string
                  element_description TYPE REF TO cl_abap_elemdescr
                  json_type           TYPE zif_aff_writer=>enum_type_info
        RETURNING VALUE(result)       TYPE string
        RAISING   cx_aff_root,

      open_json_schema_for_structure
        IMPORTING structure_name        TYPE string
                  structure_description TYPE REF TO cl_abap_typedescr
        RAISING   cx_aff_root,

      open_json_schema_for_table
        IMPORTING table_name        TYPE string
                  table_description TYPE REF TO cl_abap_tabledescr
        RAISING   cx_aff_root,

      open_json_schema_for_element,

      get_description
        IMPORTING abap_doc         TYPE cl_aff_abap_doc_parser=>abap_doc
                  type_description TYPE REF TO cl_abap_typedescr OPTIONAL
        RETURNING VALUE(result)    TYPE string,

      get_enum_values
        IMPORTING element_name        TYPE string
                  element_description TYPE REF TO cl_abap_elemdescr
                  values_link         TYPE string
                  fullname_of_type    TYPE string
        RETURNING VALUE(result)       TYPE rswsourcet
        RAISING
                  cx_aff_root,

      get_enum_descriptions
        IMPORTING element_name        TYPE string
                  element_description TYPE REF TO cl_abap_elemdescr
                  values_link         TYPE string
        RETURNING VALUE(result)       TYPE rswsourcet,

      type_is_integer
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
        RETURNING
          VALUE(result)       TYPE abap_bool,

      set_enum_properties
        IMPORTING
          values_link      TYPE string
          fullname_of_type TYPE string
          enum_type        TYPE abap_typekind
        RAISING
          cx_aff_root,

      add_required_table_to_stack,

      delete_first_tab_of_req_stack,

      write_req_and_add_props,

      get_format
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
        RETURNING
          VALUE(result)       TYPE string,

      date_time_from_abap_to_json
        IMPORTING
          date_time_abap        TYPE string
          element_description   TYPE REF TO cl_abap_elemdescr
        RETURNING
          VALUE(date_time_json) TYPE string,

      handle_default
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
          json_type           TYPE zif_aff_writer=>enum_type_info
          abap_doc            TYPE cl_aff_abap_doc_parser=>abap_doc
          fullname_of_type    TYPE string
        RAISING
          cx_aff_root,

      handle_extrema
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
          element_name        TYPE string
          abap_doc            TYPE cl_aff_abap_doc_parser=>abap_doc,

      handle_string
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
          abap_doc            TYPE cl_aff_abap_doc_parser=>abap_doc,

      handle_language_field,

      handle_enums
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
          element_name        TYPE string
          abap_doc            TYPE cl_aff_abap_doc_parser=>abap_doc
          enums               TYPE rswsourcet,

      write_subschema
        IMPORTING
          callback_class TYPE string,

      reset_indent_level_tag,

      write_enum_properties
        IMPORTING
          property_table TYPE rswsourcet,

      check_title_and_description
        IMPORTING
          abap_doc         TYPE cl_aff_abap_doc_parser=>abap_doc
          fullname_of_type TYPE string,

      write_title_and_description
        IMPORTING
          type_description TYPE REF TO cl_abap_typedescr
          fullname_of_type TYPE string
          abap_doc         TYPE cl_aff_abap_doc_parser=>abap_doc
          check_not_needed TYPE abap_boolean DEFAULT abap_false.

ENDCLASS.



CLASS zcl_aff_writer_json_schema IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    me->formatting_option = zif_aff_writer=>formatting_option-camel_case.
    me->schema_id = schema_id.
    me->format_version = format_version.
    zif_aff_writer~set_name_mappings( VALUE #( ( abap = 'schema' json = '$schema' ) ) ) ##NO_TEXT.
  ENDMETHOD.


  METHOD write_element.
    IF ignore_next_elements = abap_true.
      RETURN.
    ENDIF.

    CLEAR enum_titles.
    CLEAR enum_descriptions.
    CLEAR enum_values.

    append_comma_to_prev_line( ).
    DATA(json_type) = get_json_type_from_description( element_description ).
    DATA(mapped_and_formatted_name) = map_and_format_name( element_name ).

    DATA(splitted_prev_name) = get_splitted_absolute_name( element_description->absolute_name ).
* Simple Component of a structure, defined in the structure definition
    IF lines( stack_of_structure ) > 0.
      get_all_path_information(
        EXPORTING
          name             = element_name
        IMPORTING
          source_type      = DATA(source_type)
          source           = DATA(source)
          fullname_of_type = DATA(fullname_of_type)   ).

* Element which is in no structure
    ELSEIF lines( stack_of_structure ) = 0.
      fullname_of_type = element_name.
      source_type = splitted_prev_name[ 1 ].
      source = splitted_prev_name[ 2 ].
      DATA(already_searched) = abap_true.
    ENDIF.

    IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
      DATA(abap_doc) = call_reader_and_decode( name_of_source = source
                                               element_name   = fullname_of_type ).
    ENDIF.

    IF already_searched = abap_false.
      DATA(abap_doc_second) = get_abap_doc_for_absolute_name( element_description->absolute_name ).
      compare_abap_doc(
        EXPORTING
          abap_doc_additional = abap_doc_second
        CHANGING
          abap_doc_base       = abap_doc   ).
    ENDIF.


    IF abap_doc-required = abap_true AND lines( stack_of_required_tabs ) >= 1.

      ASSIGN stack_of_required_tabs[ 1 ] TO <table1>.
      APPEND mapped_and_formatted_name TO <table1>.
    ENDIF.

    DATA(callback_class) = to_upper( abap_doc-callback_class ).
    IF callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = callback_class
                                                                  component_name = fullname_of_type ).
      IF last_operation( ) = zif_aff_writer=>operation-initial.
        open_json_schema_for_element( ).
      ENDIF.
      write_subschema( callback_class = callback_class ).
      IF last_operation( ) = zif_aff_writer=>operation-initial.
        write_closing_tag( `}` ).
      ENDIF.
      CLEAR ignore_til_indent_level.
      RETURN.
    ENDIF.


    IF last_operation( ) = zif_aff_writer=>operation-initial.
      open_json_schema_for_element( ).
    ELSEIF last_operation( ) <> zif_aff_writer=>operation-open_table.
      write_open_tag( |"{ mapped_and_formatted_name }": \{| ).
    ENDIF.

    DATA(enums) = get_enum_values( element_name = element_name
                                   element_description = element_description
                                   values_link = abap_doc-enumvalues_link
                                   fullname_of_type = fullname_of_type ).
    IF enums IS NOT INITIAL.
      json_type = zif_aff_writer=>type_info-string.
    ENDIF.

    DATA(check_not_needed) = abap_false.

    IF last_operation( ) = zif_aff_writer=>operation-open_table AND lines( splitted_prev_name ) = 2 AND splitted_prev_name[ 2 ] = element_name.
      check_not_needed = abap_true.
    ENDIF.

    write_title_and_description( type_description = element_description
                                 fullname_of_type = fullname_of_type
                                 abap_doc = abap_doc
                                 check_not_needed = check_not_needed ).

    IF element_name = c_format_version.
      write_tag( `"type": "string",`).
      write_tag( |"const": "{ format_version }",| ).
    ELSE.
      write_tag( |"type": "{ get_json_schema_type( element_name = element_name
                                                   element_description = element_description
                                                   json_type = json_type ) }",| ).
      DATA(format) = get_format( element_description ).
      IF format IS NOT INITIAL.
        write_tag( |"format": "{ format }",| ).
      ENDIF.

      IF enums IS NOT INITIAL.
        handle_enums( element_description = element_description
                      element_name = element_name
                      abap_doc = abap_doc
                      enums = enums ).
      ELSE. "non- enum
        IF json_type = zif_aff_writer=>type_info-numeric.
          handle_extrema( element_description = element_description
                          element_name = element_name
                          abap_doc = abap_doc ).
        ELSEIF json_type = zif_aff_writer=>type_info-string AND NOT ( element_description->type_kind = cl_abap_typedescr=>typekind_date OR element_description->type_kind = cl_abap_typedescr=>typekind_time OR
             element_description->type_kind = cl_abap_typedescr=>typekind_utclong ).
          IF is_sy_langu( element_description ).
            handle_language_field( ).
          ELSE.
            handle_string( element_description = element_description
                           abap_doc = abap_doc ).
          ENDIF.
        ENDIF.
      ENDIF.

      IF abap_doc-default IS NOT INITIAL.
        handle_default( element_description = element_description
                        json_type = json_type
                        abap_doc = abap_doc
                        fullname_of_type = fullname_of_type ).
      ENDIF.
    ENDIF.

*    remove "," in last line
    IF ignore_til_indent_level > indent_level OR ignore_til_indent_level IS INITIAL.
      DATA(last_line) = content[ lines( content ) ].
      content[ lines( content ) ] = substring( val = last_line
                                               off = 0
                                               len = strlen( last_line ) - 1 ).
    ENDIF.

    IF last_operation( ) <> zif_aff_writer=>operation-open_table.
      write_closing_tag( `}` ).
    ENDIF.
  ENDMETHOD.


  METHOD write_title_and_description.
    IF check_not_needed = abap_false.
      check_title_and_description( abap_doc = abap_doc
                                   fullname_of_type = fullname_of_type ).
    ENDIF.
    DATA(title) = escape( val = abap_doc-title
                          format = cl_abap_format=>e_json_string ).
    DATA(description) = escape( val = get_description( type_description = type_description abap_doc = abap_doc )
                                format = cl_abap_format=>e_json_string ).
    IF title IS NOT INITIAL.
      write_tag( |"title": "{ title }",| ).
    ENDIF.
    IF description IS NOT INITIAL.
      write_tag( |"description": "{ description }",| ).
    ENDIF.
  ENDMETHOD.


  METHOD handle_enums.
    write_tag( `"enum": [`).
    write_enum_properties( enums ).

    IF enum_titles IS NOT INITIAL.
      write_tag( `"enumTitles": [`).
      write_enum_properties( enum_titles ).
    ENDIF.

    DATA(enum_descr) = get_enum_descriptions( element_name = element_name
                                              element_description = element_description
                                              values_link = abap_doc-enumvalues_link ).
    write_tag( `"enumDescriptions": [`).
    write_enum_properties( enum_descr ).
  ENDMETHOD.


  METHOD write_enum_properties.
    indent_level = indent_level + 1.
    LOOP AT property_table ASSIGNING FIELD-SYMBOL(<value>).
      IF sy-tabix < lines( property_table ).
        write_tag( |"{ <value> }",| ).
      ELSE.
        write_tag( |"{ <value> }"| ).
      ENDIF.
    ENDLOOP.
    indent_level = indent_level - 1.
    write_tag( `],` ).
  ENDMETHOD.


  METHOD handle_string.
    IF abap_doc-max_length IS NOT INITIAL.
      DATA(max_length) = abap_doc-max_length.
    ELSE.
      max_length = NEW zcl_aff_extreme_values( )->get_max_length( element_description ).
    ENDIF.
    IF abap_doc-min_length IS NOT INITIAL.
      write_tag( |"minLength": { abap_doc-min_length },| ).
    ENDIF.
    IF max_length IS NOT INITIAL.
      write_tag( |"maxLength": { max_length },| ).
      IF element_description->type_kind = cl_abap_typedescr=>typekind_num.
        write_tag( `"pattern": "^[0-9]+$",` ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD handle_extrema.
    IF get_value_mapping_for_element( element_name ) IS INITIAL.
      NEW zcl_aff_extreme_values( )->get_extrema(
        EXPORTING
          element_description = element_description
        IMPORTING
          max                 = DATA(max_value)
          min                 = DATA(min_value)   ).
    ENDIF.
    DATA(multiple_of) = abap_doc-multiple_of.

    IF multiple_of IS INITIAL AND element_description->type_kind = cl_abap_typedescr=>typekind_packed.
      DATA(decimals) = element_description->decimals.
      IF decimals > 0.
        multiple_of = |0.{ repeat( val = `0`
                                   occ = decimals - 1 ) }1|.
      ENDIF.
    ENDIF.

    DATA(exclusive_minimum) = abap_doc-exclusive_minimum.
    DATA(exclusive_maximum) = abap_doc-exclusive_maximum.

    IF exclusive_minimum IS NOT INITIAL.
      CLEAR min_value.
    ELSEIF abap_doc-minimum IS NOT INITIAL.
      min_value = abap_doc-minimum.
    ENDIF.

    IF exclusive_maximum IS NOT INITIAL.
      CLEAR max_value.
    ELSEIF abap_doc-maximum IS NOT INITIAL.
      max_value = abap_doc-maximum.
    ENDIF.

    IF min_value IS NOT INITIAL.
      write_tag( |"minimum": { min_value },| ).
    ENDIF.
    IF exclusive_minimum IS NOT INITIAL.
      write_tag( |"exclusiveMinimum": { exclusive_minimum },| ).
    ENDIF.
    IF max_value IS NOT INITIAL.
      write_tag( |"maximum": { max_value },| ).
    ENDIF.
    IF exclusive_maximum IS NOT INITIAL.
      write_tag( |"exclusiveMaximum": { exclusive_maximum },| ).
    ENDIF.

    IF multiple_of IS NOT INITIAL.
      write_tag( |"multipleOf": { multiple_of },| ).
    ENDIF.
  ENDMETHOD.


  METHOD handle_default.
    DATA(default) = abap_doc-default.
    IF abap_doc-default CS '@link'.
      default = get_default_from_link( link = abap_doc-default
                                       fullname_of_type = fullname_of_type
                                       element_type = element_description->type_kind ).
      IF default IS INITIAL.
        RETURN.
      ENDIF.
      default = |"{ default }"|.
    ELSEIF is_default_value_valid( element_description = element_description
                                   default_value = default
                                   fullname_of_type = fullname_of_type ).
      IF json_type = zif_aff_writer=>type_info-numeric OR json_type = zif_aff_writer=>type_info-boolean.
        REPLACE ALL OCCURRENCES OF `"` IN default WITH ``.
      ELSEIF json_type = zif_aff_writer=>type_info-date_time.
        default = `"` && date_time_from_abap_to_json( date_time_abap = default
                                                      element_description = element_description ) && `"`.
      ENDIF.
      IF json_type = zif_aff_writer=>type_info-numeric.
        REPLACE `E` IN default WITH `e`.
      ENDIF.
      IF json_type = zif_aff_writer=>type_info-boolean.
        IF default = 'X' OR default = 'abap_true'.
          default = 'true' ##NO_TEXT.
        ELSE.
          default = 'false' ##NO_TEXT.
        ENDIF.
      ENDIF.
    ELSE.
      RETURN.
    ENDIF.

    write_tag( |"default": { default },| ).
  ENDMETHOD.


  METHOD open_structure.
      FIELD-SYMBOLS <table1> TYPE stringtab.
*  add a new empty required_table to the stack
    IF last_operation( ) = zif_aff_writer=>operation-initial.
      INSERT VALUE #( name = structure_name absolute_name = structure_description->absolute_name ) INTO me->stack_of_structure INDEX 1.
      add_required_table_to_stack( ).
      open_json_schema_for_structure( structure_name = structure_name
                                      structure_description = structure_description ).
      INSERT VALUE #( name = structure_name number_brackets = 2 ) INTO me->structure_buffer INDEX 1.
      RETURN.
    ENDIF.

    append_comma_to_prev_line( ).

    DATA(mapped_and_formatted_name) = map_and_format_name( structure_name ).

    IF last_operation( ) = zif_aff_writer=>operation-open_table.
      DATA(absolute_name) = structure_description->absolute_name.
      DATA(splitted_absolute_name) = get_splitted_absolute_name( absolute_name ).
      DATA(source_type) = splitted_absolute_name[ 1 ].
      DATA(source) = splitted_absolute_name[ 2 ].
      DATA(fullname_of_type) = structure_name.
      DATA(already_found) = abap_true.
    ELSE.
      get_all_path_information(
        EXPORTING
          name             = structure_name
        IMPORTING
          source_type      = source_type
          source           = source
          fullname_of_type = fullname_of_type   ).
    ENDIF.

    IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
      DATA(abap_doc) = call_reader_and_decode( name_of_source = source
                                               element_name   = fullname_of_type ).
    ENDIF.
    IF ( ( abap_doc-title IS INITIAL AND abap_doc-description IS INITIAL )
        OR ( abap_doc-callback_class IS INITIAL ) ) AND already_found = abap_false.
      DATA(abap_doc_second) = get_abap_doc_for_absolute_name( structure_description->absolute_name ).
      compare_abap_doc(
        EXPORTING
          abap_doc_additional = abap_doc_second
        CHANGING
          abap_doc_base       = abap_doc   ).
    ENDIF.

    DATA(callback_class) = to_upper( abap_doc-callback_class ).
    IF callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = callback_class
                                                                  component_name = fullname_of_type ).
      write_subschema( callback_class = callback_class ).
    ENDIF.

    INSERT VALUE #( name = structure_name absolute_name = structure_description->absolute_name ) INTO me->stack_of_structure INDEX 1.

    IF last_operation( ) <> zif_aff_writer=>operation-open_table.
      write_open_tag( |"{ mapped_and_formatted_name }": \{| ).
      INSERT VALUE #( name = structure_name number_brackets = 2 ) INTO me->structure_buffer INDEX 1.
    ELSE.
      INSERT VALUE #( name = structure_name number_brackets = 1 ) INTO me->structure_buffer INDEX 1.
    ENDIF.

    write_title_and_description( type_description = structure_description
                                 fullname_of_type = fullname_of_type
                                 abap_doc = abap_doc ).

    IF abap_doc-required = abap_true.

      ASSIGN stack_of_required_tabs[ 1 ] TO <table1>.
      APPEND mapped_and_formatted_name TO <table1>.
    ENDIF.
    write_tag( `"type": "object",` ).
    write_open_tag( `"properties": {` ).
    add_required_table_to_stack( ).
  ENDMETHOD.


  METHOD close_structure.
    delete_first_of_struc_stack( ).
    DO me->structure_buffer[ 1 ]-number_brackets TIMES.
      IF me->structure_buffer[ 1 ]-number_brackets = 2 AND sy-index = 2.
        write_req_and_add_props( ).
      ENDIF.
      write_closing_tag( `}` ).
      IF me->structure_buffer[ 1 ]-number_brackets = 1.
        write_req_and_add_props( ).
      ENDIF.
    ENDDO.
    DELETE me->structure_buffer INDEX 1.
    reset_indent_level_tag( ).
  ENDMETHOD.


  METHOD open_table.
      FIELD-SYMBOLS <table1> TYPE stringtab.
    IF last_operation( ) = zif_aff_writer=>operation-initial.
      open_json_schema_for_table( table_name = table_name
                                  table_description = CAST cl_abap_tabledescr( table_description ) ).
      INSERT VALUE #( name = table_name number_brackets = 2 ) INTO TABLE me->table_buffer.
      RETURN.
    ENDIF.
    append_comma_to_prev_line( ).
    DATA(mapped_and_formatted_name) = map_and_format_name( table_name ).

    IF last_operation( ) <> zif_aff_writer=>operation-open_table.
      get_all_path_information(
        EXPORTING
          name             = table_name
        IMPORTING
          source_type      = DATA(source_type)
          source           = DATA(source)
          fullname_of_type = DATA(fullname_of_type)   ).
    ELSE.
      DATA(absolute_name) = table_description->absolute_name.
      DATA(splitted_absolute_name) = get_splitted_absolute_name( absolute_name ).
      source_type = splitted_absolute_name[ 1 ].
      source = splitted_absolute_name[ 2 ].
      fullname_of_type = table_name.
      DATA(already_searched) = abap_true.
    ENDIF.

    IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
      DATA(abap_doc) = call_reader_and_decode( name_of_source = source
                                                element_name   = fullname_of_type ).
    ENDIF.

    IF ( ( abap_doc-title IS INITIAL AND abap_doc-description IS INITIAL )
        OR abap_doc-callback_class IS INITIAL ) AND already_searched = abap_false.
      DATA(abap_doc_second) = get_abap_doc_for_absolute_name( absolute_name = table_description->absolute_name ).
      compare_abap_doc(
        EXPORTING
          abap_doc_additional = abap_doc_second
        CHANGING
          abap_doc_base       = abap_doc   ).
    ENDIF.

    IF abap_doc-required = abap_true AND lines( stack_of_required_tabs ) >= 1.

      ASSIGN stack_of_required_tabs[ 1 ] TO <table1>.
      APPEND mapped_and_formatted_name TO <table1>.
    ENDIF.

    DATA(callback_class) = to_upper( abap_doc-callback_class ).
    IF callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = callback_class
                                                                  component_name = fullname_of_type ).
      write_subschema( callback_class = callback_class ).
    ENDIF.

    IF last_operation( ) <> zif_aff_writer=>operation-open_table.
      write_open_tag( |"{ mapped_and_formatted_name }": \{| ).
      INSERT VALUE #( name = table_name number_brackets = 2 ) INTO TABLE me->table_buffer.
    ELSE.
      INSERT VALUE #( name = table_name number_brackets = 1 ) INTO TABLE me->table_buffer.
    ENDIF.

    write_title_and_description( type_description = table_description
                                 fullname_of_type = fullname_of_type
                                 abap_doc = abap_doc ).

    write_tag( `"type": "array",` ).
    IF CAST cl_abap_tabledescr( table_description )->has_unique_key = abap_true.
      write_tag( `"uniqueItems": true,`).
    ENDIF.
    write_open_tag( `"items": {` ).
  ENDMETHOD.


  METHOD close_table.
    DO me->table_buffer[ name = table_name ]-number_brackets TIMES.
      write_closing_tag( `}` ).
    ENDDO.
    DELETE me->table_buffer WHERE name = table_name.
    reset_indent_level_tag( ).
  ENDMETHOD.


  METHOD append_comma_to_prev_line.
    IF ( last_operation( ) = zif_aff_writer=>operation-write_element OR
        last_operation( ) = zif_aff_writer=>operation-close_structure OR
       last_operation( ) = zif_aff_writer=>operation-close_table ) AND ( ignore_til_indent_level > indent_level OR ignore_til_indent_level IS INITIAL ).
      append_to_previous_line( `,` ).
    ENDIF.
  ENDMETHOD.


  METHOD get_json_schema_type.
    DATA(value_mapping) = get_value_mapping_for_element( element_name ).
    IF value_mapping IS NOT INITIAL.
      DATA(type) = value_mapping-target_type.
    ELSE.
      type = json_type.
    ENDIF.
    IF type = zif_aff_writer=>type_info-numeric.
      result = 'number' ##NO_TEXT.
      IF type_is_integer( element_description ) = abap_true.
        result = 'integer'  ##NO_TEXT.
      ENDIF.
    ELSEIF type = zif_aff_writer=>type_info-date_time.
      result = 'string' ##NO_TEXT.
    ELSE.
      result = to_lower( type ).
    ENDIF.
  ENDMETHOD.


  METHOD open_json_schema_for_structure.
    DATA(absolute_name) = stack_of_structure[ 1 ]-absolute_name.
    DATA(splitted_absolute_name) = get_splitted_absolute_name( absolute_name ).
    DATA(source_type) = splitted_absolute_name[ 1 ].
    IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
      DATA(source) = splitted_absolute_name[ 2 ].
      DATA(abap_doc) = call_reader_and_decode( name_of_source = source
                                               element_name = structure_name ).
    ENDIF.
    write_open_tag( '{' ).
    write_tag( |"$comment": "This file is autogenerated, do not edit manually, see { c_link_to_repository } for more information.",| ) ##NO_TEXT.
    write_tag( |"$schema": "{ c_schema_specification }",| ).
    write_tag( |"$id": "{ schema_id }",| ).
    DATA(callback_class) = to_upper( abap_doc-callback_class ).
    IF callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = callback_class
                                                                  component_name = structure_name ).
      write_subschema( callback_class = callback_class ).
    ENDIF.

    write_title_and_description( type_description = structure_description
                                 fullname_of_type = structure_name
                                 abap_doc = abap_doc ).

    write_tag( '"type": "object",' ).
    write_open_tag( '"properties": {' ).
  ENDMETHOD.


  METHOD open_json_schema_for_table.
    DATA(absolute_name) = table_description->absolute_name.
    DATA(splitted_absolute_name) = get_splitted_absolute_name( absolute_name ).
    DATA(source_type) = splitted_absolute_name[ 1 ].
    IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
      DATA(source) = splitted_absolute_name[ 2 ].
      DATA(abap_doc) = call_reader_and_decode( name_of_source = source
                                               element_name   = table_name ).
    ENDIF.

    write_open_tag( '{' ).
    write_tag( |"$comment": "This file is autogenerated, do not edit manually, see { c_link_to_repository } for more information.",| ) ##NO_TEXT.
    write_tag( |"$schema": "{ c_schema_specification }",| ).
    write_tag( |"$id": "{ schema_id }",| ).

    DATA(callback_class) = to_upper( abap_doc-callback_class ).
    IF callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = callback_class
                                                                  component_name = table_name ).
      write_subschema( callback_class = callback_class ).
    ENDIF.

    write_title_and_description( type_description = table_description
                                 fullname_of_type = table_name
                                 abap_doc = abap_doc ).

    write_tag( '"type": "array",' ).
    IF table_description->has_unique_key = abap_true.
      write_tag( '"uniqueItems": true,').
    ENDIF.
    write_open_tag( '"items": {' ).
  ENDMETHOD.


  METHOD write_subschema.
    TRY.
        DATA subschema TYPE rswsourcet.
        CALL METHOD (callback_class)=>get_subschema
          RECEIVING
            subschema = subschema.
        LOOP AT subschema ASSIGNING FIELD-SYMBOL(<line>).
          write_tag( <line> ).
        ENDLOOP.
        ignore_til_indent_level = indent_level.
      CATCH cx_sy_dyn_call_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD open_json_schema_for_element.
    write_open_tag( '{' ).
    write_tag( |"$comment": "This file is autogenerated, do not edit manually, see { c_link_to_repository } for more information.",| ) ##NO_TEXT.
    write_tag( |"$schema": "{ c_schema_specification }",| ).
    write_tag( |"$id": "{ me->schema_id }",| ).
  ENDMETHOD.


  METHOD get_description.
    IF abap_doc-description IS NOT INITIAL.
      result = abap_doc-description.
    ELSEIF type_description IS SUPPLIED.
      IF type_description IS INSTANCE OF cl_abap_elemdescr.
        DATA element_description TYPE REF TO cl_abap_elemdescr.
        element_description = CAST cl_abap_elemdescr( type_description ).
        element_description->get_ddic_field(
          EXPORTING
            p_langu    = 'E'
          RECEIVING
            p_flddescr = DATA(ddic_field)
          EXCEPTIONS
            OTHERS     = 1 ) ##SUBRC_OK.
        IF ddic_field IS NOT INITIAL AND ddic_field-fieldtext IS NOT INITIAL.
          result = ddic_field-fieldtext.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_enum_values.
        DATA text TYPE string.
    DATA(value_mapping) = get_value_mapping_for_element( element_name ).
    IF value_mapping IS NOT INITIAL.
      LOOP AT value_mapping-value_mappings ASSIGNING FIELD-SYMBOL(<mapping>).
        APPEND <mapping>-json  TO result.
      ENDLOOP.
    ELSEIF values_link IS NOT INITIAL.
      set_enum_properties( values_link = values_link
                           fullname_of_type = fullname_of_type
                           enum_type = element_description->type_kind ).
      result = enum_values.
    ELSEIF element_description IS INSTANCE OF cl_abap_enumdescr.
      DATA(enum_description) = CAST cl_abap_enumdescr( element_description ).
      LOOP AT enum_description->members ASSIGNING FIELD-SYMBOL(<member>).
        DATA(formatted_name) = apply_formatting( CONV #( <member>-name ) ).
        APPEND formatted_name TO result.
      ENDLOOP.
    ELSE.
      IF get_json_type_from_description( element_description ) = zif_aff_writer=>type_info-boolean.
        RETURN.
      ENDIF.
      element_description->get_ddic_fixed_values(
        RECEIVING
          p_fixed_values = DATA(ddic_fixed_values)
        EXCEPTIONS
          OTHERS         = 1 ) ##SUBRC_OK.
      IF ddic_fixed_values IS INITIAL.
        RETURN.
      ENDIF.
      LOOP AT ddic_fixed_values ASSIGNING FIELD-SYMBOL(<value>).
        DATA text TYPE string.
        text = <value>-ddtext.
        REPLACE ALL OCCURRENCES OF PCRE '\s' IN text WITH '_'.
        APPEND apply_formatting( text ) TO result.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_enum_descriptions.
    DATA(value_mapping) = get_value_mapping_for_element( element_name ).
    IF value_mapping IS NOT INITIAL.
      LOOP AT value_mapping-value_mappings ASSIGNING FIELD-SYMBOL(<mapping>).
        APPEND <mapping>-json TO result.
      ENDLOOP.
    ELSEIF values_link IS NOT INITIAL.
      result = enum_descriptions.
    ELSEIF element_description IS INSTANCE OF cl_abap_enumdescr.
      DATA(enum_description) = CAST cl_abap_enumdescr( element_description ).
      LOOP AT enum_description->members ASSIGNING FIELD-SYMBOL(<member>).
        DATA(description) = map_and_format_name( CONV #( <member>-name ) ).
        APPEND description TO result.
      ENDLOOP.
    ELSE.
      element_description->get_ddic_fixed_values(
        RECEIVING
          p_fixed_values = DATA(ddic_fixed_values)
        EXCEPTIONS
          OTHERS         = 1 ) ##SUBRC_OK.
      IF ddic_fixed_values IS NOT INITIAL.
        LOOP AT ddic_fixed_values ASSIGNING FIELD-SYMBOL(<value>).
          APPEND <value>-ddtext TO result.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD type_is_integer.
    result = abap_false.
    IF element_description->type_kind = cl_abap_typedescr=>typekind_int OR
       element_description->type_kind = cl_abap_typedescr=>typekind_int1 OR
       element_description->type_kind = cl_abap_typedescr=>typekind_int2 OR
       element_description->type_kind = cl_abap_typedescr=>typekind_int8.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD set_enum_properties.
    get_structure_of_enum_values(
      EXPORTING
        link_to_values      = values_link
        fullname_of_type    = fullname_of_type
      IMPORTING
        structure_of_values = DATA(structure_of_values)
        name_of_source      = DATA(name_of_source)
        name_of_constant    = DATA(name_of_constant)   ).

    IF structure_of_values IS NOT INITIAL.
      LOOP AT structure_of_values->components ASSIGNING FIELD-SYMBOL(<component>).
        IF <component>-type_kind <> enum_type.
          RAISE EXCEPTION TYPE cx_aff_root MESSAGE e122(saff_core) WITH name_of_constant fullname_of_type.
        ENDIF.
        DATA(fullname_of_value) = name_of_constant && '-' && <component>-name.
        DATA(abap_doc) = call_reader_and_decode( name_of_source = name_of_source
                                                 element_name   = fullname_of_value ).
        DATA(enum_value) = apply_formatting( CONV #( <component>-name ) ).

        APPEND enum_value TO enum_values.
        APPEND abap_doc-description TO enum_descriptions.
        APPEND abap_doc-title TO enum_titles.

        check_title_and_description( abap_doc = abap_doc
                                     fullname_of_type = fullname_of_value ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD add_required_table_to_stack.
    INSERT VALUE #( ) INTO stack_of_required_tabs INDEX 1.
  ENDMETHOD.


  METHOD delete_first_tab_of_req_stack.
    IF stack_of_required_tabs IS NOT INITIAL.
      DELETE stack_of_required_tabs INDEX 1.
    ENDIF.
  ENDMETHOD.


  METHOD write_req_and_add_props.
    IF ignore_til_indent_level > indent_level OR ignore_til_indent_level IS INITIAL.
      content[ lines( content ) ] = content[ lines( content ) ] && `,`.
      write_tag( `"additionalProperties": false` ).
      IF stack_of_required_tabs[ 1 ] IS NOT INITIAL.
        content[ lines( content ) ] = content[ lines( content ) ] && `,`.
        write_tag( `"required": [` ).
        indent_level = indent_level + 1.
        LOOP AT stack_of_required_tabs[ 1 ] ASSIGNING FIELD-SYMBOL(<required_comp>).
          IF sy-tabix < lines( stack_of_required_tabs[ 1 ] ).
            write_tag( |"{ <required_comp> }",| ).
          ELSE.
            write_tag( |"{ <required_comp> }"| ).
          ENDIF.
        ENDLOOP.
        indent_level = indent_level - 1.
        write_tag( `]` ).
      ENDIF.
    ENDIF.
    delete_first_tab_of_req_stack( ).
  ENDMETHOD.


  METHOD get_format.
    IF element_description->type_kind = cl_abap_typedescr=>typekind_date OR
    element_description->type_kind = cl_abap_typedescr=>typekind_time OR
    element_description->type_kind = cl_abap_typedescr=>typekind_utclong.
      result = `date-time` ##NO_TEXT.
    ENDIF.
  ENDMETHOD.


  METHOD write_tag.
    IF ignore_til_indent_level IS INITIAL OR ignore_til_indent_level > indent_level.
      APPEND |{ repeat( val = ` `
                        occ = indent_level * c_indent_number_characters ) }{ line }| TO content.
    ENDIF.
  ENDMETHOD.


  METHOD date_time_from_abap_to_json.
    DATA(abap_date) = date_time_abap.
    REPLACE ALL OCCURRENCES OF `"` IN abap_date WITH ``.
    IF element_description->type_kind = cl_abap_typedescr=>typekind_date.
      IF strlen( abap_date ) = 8.
        date_time_json = abap_date+0(4) && `-` && abap_date+4(2) && `-` && abap_date+6(2).
      ELSEIF strlen( abap_date ) = 6.
        date_time_json = abap_date+0(4) && `-` && abap_date+4(2).
      ELSE.
        date_time_json = abap_date.
      ENDIF.
    ELSEIF element_description->type_kind = cl_abap_typedescr=>typekind_utclong.
      date_time_json = abap_date+0(19) && `+00:00`.
    ELSEIF element_description->type_kind = cl_abap_typedescr=>typekind_time.
      DATA(difference) = 6 - strlen( abap_date ).
      IF difference > 0.
        abap_date = abap_date && repeat( val = '0'
                                         occ = difference ).
      ENDIF.
      date_time_json = abap_date+0(2) && `:` && abap_date+2(2) && `:` && abap_date+4(2).
    ENDIF.
  ENDMETHOD.


  METHOD reset_indent_level_tag.
    IF ignore_til_indent_level = indent_level.
      CLEAR ignore_til_indent_level.
    ENDIF.
  ENDMETHOD.


  METHOD append_after_output.
    APPEND `` TO output.
  ENDMETHOD.


  METHOD check_title_and_description.
    IF ignore_til_indent_level IS INITIAL OR ignore_til_indent_level > indent_level. "Only write message if no callback class provided
      IF abap_doc-title IS INITIAL.
        MESSAGE i119(saff_core) WITH 'Title' fullname_of_type INTO DATA(message) ##NEEDED ##NO_TEXT.
        log->add_info( message = cl_aff_log=>get_sy_message( )
                       object = VALUE #( ) ).
      ENDIF.

      IF abap_doc-description IS INITIAL.
        MESSAGE i119(saff_core) WITH 'Description' fullname_of_type INTO message ##NEEDED ##NO_TEXT.
        log->add_info( message = cl_aff_log=>get_sy_message( )
                       object = VALUE #( ) ).
      ELSEIF strlen( abap_doc-description ) > c_max_length_of_description.
        MESSAGE w125(saff_core) WITH fullname_of_type c_max_length_of_description INTO message ##NEEDED ##NO_TEXT.
        log->add_warning( message = cl_aff_log=>get_sy_message( )
                          object = VALUE #( ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_aff_writer~validate.
    TRY.
        DATA(json_as_xstring) = cl_aff_content_handler_factory=>get_handler_for_plain_text( )->serialize( source ).
        DATA(json_reader) = cl_sxml_string_reader=>create( json_as_xstring ).
        json_reader->next_node( ).
        json_reader->skip_node( ).
      CATCH cx_aff_root cx_sxml_parse_error INTO DATA(exception).
        log->add_exception( exception = exception
                            object = VALUE #( ) ).
        RETURN.
    ENDTRY.
    result = abap_true.
  ENDMETHOD.


  METHOD handle_language_field.
    write_tag( `"minLength": 2,` ).
    write_tag( `"maxLength": 2,` ).
    write_tag( `"pattern": "^[a-z]+$",` ).
  ENDMETHOD.
ENDCLASS.

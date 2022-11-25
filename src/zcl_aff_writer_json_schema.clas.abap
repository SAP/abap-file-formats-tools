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

    TYPES:
      BEGIN OF ty_enum_value,
        value             TYPE string,
        overwritten_value TYPE string,
      END OF ty_enum_value.

    TYPES:
      BEGIN OF ty_enum_properties,
        values       TYPE STANDARD TABLE OF ty_enum_value WITH DEFAULT KEY,
        titles       TYPE string_table,
        descriptions TYPE string_table,
      END OF ty_enum_properties.


    CONSTANTS:
      c_format_version            TYPE string VALUE 'FORMAT_VERSION',
      c_max_length_of_description TYPE i VALUE 253.

    DATA:
      schema_id              TYPE string,
      structure_buffer       TYPE tt_buffer,
      table_buffer           TYPE tt_buffer,
      ignore_next_elements   TYPE abap_boolean,
      stack_of_required_tabs TYPE STANDARD TABLE OF string_table,
      format_version         TYPE i.

    METHODS: append_comma_to_prev_line,

      get_json_schema_type
        IMPORTING element_description TYPE REF TO cl_abap_elemdescr
                  json_type           TYPE string
        RETURNING VALUE(result)       TYPE string
        RAISING   zcx_aff_tools,

      open_json_schema_for_structure
        IMPORTING structure_name        TYPE string
                  structure_description TYPE REF TO cl_abap_typedescr
        RAISING   zcx_aff_tools,

      open_json_schema_for_table
        IMPORTING table_name        TYPE string
                  table_description TYPE REF TO cl_abap_tabledescr
        RAISING   zcx_aff_tools,

      open_json_schema_for_element,

      get_description
        IMPORTING type_description TYPE REF TO cl_abap_typedescr OPTIONAL
        RETURNING VALUE(result)    TYPE string,

      get_enum_properties
        IMPORTING element_description TYPE REF TO cl_abap_elemdescr
        RETURNING VALUE(result)       TYPE ty_enum_properties
        RAISING
                  zcx_aff_tools,

      get_enum_descriptions
        IMPORTING element_description TYPE REF TO cl_abap_elemdescr
                  enum_properties     TYPE ty_enum_properties
        RETURNING VALUE(result)       TYPE string_table,

      type_is_integer
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
        RETURNING
          VALUE(result)       TYPE abap_bool,

      get_properties_from_structure
        IMPORTING
                  enum_type     TYPE abap_typekind
        RETURNING VALUE(result) TYPE ty_enum_properties
        RAISING
                  zcx_aff_tools,

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
          json_type           TYPE string
          enum_properties     TYPE ty_enum_properties
        RAISING
          zcx_aff_tools,

      handle_extrema
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr,

      handle_string
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr,

      handle_language_field,

      handle_enums
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
          enum_properties     TYPE ty_enum_properties,

      write_subschema
        IMPORTING
          callback_class TYPE string,

      reset_indent_level_tag,

      write_enum_properties
        IMPORTING
          property_table TYPE string_table,

      check_title_and_description
        IMPORTING abap_doc_to_check        TYPE zcl_aff_abap_doc_parser=>abap_doc
                  fullname_of_checked_type TYPE string,


      write_title_and_description
        IMPORTING
          type_description TYPE REF TO cl_abap_typedescr
          check_not_needed TYPE abap_boolean DEFAULT abap_false,
      set_abapdoc_fullname_element
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
          element_name        TYPE string
          splitted_prev_name  TYPE string_table,
      set_abapdoc_fullname_struc_tab
        IMPORTING
          type_description TYPE REF TO cl_abap_typedescr
          type_name        TYPE string,

      get_max_length
        IMPORTING element_description TYPE REF TO cl_abap_elemdescr
        RETURNING VALUE(result)       TYPE string,
      get_extrema
        IMPORTING element_description TYPE REF TO cl_abap_elemdescr
        EXPORTING VALUE(max)          TYPE string
                  VALUE(min)          TYPE string.
ENDCLASS.


CLASS zcl_aff_writer_json_schema IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    me->schema_id = schema_id.
    me->format_version = format_version.
  ENDMETHOD.


  METHOD write_element.
    IF ignore_next_elements = abap_true.
      RETURN.
    ENDIF.

    clear_type_specifics( ).

    append_comma_to_prev_line( ).
    DATA(json_type) = get_json_type_from_description( element_description ).
    DATA(formatted_name) = format_name( element_name ).

    DATA(splitted_prev_name) = get_splitted_absolute_name( element_description->absolute_name ).
    set_abapdoc_fullname_element( element_description = element_description element_name = element_name splitted_prev_name = splitted_prev_name ).

    IF abap_doc-required = abap_true AND lines( stack_of_required_tabs ) >= 1.
      FIELD-SYMBOLS <table1> TYPE string_table.
      ASSIGN stack_of_required_tabs[ 1 ] TO <table1>.
      APPEND formatted_name TO <table1>.
    ENDIF.

    DATA(callback_class) = to_upper( abap_doc-callback_class ).
    IF callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = callback_class component_name = fullname_of_type ).
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
      write_open_tag( |"{ formatted_name }": \{| ).
    ENDIF.

    DATA(enum_properties) = get_enum_properties( element_description ).
    IF enum_properties IS NOT INITIAL.
      json_type = zif_aff_writer=>type_info-string.
    ENDIF.

    DATA(check_not_needed) = abap_false.

    IF last_operation( ) = zif_aff_writer=>operation-open_table AND lines( splitted_prev_name ) = 2 AND splitted_prev_name[ 2 ] = element_name.
      check_not_needed = abap_true.
    ENDIF.

    write_title_and_description( type_description = element_description check_not_needed = check_not_needed ).

    IF element_name = c_format_version.
      write_tag( `"type": "string",` ).
      write_tag( |"const": "{ format_version }",| ).
    ELSE.
      write_tag( |"type": "{ get_json_schema_type( element_description = element_description json_type = json_type ) }",| ).
      DATA(format) = get_format( element_description ).
      IF format IS NOT INITIAL.
        write_tag( |"format": "{ format }",| ).
      ENDIF.

      IF enum_properties IS NOT INITIAL.
        handle_enums( element_description = element_description enum_properties = enum_properties ).
      ELSE. "non- enum
        IF json_type = zif_aff_writer=>type_info-numeric.
          handle_extrema( element_description ).
        ELSEIF json_type = zif_aff_writer=>type_info-string AND NOT ( element_description->type_kind = cl_abap_typedescr=>typekind_date OR element_description->type_kind = cl_abap_typedescr=>typekind_time OR
             element_description->type_kind = cl_abap_typedescr=>typekind_utclong ).
          IF is_sy_langu( element_description ).
            handle_language_field( ).
          ELSE.
            handle_string( element_description = element_description ).
          ENDIF.
        ENDIF.
      ENDIF.

      IF abap_doc-default IS NOT INITIAL.
        handle_default( element_description = element_description json_type = json_type enum_properties = enum_properties ).
      ENDIF.
    ENDIF.

*    remove "," in last line
    IF ignore_til_indent_level > indent_level OR ignore_til_indent_level IS INITIAL.
      DATA(last_line) = content[ lines( content ) ].
      content[ lines( content ) ] = substring( val = last_line off = 0 len = strlen( last_line ) - 1 ).
    ENDIF.

    IF last_operation( ) <> zif_aff_writer=>operation-open_table.
      write_closing_tag( `}` ).
    ENDIF.
  ENDMETHOD.


  METHOD write_title_and_description.
    IF check_not_needed = abap_false.
      check_title_and_description( abap_doc_to_check = abap_doc fullname_of_checked_type = fullname_of_type ).
    ENDIF.
    DATA(title) = escape( val = abap_doc-title format = cl_abap_format=>e_json_string ).
    DATA(description) = escape( val = get_description( type_description = type_description ) format = cl_abap_format=>e_json_string ).
    IF title IS NOT INITIAL.
      write_tag( |"title": "{ title }",| ).
    ENDIF.
    IF description IS NOT INITIAL.
      write_tag( |"description": "{ description }",| ).
    ENDIF.
  ENDMETHOD.


  METHOD handle_enums.
    DATA enum_values TYPE string_table.
    write_tag( `"enum": [` ).

    LOOP AT enum_properties-values ASSIGNING FIELD-SYMBOL(<enum_value>).
      IF <enum_value>-overwritten_value IS INITIAL.
        INSERT <enum_value>-value INTO TABLE enum_values.
      ELSE.
        INSERT <enum_value>-overwritten_value INTO TABLE enum_values.
      ENDIF.
    ENDLOOP.
    write_enum_properties( enum_values ).

    IF enum_properties-titles IS NOT INITIAL.
      write_tag( `"enumTitles": [` ).
      write_enum_properties( enum_properties-titles ).
    ENDIF.

    DATA(enum_descr) = get_enum_descriptions( element_description = element_description enum_properties = enum_properties ).
    write_tag( `"enumDescriptions": [` ).
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
      max_length = get_max_length( element_description ).
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
    get_extrema(
      EXPORTING
        element_description = element_description
      IMPORTING
        max                 = DATA(max_value)
        min                 = DATA(min_value) ).
    DATA(multiple_of) = abap_doc-multiple_of.

    IF multiple_of IS INITIAL AND element_description->type_kind = cl_abap_typedescr=>typekind_packed.
      DATA(decimals) = element_description->decimals.
      IF decimals > 0.
        multiple_of = |0.{ repeat( val = `0`  occ = decimals - 1 ) }1|.
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
      default = get_default_from_link( link = abap_doc-default fullname_of_type = fullname_of_type element_type = element_description->type_kind ).
      IF default IS INITIAL.
        RETURN.
      ENDIF.
      READ TABLE enum_properties-values WITH KEY value = default ASSIGNING FIELD-SYMBOL(<entry>).
      IF sy-subrc = 0 AND <entry>-overwritten_value IS NOT INITIAL.
        default = <entry>-overwritten_value.
      ENDIF.

      default = |"{ default }"|.
    ELSEIF is_default_value_valid( element_description = element_description default_value = default fullname_of_type = fullname_of_type ).
      IF json_type = zif_aff_writer=>type_info-numeric OR json_type = zif_aff_writer=>type_info-boolean.
        REPLACE ALL OCCURRENCES OF `"` IN default WITH ``.
      ELSEIF json_type = zif_aff_writer=>type_info-date_time.
        default = `"` && date_time_from_abap_to_json( date_time_abap = default element_description = element_description ) && `"`.
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
    clear_type_specifics( ).
*  add a new empty required_table to the stack
    IF last_operation( ) = zif_aff_writer=>operation-initial.
      INSERT VALUE #( name = structure_name absolute_name = structure_description->absolute_name ) INTO me->stack_of_structure INDEX 1.
      add_required_table_to_stack( ).
      open_json_schema_for_structure( structure_name = structure_name structure_description = structure_description ).
      INSERT VALUE #( name = structure_name number_brackets = 2 ) INTO me->structure_buffer INDEX 1.
      RETURN.
    ENDIF.

    append_comma_to_prev_line( ).

    DATA(formatted_name) = format_name( structure_name ).

    set_abapdoc_fullname_struc_tab( type_description = structure_description type_name = structure_name ).

    DATA(callback_class) = to_upper( abap_doc-callback_class ).
    IF callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = callback_class component_name = fullname_of_type ).
      write_subschema( callback_class = callback_class ).
    ENDIF.

    INSERT VALUE #( name = structure_name absolute_name = structure_description->absolute_name ) INTO me->stack_of_structure INDEX 1.

    IF last_operation( ) <> zif_aff_writer=>operation-open_table.
      write_open_tag( |"{ formatted_name }": \{| ).
      INSERT VALUE #( name = structure_name number_brackets = 2 ) INTO me->structure_buffer INDEX 1.
    ELSE.
      INSERT VALUE #( name = structure_name number_brackets = 1 ) INTO me->structure_buffer INDEX 1.
    ENDIF.

    write_title_and_description( structure_description ).

    IF abap_doc-required = abap_true.
      FIELD-SYMBOLS <table1> TYPE string_table.
      ASSIGN stack_of_required_tabs[ 1 ] TO <table1>.
      APPEND formatted_name TO <table1>.
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
    clear_type_specifics( ).
    IF last_operation( ) = zif_aff_writer=>operation-initial.
      open_json_schema_for_table( table_name = table_name  table_description = CAST cl_abap_tabledescr( table_description ) ).
      INSERT VALUE #( name = table_name number_brackets = 2 ) INTO TABLE me->table_buffer.
      RETURN.
    ENDIF.
    append_comma_to_prev_line( ).
    DATA(formatted_name) = format_name( table_name ).

    set_abapdoc_fullname_struc_tab( type_description = table_description type_name = table_name ).

    IF abap_doc-required = abap_true AND lines( stack_of_required_tabs ) >= 1.
      FIELD-SYMBOLS <table1> TYPE string_table.
      ASSIGN stack_of_required_tabs[ 1 ] TO <table1>.
      APPEND formatted_name TO <table1>.
    ENDIF.

    DATA(callback_class) = to_upper( abap_doc-callback_class ).
    IF callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = callback_class component_name = fullname_of_type ).
      write_subschema( callback_class = callback_class ).
    ENDIF.

    IF last_operation( ) <> zif_aff_writer=>operation-open_table.
      write_open_tag( |"{ formatted_name }": \{| ).
      INSERT VALUE #( name = table_name number_brackets = 2 ) INTO TABLE me->table_buffer.
    ELSE.
      INSERT VALUE #( name = table_name number_brackets = 1 ) INTO TABLE me->table_buffer.
    ENDIF.

    write_title_and_description( table_description ).

    write_tag( `"type": "array",` ).
    IF CAST cl_abap_tabledescr( table_description )->has_unique_key = abap_true.
      write_tag( `"uniqueItems": true,` ).
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

  METHOD set_abapdoc_fullname_struc_tab.
    IF last_operation( ) = zif_aff_writer=>operation-open_table.
      DATA(absolute_name) = type_description->absolute_name.
      DATA(splitted_absolute_name) = get_splitted_absolute_name( absolute_name ).
      DATA(source_type) = splitted_absolute_name[ 1 ].
      DATA(source) = splitted_absolute_name[ 2 ].
      fullname_of_type = type_name.
      DATA(already_found) = abap_true.
    ELSE.
      get_all_path_information(
        EXPORTING
          name             = type_name
        IMPORTING
          source_type      = source_type
          source           = source
          fullname_of_type = fullname_of_type ).
    ENDIF.

    IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
      abap_doc = call_reader_and_decode( name_of_source = source element_name = fullname_of_type ).
    ENDIF.
    IF already_found = abap_false.
      DATA(abap_doc_second) = get_abap_doc_for_absolute_name( type_description->absolute_name ).
      compare_abap_doc(
        EXPORTING
          abap_doc_additional = abap_doc_second
        CHANGING
          abap_doc_base       = abap_doc ).
    ENDIF.
    check_redundant_annotations( ).
  ENDMETHOD.


  METHOD set_abapdoc_fullname_element.
* Simple Component of a structure, defined in the structure definition
    IF lines( stack_of_structure ) > 0.
      get_all_path_information(
        EXPORTING
          name             = element_name
        IMPORTING
          source_type      = DATA(source_type)
          source           = DATA(source)
          fullname_of_type = fullname_of_type ).

* Element which is in no structure
    ELSEIF lines( stack_of_structure ) = 0.
      fullname_of_type = element_name.
      source_type = splitted_prev_name[ 1 ].
      source = splitted_prev_name[ 2 ].
      DATA(already_searched) = abap_true.
    ENDIF.

    IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
      abap_doc = call_reader_and_decode( name_of_source = source element_name = fullname_of_type ).
    ENDIF.

    IF already_searched = abap_false.
      DATA(abap_doc_second) = get_abap_doc_for_absolute_name( absolute_name = element_description->absolute_name ).
      compare_abap_doc(
        EXPORTING
          abap_doc_additional = abap_doc_second
        CHANGING
          abap_doc_base       = abap_doc ).
    ENDIF.
    check_redundant_annotations( ).
  ENDMETHOD.


  METHOD get_json_schema_type.
    IF json_type = zif_aff_writer=>type_info-numeric.
      result = 'number' ##NO_TEXT.
      IF type_is_integer( element_description ) = abap_true.
        result = 'integer'  ##NO_TEXT.
      ENDIF.
    ELSEIF json_type = zif_aff_writer=>type_info-date_time.
      result = 'string' ##NO_TEXT.
    ELSE.
      result = to_lower( json_type ).
    ENDIF.
  ENDMETHOD.


  METHOD open_json_schema_for_structure.
    DATA(absolute_name) = stack_of_structure[ 1 ]-absolute_name.
    DATA(splitted_absolute_name) = get_splitted_absolute_name( absolute_name ).
    DATA(source_type) = splitted_absolute_name[ 1 ].
    IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
      DATA(source) = splitted_absolute_name[ 2 ].
      abap_doc = call_reader_and_decode( name_of_source = source element_name = structure_name ).
    ENDIF.
    fullname_of_type = structure_name.
    check_redundant_annotations( ).
    write_open_tag( '{' ).
    write_tag( |"$comment": "This file is autogenerated, do not edit manually, see { c_link_to_repository } for more information.",| ) ##NO_TEXT.
    write_tag( |"$schema": "{ c_schema_specification }",| ).
    write_tag( |"$id": "{ me->schema_id }",| ).
    DATA(callback_class) = to_upper( abap_doc-callback_class ).
    IF callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = callback_class component_name = structure_name ).
      write_subschema( callback_class = callback_class ).
    ENDIF.

    write_title_and_description( structure_description ).

    write_tag( '"type": "object",' ).
    write_open_tag( '"properties": {' ).
  ENDMETHOD.


  METHOD open_json_schema_for_table.
    DATA(absolute_name) = table_description->absolute_name.
    DATA(splitted_absolute_name) = get_splitted_absolute_name( absolute_name ).
    DATA(source_type) = splitted_absolute_name[ 1 ].
    IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
      DATA(source) = splitted_absolute_name[ 2 ].
      abap_doc = call_reader_and_decode( name_of_source = source element_name = table_name ).
    ENDIF.
    fullname_of_type = table_name.
    check_redundant_annotations( ).
    write_open_tag( '{' ).
    write_tag( |"$comment": "This file is autogenerated, do not edit manually, see { c_link_to_repository } for more information.",| ) ##NO_TEXT.
    write_tag( |"$schema": "{ c_schema_specification }",| ).
    write_tag( |"$id": "{ me->schema_id }",| ).

    DATA(callback_class) = to_upper( abap_doc-callback_class ).
    IF callback_class IS NOT INITIAL AND is_callback_class_valid( class_name = callback_class component_name = table_name ).
      write_subschema( callback_class = callback_class ).
    ENDIF.

    write_title_and_description( table_description ).

    write_tag( '"type": "array",' ).
    IF table_description->has_unique_key = abap_true.
      write_tag( '"uniqueItems": true,' ).
    ENDIF.
    write_open_tag( '"items": {' ).
  ENDMETHOD.


  METHOD write_subschema.
    TRY.
        DATA subschema TYPE string_table.
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
      DATA element_description TYPE REF TO cl_abap_elemdescr.
      TRY.
          element_description = CAST cl_abap_elemdescr( type_description ).
        CATCH cx_sy_move_cast_error.
          RETURN.
      ENDTRY.
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
  ENDMETHOD.


  METHOD get_enum_properties.
    IF abap_doc-enumvalues_link IS NOT INITIAL.
      result = get_properties_from_structure( element_description->type_kind ).
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
        REPLACE ALL OCCURRENCES OF REGEX '\s' IN text WITH '_'  ##REGEX_POSIX.
        INSERT VALUE #( value = format_to_camel_case( text ) ) INTO TABLE result-values.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_enum_descriptions.
    IF abap_doc-enumvalues_link IS NOT INITIAL.
      result = enum_properties-descriptions.
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


  METHOD get_properties_from_structure.
    get_structure_of_enum_values(
      EXPORTING
        link_to_values      = abap_doc-enumvalues_link
        fullname_of_type    = fullname_of_type
      IMPORTING
        structure_of_values = DATA(structure_of_values)
        name_of_source      = DATA(name_of_source)
        name_of_constant    = DATA(name_of_constant) ).

    IF structure_of_values IS NOT INITIAL.
      LOOP AT structure_of_values->components ASSIGNING FIELD-SYMBOL(<component>).
        IF <component>-type_kind <> enum_type.
          DATA(msg) = log->get_message_text( msgno = 122 msgv1 = CONV #( name_of_constant ) msgv2 = CONV #( fullname_of_type ) ).
          RAISE EXCEPTION NEW zcx_aff_tools( message = msg ).
        ENDIF.

        DATA(fullname_of_value) = name_of_constant && '-' && <component>-name.
        DATA(abap_doc_of_component) = call_reader_and_decode( name_of_source = name_of_source element_name = fullname_of_value ).

        APPEND VALUE ty_enum_value( value = format_to_camel_case( CONV #( <component>-name ) )  overwritten_value = abap_doc_of_component-enum_value ) TO result-values.
        APPEND abap_doc_of_component-description TO result-descriptions.
        APPEND abap_doc_of_component-title TO result-titles.

        check_title_and_description( abap_doc_to_check = abap_doc_of_component fullname_of_checked_type = fullname_of_value ).
      ENDLOOP.
      IF abap_doc-required = abap_false AND abap_doc-default IS INITIAL.
        log->add_warning( message_text = zif_aff_log=>co_msg127 component_name = fullname_of_type ).
      ENDIF.
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
      APPEND |{ repeat( val = ` `  occ = indent_level * c_indent_number_characters ) }{ line }| TO content.
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
        abap_date = abap_date && repeat( val = '0' occ = difference ).
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
    DATA msg TYPE string.

    IF ignore_til_indent_level IS INITIAL OR ignore_til_indent_level > indent_level. "Only write message if no callback class provided
      IF abap_doc_to_check-title IS INITIAL.
        msg = log->get_message_text( msgno = 119 msgv1 = `Title` ) ##NO_TEXT.
        log->add_info( message_text = msg component_name = fullname_of_checked_type ).
      ENDIF.

      IF abap_doc_to_check-description IS INITIAL.
        msg = log->get_message_text( msgno = 119 msgv1 = `Description` ) ##NO_TEXT.
        log->add_info( message_text = msg component_name = fullname_of_checked_type ).
      ELSEIF strlen( abap_doc_to_check-description ) > c_max_length_of_description.
        msg = log->get_message_text( msgno = 125 msgv1 = CONV #( c_max_length_of_description ) ).
        log->add_warning( message_text = msg component_name = fullname_of_checked_type ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_aff_writer~validate.
    TRY.
        DATA(string) = concat_lines_of( table = source sep = cl_abap_char_utilities=>newline ).
        DATA(json_as_xstring) = cl_abap_codepage=>convert_to( string ).
        DATA(json_reader) = cl_sxml_string_reader=>create( json_as_xstring ).
        json_reader->next_node( ).
        json_reader->skip_node( ).
      CATCH cx_sxml_parse_error INTO DATA(exception).
        log->add_exception( exception = exception component_name = `` ).
        RETURN.
    ENDTRY.
    result = abap_true.
  ENDMETHOD.


  METHOD handle_language_field.
    write_tag( `"minLength": 2,` ).
    write_tag( `"maxLength": 2,` ).
    write_tag( `"pattern": "^[a-z]+$",` ).
  ENDMETHOD.


  METHOD get_max_length.
    IF element_description->output_length > 0.
      DATA(length) = COND i( WHEN ( element_description->length / cl_abap_char_utilities=>charsize ) > element_description->output_length THEN element_description->length / cl_abap_char_utilities=>charsize ELSE element_description->output_length ).
      DATA length_as_string TYPE string.
      length_as_string = length.
      remove_leading_trailing_spaces( CHANGING string_to_work_on = length_as_string ).
      result = length_as_string.
    ENDIF.
  ENDMETHOD.


  METHOD get_extrema.
    DATA r_field TYPE REF TO data.
    FIELD-SYMBOLS <field> TYPE any.
    CREATE DATA r_field TYPE HANDLE element_description.
    ASSIGN r_field->* TO <field>.

    DATA(max_val) = cl_abap_exceptional_values=>get_max_value( <field> ).
    ASSIGN max_val->* TO FIELD-SYMBOL(<max>).
    IF <max> IS ASSIGNED.
      max = <max>.
      REPLACE ALL OCCURRENCES OF 'E' IN max WITH 'e'.
      REPLACE ALL OCCURRENCES OF '+' IN max WITH ''.
      remove_leading_trailing_spaces( CHANGING string_to_work_on = max ).
    ENDIF.

    IF element_description->type_kind = cl_abap_typedescr=>typekind_decfloat OR
          element_description->type_kind = cl_abap_typedescr=>typekind_decfloat16 OR
          element_description->type_kind = cl_abap_typedescr=>typekind_decfloat34.
      IF <max> IS ASSIGNED.
        min = '-' && max.
      ENDIF.
    ELSE.
      DATA(min_val) = cl_abap_exceptional_values=>get_min_value( <field> ).
      ASSIGN min_val->* TO FIELD-SYMBOL(<min>).
      IF <min> IS ASSIGNED.
        DATA min_str TYPE string.
        min_str = <min>.
        DATA(length) = strlen( min_str ) - 1.
        DATA(front) = substring( val = min_str off = 0 len = length ).
        DATA(back) = substring( val = min_str off = length len = 1 ).
        IF back = '-'.
          min = back && front.
        ELSE.
          min = min_str.
        ENDIF.
        REPLACE ALL OCCURRENCES OF 'E' IN min WITH 'e'.
        REPLACE ALL OCCURRENCES OF '+' IN min WITH ''.
        remove_leading_trailing_spaces( CHANGING string_to_work_on = min ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

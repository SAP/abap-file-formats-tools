CLASS zcl_aff_writer DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_aff_writer
      FINAL METHODS open_node close_node write_element get_output.

    METHODS constructor.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_stack_entry,
        operation TYPE string,
        name      TYPE string,
      END OF ty_stack_entry.

    TYPES:
      BEGIN OF ty_structure_stack,
        name          TYPE string,
        absolute_name TYPE abap_abstypename,
      END OF ty_structure_stack,
      tt_structure_stack TYPE STANDARD TABLE OF ty_structure_stack.

    CONSTANTS:
      c_indent_number_characters TYPE i VALUE 2.

    DATA:
      output                  TYPE string_table,
      content                 TYPE string_table,
      stack_of_structure      TYPE tt_structure_stack,
      stack                   TYPE STANDARD TABLE OF ty_stack_entry,
      indent_level            TYPE i VALUE 0,
      log                     TYPE REF TO zif_aff_log,
      abap_doc_parser         TYPE REF TO zcl_aff_abap_doc_parser,
      ignore_til_indent_level TYPE i,
      abap_doc                TYPE zcl_aff_abap_doc_parser=>abap_doc,
      fullname_of_type        TYPE string.

    METHODS: format_name
      IMPORTING name          TYPE string
      RETURNING VALUE(result) TYPE string,
      get_json_type_from_description
        IMPORTING element_description TYPE REF TO cl_abap_elemdescr
        RETURNING VALUE(result)       TYPE string
        RAISING   zcx_aff_tools,

      write_open_tag FINAL
        IMPORTING
          line TYPE string,
      write_closing_tag FINAL
        IMPORTING
          line TYPE string,
      add_to_stack FINAL
        IMPORTING
          entry TYPE ty_stack_entry,
      last_operation FINAL
        RETURNING VALUE(result) TYPE string,
      append_to_previous_line FINAL
        IMPORTING
          string TYPE string,
      append_before_output,
      append_after_output,

      write_tag ABSTRACT
        IMPORTING
          line TYPE string,

      write_element ABSTRACT
        IMPORTING
                  element_name        TYPE string
                  element_description TYPE REF TO cl_abap_elemdescr
        RAISING   zcx_aff_tools,

      open_structure ABSTRACT
        IMPORTING
                  structure_name        TYPE string
                  structure_description TYPE REF TO cl_abap_typedescr
        RAISING   zcx_aff_tools,

      close_structure ABSTRACT
        IMPORTING
                  structure_name        TYPE string
                  structure_description TYPE REF TO cl_abap_typedescr
        RAISING   zcx_aff_tools,

      open_table ABSTRACT
        IMPORTING
                  table_name        TYPE string
                  table_description TYPE REF TO cl_abap_typedescr
        RAISING   zcx_aff_tools ##NEEDED,

      close_table ABSTRACT
        IMPORTING
                  table_name        TYPE string
                  table_description TYPE REF TO cl_abap_typedescr
        RAISING   zcx_aff_tools ##NEEDED,

      format_to_camel_case
        IMPORTING name          TYPE string
        RETURNING VALUE(result) TYPE string,

      call_reader_and_decode
        IMPORTING
          name_of_source       TYPE string
          element_name         TYPE string
        RETURNING
          VALUE(read_abap_doc) TYPE zcl_aff_abap_doc_parser=>abap_doc,

      delete_first_of_struc_stack,

      get_all_path_information
        IMPORTING
          name                    TYPE string
        EXPORTING
          VALUE(source_type)      TYPE string
          VALUE(source)           TYPE string
          VALUE(fullname_of_type) TYPE string,

      get_structure_of_enum_values
        IMPORTING
          link_to_values             TYPE string
          fullname_of_type           TYPE string
        EXPORTING
          VALUE(structure_of_values) TYPE REF TO cl_abap_structdescr
          VALUE(name_of_source)      TYPE string
          VALUE(name_of_constant)    TYPE string,


      get_abap_doc_for_absolute_name
        IMPORTING
          absolute_name   TYPE abap_abstypename
        RETURNING
          VALUE(abap_doc) TYPE zcl_aff_abap_doc_parser=>abap_doc,

      compare_abap_doc
        IMPORTING
          abap_doc_additional TYPE zcl_aff_abap_doc_parser=>abap_doc
        CHANGING
          abap_doc_base       TYPE zcl_aff_abap_doc_parser=>abap_doc,

      get_splitted_absolute_name
        IMPORTING
          absolute_name TYPE abap_abstypename
        RETURNING
          VALUE(result) TYPE string_table,

      get_default_from_link
        IMPORTING
          link                 TYPE string
          fullname_of_type     TYPE string
          element_type         TYPE abap_typekind
        RETURNING
          VALUE(default_value) TYPE string,

      remove_leading_trailing_spaces
        CHANGING
          string_to_work_on TYPE string,

      is_callback_class_valid
        IMPORTING
          class_name      TYPE string
          component_name  TYPE string
        RETURNING
          VALUE(is_valid) TYPE abap_boolean,

      is_default_value_valid
        IMPORTING element_description TYPE REF TO cl_abap_elemdescr
                  default_value       TYPE string
                  fullname_of_type    TYPE string
        RETURNING VALUE(is_valid)     TYPE abap_boolean
        RAISING   zcx_aff_tools,

      is_sy_langu
        IMPORTING
          element_description TYPE REF TO cl_abap_elemdescr
        RETURNING
          VALUE(result)       TYPE abap_bool,

      clear_type_specifics,

      check_redundant_annotations.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_abap_types,
        boolean   TYPE string VALUE `ABAP_BOOLEAN;ABAP_BOOL;BOOLEAN;BOOLE_D;XFELD;XSDBOOLEAN;FLAG`,
        timestamp TYPE string VALUE `TIMESTAMP;TIMESTAMPL`,
      END OF c_abap_types.


    METHODS: is_type_timestamp
      IMPORTING element_description TYPE REF TO cl_abap_elemdescr
      RETURNING VALUE(result)       TYPE abap_boolean,

      is_type_boolean
        IMPORTING element_description TYPE REF TO cl_abap_elemdescr
        RETURNING VALUE(result)       TYPE abap_boolean,

      get_constant_as_struc
        IMPORTING
          name_of_source           TYPE string
          name_of_constant         TYPE string
          fullname_of_type         TYPE string
        RETURNING
          VALUE(constant_as_struc) TYPE REF TO cl_abap_structdescr,

      get_infos_of_values_link
        IMPORTING
          values_link             TYPE string
        EXPORTING
          VALUE(name_of_source)   TYPE string
          VALUE(name_of_constant) TYPE string,

      validate_default_link
        IMPORTING
          splitted_link    TYPE string_table
          fullname_of_type TYPE string
          element_type     TYPE abap_typekind
        RETURNING
          VALUE(is_valid)  TYPE abap_boolean.



ENDCLASS.

CLASS zcl_aff_writer IMPLEMENTATION.

  METHOD constructor.
    log = NEW zcl_aff_log( ).
    abap_doc_parser = NEW zcl_aff_abap_doc_parser( ).
  ENDMETHOD.


  METHOD format_name.
    result = me->format_to_camel_case( name ).
  ENDMETHOD.


  METHOD format_to_camel_case.
    DATA(lower_name) = to_lower( name ).
    result = to_mixed( lower_name ).
  ENDMETHOD.


  METHOD get_json_type_from_description.
    CASE element_description->type_kind.
      WHEN cl_abap_typedescr=>typekind_string OR cl_abap_typedescr=>typekind_csequence OR
           cl_abap_typedescr=>typekind_clike OR cl_abap_typedescr=>typekind_char OR
           cl_abap_typedescr=>typekind_w OR cl_abap_typedescr=>typekind_xstring OR
           cl_abap_typedescr=>typekind_hex OR cl_abap_typedescr=>typekind_num OR cl_abap_typedescr=>typekind_enum.
        result = COND #( WHEN is_type_boolean( element_description ) THEN zif_aff_writer=>type_info-boolean
                         ELSE zif_aff_writer=>type_info-string ).
      WHEN cl_abap_typedescr=>typekind_float OR cl_abap_typedescr=>typekind_int OR
           cl_abap_typedescr=>typekind_int1 OR cl_abap_typedescr=>typekind_int2 OR
           cl_abap_typedescr=>typekind_int8 OR cl_abap_typedescr=>typekind_decfloat OR
           cl_abap_typedescr=>typekind_decfloat16 OR cl_abap_typedescr=>typekind_decfloat34  OR cl_abap_typedescr=>typekind_numeric.
        result = zif_aff_writer=>type_info-numeric.
      WHEN cl_abap_typedescr=>typekind_packed.
        result = COND #( WHEN is_type_timestamp( element_description ) THEN zif_aff_writer=>type_info-date_time
                         ELSE zif_aff_writer=>type_info-numeric ).
      WHEN cl_abap_typedescr=>typekind_date OR cl_abap_typedescr=>typekind_time OR
           cl_abap_typedescr=>typekind_utclong.
        result = zif_aff_writer=>type_info-date_time.
      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_aff_tools( ).
    ENDCASE.
  ENDMETHOD.


  METHOD is_type_boolean.
    DATA(type_name) = element_description->get_relative_name( ).
    result = xsdbool( element_description->output_length = 1 AND ( type_name IS NOT INITIAL AND c_abap_types-boolean CS type_name ) ).
  ENDMETHOD.


  METHOD is_type_timestamp.
    DATA(type_name) = element_description->get_relative_name( ).
    result = xsdbool( type_name IS NOT INITIAL AND c_abap_types-timestamp CS type_name ).
  ENDMETHOD.


  METHOD zif_aff_writer~write_element.
    write_element( element_name = element_name element_description = element_description ).
    add_to_stack( VALUE #( operation = zif_aff_writer=>operation-write_element name = element_name ) ).
  ENDMETHOD.


  METHOD zif_aff_writer~open_node.
    CASE node_description->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        open_structure( structure_name = node_name structure_description = node_description ).
        add_to_stack( VALUE #( operation = zif_aff_writer=>operation-open_structure name = node_name ) ).

      WHEN cl_abap_typedescr=>kind_table.
        open_table( table_name = node_name table_description = node_description ).
        add_to_stack( VALUE #( operation = zif_aff_writer=>operation-open_table name = node_name ) ).
      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_aff_tools( ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_aff_writer~close_node.
    CASE node_description->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        close_structure( structure_name = node_name structure_description = node_description ).
        add_to_stack( VALUE #( operation = zif_aff_writer=>operation-close_structure name = node_name ) ).

      WHEN cl_abap_typedescr=>kind_table.
        close_table( table_name = node_name table_description = node_description ).
        add_to_stack( VALUE #( operation = zif_aff_writer=>operation-close_table name = node_name ) ).

      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_aff_tools( ).
    ENDCASE.
  ENDMETHOD.


  METHOD zif_aff_writer~get_output.
    append_before_output( ).
    APPEND LINES OF content TO output.
    append_after_output( ).
    result = output.
  ENDMETHOD.


  METHOD write_open_tag.
    write_tag( line ).
    indent_level += 1.
  ENDMETHOD.


  METHOD write_closing_tag.
    indent_level -= 1.
    write_tag( line ).
  ENDMETHOD.


  METHOD add_to_stack.
    INSERT entry INTO stack INDEX 1.
  ENDMETHOD.


  METHOD last_operation.
    IF stack IS NOT INITIAL.
      result = VALUE #( stack[ 1 ]-operation OPTIONAL ).
    ELSE.
      result = zif_aff_writer=>operation-initial.
    ENDIF.
  ENDMETHOD.


  METHOD append_to_previous_line.
    DATA(index) = lines( me->content ).
    IF index > 0.
      me->content[ index ] = me->content[ index ] && string.
    ELSE.
      INSERT string INTO TABLE me->content.
    ENDIF.
  ENDMETHOD.


  METHOD append_after_output ##NEEDED.

  ENDMETHOD.


  METHOD append_before_output ##NEEDED.

  ENDMETHOD.


  METHOD call_reader_and_decode.
    DATA(ref) = cl_oo_factory=>create_instance( )->create_clif_source( name_of_source ).
    ref->get_source( IMPORTING source = DATA(source) ).
    DATA(reader) = zcl_aff_abap_doc_reader=>create_instance(
      name   = name_of_source
      source = source ).
    TRY.
        DATA(result) = reader->get_abap_doc_for_element( element_name ).

        read_abap_doc = abap_doc_parser->parse(
          EXPORTING
            component_name = element_name
            to_parse       = result
          CHANGING
            log            = log ).
      CATCH cx_root ##NO_HANDLER ##CATCH_ALL.
    ENDTRY.
  ENDMETHOD.


  METHOD remove_leading_trailing_spaces.
    SHIFT string_to_work_on RIGHT DELETING TRAILING space.
    SHIFT string_to_work_on LEFT DELETING LEADING space.
  ENDMETHOD.


  METHOD delete_first_of_struc_stack.
    IF stack_of_structure IS NOT INITIAL.
      DELETE stack_of_structure INDEX 1.
    ENDIF.
  ENDMETHOD.


  METHOD get_all_path_information.
    DATA previous_absolute_name TYPE abap_abstypename.
    DATA splitted_prev_name TYPE string_table.
    DATA(index) = 0.
    WHILE lines( splitted_prev_name ) <= 2.
      IF index >= lines( stack_of_structure ).
        RETURN.
      ENDIF.
      index = index + 1.
      previous_absolute_name = stack_of_structure[ index ]-absolute_name.
      splitted_prev_name = get_splitted_absolute_name( previous_absolute_name ).
    ENDWHILE.
    DATA(name_of_prev) = splitted_prev_name[ lines( splitted_prev_name ) ].
    source_type = splitted_prev_name[ 1 ].
    source = splitted_prev_name[ 2 ].
    fullname_of_type = name_of_prev && '-'.
    index = index - 1.
    WHILE index > 0.
      fullname_of_type = fullname_of_type  && stack_of_structure[ index ]-name && '-'.
      index = index - 1.
    ENDWHILE.
    fullname_of_type = fullname_of_type && name.
  ENDMETHOD.


  METHOD get_splitted_absolute_name.
    DATA(place_of_type) = absolute_name.
    SPLIT place_of_type AT '\' INTO TABLE DATA(splitted_in_componets).
    LOOP AT splitted_in_componets ASSIGNING FIELD-SYMBOL(<component>).
      IF <component> IS NOT INITIAL.
        SPLIT <component> AT '=' INTO TABLE DATA(splitted_in_details).
        APPEND LINES OF splitted_in_details TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_structure_of_enum_values.
    get_infos_of_values_link(
      EXPORTING
        values_link      = link_to_values
      IMPORTING
        name_of_source   = name_of_source
        name_of_constant = name_of_constant ).

    structure_of_values = get_constant_as_struc(
      name_of_source   = name_of_source
      name_of_constant = name_of_constant
      fullname_of_type = fullname_of_type ).
  ENDMETHOD.

  METHOD get_constant_as_struc.
    DATA constant TYPE REF TO cl_abap_datadescr.
    DATA msg TYPE string.

    cl_abap_typedescr=>describe_by_name(
      EXPORTING
        p_name         = name_of_source
      RECEIVING
        p_descr_ref    = DATA(constant_descr)
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).

    IF sy-subrc <> 0.
*    class or interface doesn't exist
      msg = log->get_message_text( msgno = 103 msgv1 = CONV #( name_of_source ) ).
      log->add_warning( message_text = msg component_name = fullname_of_type ).
    ELSE.
      IF constant_descr->kind = cl_abap_typedescr=>kind_intf.
        DATA(constant_descr_intf) = CAST cl_abap_intfdescr( constant_descr ).
        constant_descr_intf->get_attribute_type(
          EXPORTING
            p_name              = name_of_constant
          RECEIVING
            p_descr_ref         = constant
          EXCEPTIONS
            attribute_not_found = 1
            OTHERS              = 2 ).
        IF sy-subrc <> 0.
*      constant in interface does not exist
          msg = log->get_message_text( msgno = 104 msgv1 = CONV #( name_of_source && '=>' && name_of_constant ) ).
          log->add_warning( message_text = msg component_name = fullname_of_type ).
        ENDIF.
      ELSEIF constant_descr->kind = cl_abap_typedescr=>kind_class.
        DATA(constant_descr_clas) = CAST cl_abap_classdescr( constant_descr ).
        constant_descr_clas->get_attribute_type(
          EXPORTING
            p_name              = name_of_constant
          RECEIVING
            p_descr_ref         = constant
          EXCEPTIONS
            attribute_not_found = 1
            OTHERS              = 2 ).
        IF sy-subrc <> 0.
*      constant in class does not exits
          msg = log->get_message_text( msgno = 104 msgv1 = CONV #( name_of_source && '=>' && name_of_constant ) ).
          log->add_warning( message_text = msg component_name = fullname_of_type ).
        ENDIF.
      ENDIF.
      constant_as_struc = CAST cl_abap_structdescr( constant ).
    ENDIF.
  ENDMETHOD.


  METHOD get_infos_of_values_link.
    DATA(link) = values_link.
    REPLACE ALL OCCURRENCES OF REGEX `[\s]` IN link WITH `` ##REGEX_POSIX.
    REPLACE ALL OCCURRENCES OF `data:` IN link WITH ``.
    SPLIT link AT '.' INTO TABLE DATA(split_at_point).
    IF lines( split_at_point ) = 2.
      name_of_source = to_upper( split_at_point[ 1 ] ).
      name_of_constant = to_upper( split_at_point[ 2 ] ).
    ENDIF.
  ENDMETHOD.


  METHOD get_abap_doc_for_absolute_name.
    DATA(splitted_prev_name) = get_splitted_absolute_name( absolute_name ).
    IF lines( splitted_prev_name ) >= 4.
      DATA(source_type) = splitted_prev_name[ 1 ].
      DATA(source) = splitted_prev_name[ 2 ].
      DATA(fullname_of_type) = splitted_prev_name[ 4 ].
      IF source_type = 'CLASS' OR source_type = 'INTERFACE'.
        abap_doc = call_reader_and_decode( name_of_source = source element_name = fullname_of_type ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD compare_abap_doc.
    IF abap_doc_base-enumvalues_link IS INITIAL.
      abap_doc_base-enumvalues_link = abap_doc_additional-enumvalues_link.
    ENDIF.
    IF abap_doc_base-title IS INITIAL AND abap_doc_base-description IS INITIAL.
      abap_doc_base-title = abap_doc_additional-title.
      abap_doc_base-description = abap_doc_additional-description.
    ENDIF.
    IF abap_doc_base-minimum IS INITIAL AND abap_doc_base-maximum IS INITIAL AND abap_doc_base-exclusive_maximum IS INITIAL AND abap_doc_base-exclusive_minimum IS INITIAL.
      abap_doc_base-minimum = abap_doc_additional-minimum.
      abap_doc_base-maximum = abap_doc_additional-maximum.
      abap_doc_base-exclusive_minimum = abap_doc_additional-exclusive_minimum.
      abap_doc_base-exclusive_maximum = abap_doc_additional-exclusive_maximum.
    ENDIF.
    IF abap_doc_base-multiple_of IS INITIAL.
      abap_doc_base-multiple_of = abap_doc_additional-multiple_of.
    ENDIF.
    IF abap_doc_base-max_length IS INITIAL AND abap_doc_base-min_length IS INITIAL.
      abap_doc_base-min_length = abap_doc_additional-min_length.
      abap_doc_base-max_length = abap_doc_additional-max_length.
    ENDIF.
    IF abap_doc_base-default IS INITIAL.
      abap_doc_base-default = abap_doc_additional-default.
    ENDIF.
    IF abap_doc_base-callback_class IS INITIAL.
      abap_doc_base-callback_class = abap_doc_additional-callback_class.
    ENDIF.
  ENDMETHOD.


  METHOD get_default_from_link.
    DATA(link_to_work_on) = link.
    REPLACE ALL OCCURRENCES OF REGEX `(@link|data:)` IN link_to_work_on WITH `` ##REGEX_POSIX.
    REPLACE ALL OCCURRENCES OF REGEX `[\s]` IN link_to_work_on WITH `` ##REGEX_POSIX.
    SPLIT link_to_work_on AT '.' INTO TABLE DATA(splitted).
    IF validate_default_link( splitted_link = splitted fullname_of_type = fullname_of_type element_type = element_type ) = abap_true.
      DATA(default_abap) = splitted[ lines( splitted ) ].
      default_value = format_to_camel_case( default_abap ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_aff_writer~get_log.
    log = me->log.
  ENDMETHOD.

  METHOD is_callback_class_valid.
    DATA(name_of_callback_class) = to_upper( class_name ).
    cl_oo_classname_service=>get_all_method_includes(
      EXPORTING
        clsname            = CONV #( name_of_callback_class )
      RECEIVING
        result             = DATA(result)
      EXCEPTIONS
        class_not_existing = 1 ).
    IF sy-subrc = 0.
      DATA(has_method_get_subschema) = xsdbool( line_exists( result[ cpdkey = VALUE #( clsname = name_of_callback_class cpdname = 'GET_SUBSCHEMA' ) ] ) ).
      DATA(has_method_serialize) = xsdbool( line_exists( result[ cpdkey = VALUE #( clsname = name_of_callback_class cpdname = 'SERIALIZE' ) ] ) ).
      DATA(has_method_deserialize) = xsdbool( line_exists( result[ cpdkey = VALUE #( clsname = name_of_callback_class cpdname = 'DESERIALIZE' ) ] ) ).
      is_valid = xsdbool( has_method_get_subschema = abap_true AND has_method_serialize = abap_true AND has_method_deserialize = abap_true ).
    ENDIF.
    IF is_valid = abap_false.
      log->add_warning( message_text = zif_aff_log=>co_msg106 component_name = component_name ).
    ENDIF.
  ENDMETHOD.

  METHOD validate_default_link.
    DATA msg TYPE string.
    IF lines( splitted_link ) = 3.
      DATA(source_name) = to_upper( splitted_link[ 1 ] ).
      DATA(constant_name) = to_upper( splitted_link[ 2 ] ).
      DATA(component_name) = to_upper( splitted_link[ 3 ] ).
      DATA(constant_description) = get_constant_as_struc(
        name_of_source   = source_name
        name_of_constant = constant_name
        fullname_of_type = fullname_of_type ).
      IF constant_description IS NOT INITIAL.
        DATA(components) = constant_description->get_components( ).
        DATA(row) = VALUE #( components[ name = component_name ] OPTIONAL ).
        IF row IS NOT INITIAL.
          IF row-type->type_kind = element_type.
            is_valid = abap_true.
          ELSE.
            msg = log->get_message_text( msgno = 122 msgv1 = CONV #( constant_name ) msgv2 = CONV #( fullname_of_type ) ).
            log->add_warning( message_text = msg component_name = fullname_of_type ).
          ENDIF.
        ELSE.
          msg = log->get_message_text( msgno = 105 msgv1 = CONV #( component_name ) msgv2 = CONV #( constant_name ) ).
          log->add_warning( message_text = msg component_name = fullname_of_type ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD is_default_value_valid.
    DATA(default) = default_value.
    REPLACE ALL OCCURRENCES OF `"` IN default WITH ``.
    DATA(type) = get_json_type_from_description( element_description ).
    DATA r_field TYPE REF TO data.
    FIELD-SYMBOLS <field> TYPE any.
    CREATE DATA r_field TYPE HANDLE element_description.
    ASSIGN r_field->* TO <field>.
    IF element_description->type_kind = cl_abap_typedescr=>typekind_utclong.
*      No support for default with utclong
      DATA(message_text) = log->get_message_text( msgno = 117 msgv1 = `UTCLONG` ).
      log->add_warning( message_text = message_text component_name = fullname_of_type ).
      is_valid = abap_false.
      RETURN.
    ELSEIF type = zif_aff_writer=>type_info-boolean.
      default = to_lower( default ).
      IF default = 'abap_true' OR default = 'x' OR default = 'abap_false' OR default = ''.
        is_valid = abap_true.
      ENDIF.
    ELSEIF type = zif_aff_writer=>type_info-string OR type = zif_aff_writer=>type_info-date_time.
      DATA string TYPE string.
      TRY.
          <field> = default.
          string = <field>.
          IF element_description->type_kind = cl_abap_typedescr=>typekind_num OR element_description->type_kind = cl_abap_typedescr=>typekind_numeric.
            SHIFT string LEFT DELETING LEADING '0'.
          ENDIF.
          IF element_description->type_kind = cl_abap_typedescr=>typekind_time.
            default = default && repeat( val = '0' occ = 6 - strlen( default ) ).
          ENDIF.
          IF element_description->type_kind = cl_abap_typedescr=>typekind_utclong.
            REPLACE REGEX `T|t` IN default WITH ` ` ##REGEX_POSIX.
          ENDIF.
          remove_leading_trailing_spaces( CHANGING string_to_work_on = string ).
          remove_leading_trailing_spaces( CHANGING string_to_work_on = default ).
          IF string = default.
            is_valid = abap_true.
          ELSE.
            is_valid = abap_false.
          ENDIF.
        CATCH cx_root.
          is_valid = abap_false.
      ENDTRY.
    ELSEIF type = zif_aff_writer=>type_info-numeric.
      TRY.
          <field> = default.
          IF <field> - default = 0.
            is_valid = abap_true.
          ELSE.
            is_valid = abap_false.
          ENDIF.
        CATCH cx_root.
          is_valid = abap_false.
      ENDTRY.
    ENDIF.
    IF is_valid = abap_false.
      log->add_warning( message_text = zif_aff_log=>co_msg114 component_name = fullname_of_type ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_aff_writer~validate.
    result = abap_true.
  ENDMETHOD.


  METHOD zif_aff_writer~close_include.
    delete_first_of_struc_stack( ).
  ENDMETHOD.


  METHOD zif_aff_writer~open_include.
    INSERT VALUE #( absolute_name = include_description->absolute_name ) INTO me->stack_of_structure INDEX 1.
  ENDMETHOD.


  METHOD is_sy_langu.
    DATA(sy_langu_description) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( VALUE sy-langu( ) ) ).
    result = xsdbool( sy_langu_description->edit_mask = element_description->edit_mask ).
  ENDMETHOD.

  METHOD clear_type_specifics.
    CLEAR abap_doc.
    CLEAR fullname_of_type.
  ENDMETHOD.

  METHOD check_redundant_annotations.
    IF abap_doc-showalways = abap_true AND abap_doc-required = abap_true.
      DATA(msg) = log->get_message_text( msgno = 112 ).
      log->add_info( message_text = msg component_name = fullname_of_type ).
    ENDIF.

    IF abap_doc-required = abap_true AND abap_doc-default IS NOT INITIAL.
      log->add_warning( message_text = zif_aff_log=>co_msg126 component_name = fullname_of_type ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

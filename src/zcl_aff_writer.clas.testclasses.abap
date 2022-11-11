CLASS ltcl_type_writer DEFINITION DEFERRED.

CLASS ltcl_writer_testable DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS
  INHERITING FROM zcl_aff_writer
  FRIENDS ltcl_type_writer.
  PROTECTED SECTION.
    METHODS:
      write_element REDEFINITION,
      open_structure REDEFINITION,
      close_structure REDEFINITION,
      open_table REDEFINITION,
      write_tag REDEFINITION,
      close_table REDEFINITION.
ENDCLASS.

CLASS ltcl_writer_testable IMPLEMENTATION.
  METHOD write_element ##NEEDED.
  ENDMETHOD.
  METHOD close_structure ##NEEDED.
  ENDMETHOD.
  METHOD close_table ##NEEDED.
  ENDMETHOD.
  METHOD open_structure ##NEEDED.
  ENDMETHOD.
  METHOD open_table ##NEEDED.
  ENDMETHOD.
  METHOD write_tag.
    APPEND |{ repeat( val = ` `  occ = indent_level * c_indent_number_characters ) }{ line }| TO content.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_type_writer DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO ltcl_writer_testable.

    METHODS: camel_case FOR TESTING RAISING cx_static_check,
      get_output FOR TESTING RAISING cx_static_check,
      get_type_info_string_like FOR TESTING RAISING cx_static_check,
      get_type_info_string_like_enum FOR TESTING RAISING cx_static_check,
      get_type_info_boolean1 FOR TESTING RAISING cx_static_check,
      get_type_info_boolean2 FOR TESTING RAISING cx_static_check,
      get_type_info_boolean3 FOR TESTING RAISING cx_static_check,
      get_type_info_numeric FOR TESTING RAISING cx_static_check,
      get_type_info_date_time FOR TESTING RAISING cx_static_check,
      stack_stores_operations FOR TESTING RAISING cx_static_check,
      append_to_previous_line FOR TESTING RAISING cx_static_check,
      call_reader_and_decode1 FOR TESTING RAISING cx_static_check,
      call_reader_and_decode2 FOR TESTING RAISING cx_static_check,
      call_reader_and_decode3 FOR TESTING RAISING cx_static_check,
      call_reader_and_decode4 FOR TESTING RAISING cx_static_check,
      call_reader_and_decode5 FOR TESTING RAISING cx_static_check,
      call_reader_and_decode6 FOR TESTING RAISING cx_static_check,
      call_reader_and_decode7 FOR TESTING RAISING cx_static_check,
      delete_first_of_struc_stack FOR TESTING RAISING cx_static_check,
      get_all_path_information FOR TESTING RAISING cx_static_check,
      get_abap_doc_for_absolute_name FOR TESTING RAISING cx_static_check,
      compare_abap_doc FOR TESTING RAISING cx_static_check,
      callback_class_is_valid FOR TESTING RAISING cx_static_check,
      callback_class_is_invalid FOR TESTING RAISING cx_static_check,
      validate_default FOR TESTING RAISING cx_static_check,
      validate_source FOR TESTING RAISING cx_static_check,
      get_struc_of_enum_values_cl FOR TESTING RAISING cx_static_check,
      handle_include FOR TESTING RAISING cx_static_check,
      setup.

    METHODS: get_element_description
      IMPORTING data          TYPE data
      RETURNING VALUE(result) TYPE REF TO cl_abap_elemdescr.
ENDCLASS.


CLASS ltcl_type_writer IMPLEMENTATION.

  METHOD setup.
    cut = NEW ltcl_writer_testable( ).
  ENDMETHOD.


  METHOD get_output.
    DATA(exp_output) = VALUE string_table( ( `line1` ) ( `line2` ) ).
    cut->output = exp_output.

    DATA(act_output) = cut->zif_aff_writer~get_output( ).

    cl_abap_unit_assert=>assert_equals( exp = exp_output act = act_output ).
  ENDMETHOD.

  METHOD camel_case.
    DATA(act_name) = cut->format_name( 'MY_TEst_nAmE' ).

    cl_abap_unit_assert=>assert_equals( act = act_name exp = 'myTestName' msg = |Actual was { act_name }, but expected is 'myTestName'| ).
  ENDMETHOD.


  METHOD get_type_info_string_like_enum.
    TYPES:
      BEGIN OF ENUM my_enum,
        option1,
        option2,
      END OF ENUM my_enum.

    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-string act = cut->get_json_type_from_description( get_element_description( VALUE my_enum( ) ) ) ).
  ENDMETHOD.

  METHOD get_type_info_string_like.
    DATA char TYPE c LENGTH 1.
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-string act = cut->get_json_type_from_description( get_element_description( char ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-string act = cut->get_json_type_from_description( get_element_description( VALUE string( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-string act = cut->get_json_type_from_description( get_element_description( VALUE char1( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-string act = cut->get_json_type_from_description( get_element_description( VALUE xstring( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-string act = cut->get_json_type_from_description( get_element_description( VALUE numc4( ) ) ) ).
  ENDMETHOD.

  METHOD get_type_info_boolean1.
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-boolean act = cut->get_json_type_from_description( get_element_description( VALUE abap_boolean( ) ) ) ).
  ENDMETHOD.

  METHOD get_type_info_boolean2.
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-boolean act = cut->get_json_type_from_description( get_element_description( VALUE abap_bool( ) ) ) ).
  ENDMETHOD.

  METHOD get_type_info_boolean3.
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-boolean act = cut->get_json_type_from_description( get_element_description( VALUE flag( ) ) ) ).
  ENDMETHOD.

  METHOD get_type_info_numeric.
    DATA packed TYPE p.
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-numeric act = cut->get_json_type_from_description( get_element_description( packed ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-numeric act = cut->get_json_type_from_description( get_element_description( VALUE f( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-numeric act = cut->get_json_type_from_description( get_element_description( VALUE int1( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-numeric act = cut->get_json_type_from_description( get_element_description( VALUE int2( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-numeric act = cut->get_json_type_from_description( get_element_description( VALUE int8( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-numeric act = cut->get_json_type_from_description( get_element_description( VALUE decfloat16( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-numeric act = cut->get_json_type_from_description( get_element_description( VALUE decfloat34( ) ) ) ).
  ENDMETHOD.

  METHOD get_type_info_date_time.
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-date_time act = cut->get_json_type_from_description( get_element_description( VALUE d( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-date_time act = cut->get_json_type_from_description( get_element_description( VALUE t( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-date_time act = cut->get_json_type_from_description( get_element_description( VALUE timestamp( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-date_time act = cut->get_json_type_from_description( get_element_description( VALUE timestampl( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-date_time act = cut->get_json_type_from_description( get_element_description( VALUE utclong( ) ) ) ).
  ENDMETHOD.

  METHOD stack_stores_operations.
    DATA table TYPE STANDARD TABLE OF tadir.

    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>operation-initial act = cut->last_operation( ) ).

    cut->zif_aff_writer~write_element( element_name = 'write_element' element_description = VALUE #( ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>operation-write_element act = cut->last_operation( ) ).

    cut->zif_aff_writer~open_node( node_name = 'open_structure' node_description = cl_abap_typedescr=>describe_by_data( VALUE tadir( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>operation-open_structure act = cut->last_operation( ) ).

    cut->zif_aff_writer~close_node( node_name = 'close_structure' node_description = cl_abap_typedescr=>describe_by_data( VALUE tadir( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>operation-close_structure act = cut->last_operation( ) ).

    cut->zif_aff_writer~open_node( node_name = 'open_table' node_description = cl_abap_typedescr=>describe_by_data( table ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>operation-open_table act = cut->last_operation( ) ).

    cut->zif_aff_writer~close_node( node_name = 'close_table' node_description = cl_abap_typedescr=>describe_by_data( table ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>operation-close_table act = cut->last_operation( ) ).
  ENDMETHOD.

  METHOD get_element_description.
    result = CAST #( cl_abap_typedescr=>describe_by_data( data ) ).
  ENDMETHOD.

  METHOD append_to_previous_line.
    cut->append_to_previous_line( '1+2' ).
    cut->append_to_previous_line( '=' ).
    cut->append_to_previous_line( '2+1' ).

    cut->write_tag( 'new line' ).
    cut->append_to_previous_line( ';' ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( cut->content ) ).
    cl_abap_unit_assert=>assert_equals( exp = '1+2=2+1' act = cut->content[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( exp = 'new line;' act = cut->content[ 2 ] ).
  ENDMETHOD.

  METHOD call_reader_and_decode1.
    DATA(name_of_source) = `ZCL_AFF_TEST_TYPES`.
    DATA(element_name) = `CATEGORY`.
    DATA(abap_doc_act) = cut->call_reader_and_decode( name_of_source = name_of_source element_name = element_name ).
    DATA abap_doc_exp TYPE zcl_aff_abap_doc_parser=>abap_doc.
    abap_doc_exp = VALUE #( description = `This is an enum` title = `myCategory` enumvalues_link = `zcl_aff_test_types.data:enum_values` ).
    cl_abap_unit_assert=>assert_equals( exp = abap_doc_exp act = abap_doc_act ).
  ENDMETHOD.

  METHOD call_reader_and_decode2.
    DATA(name_of_source) = `ZCL_AFF_TEST_TYPES`.
    DATA(element_name) = `AFF_TEST_TYPE-INNER_STRUC`.
    DATA(abap_doc_act) = cut->call_reader_and_decode( name_of_source = name_of_source element_name = element_name ).
    DATA abap_doc_exp TYPE zcl_aff_abap_doc_parser=>abap_doc.
    abap_doc_exp = VALUE #( title = 'Title of inner_struc' description = 'Description of inner_struc' showalways = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = abap_doc_exp act = abap_doc_act ).
  ENDMETHOD.

  METHOD call_reader_and_decode3.
    DATA(name_of_source) = `ZCL_AFF_TEST_TYPES`.
    DATA(element_name) = `AFF_TEST_TYPE-INNER_STRUC-INNER_ELEMENT`.
    DATA(abap_doc_act) = cut->call_reader_and_decode( name_of_source = name_of_source element_name = element_name ).
    DATA abap_doc_exp TYPE zcl_aff_abap_doc_parser=>abap_doc.
    abap_doc_exp = VALUE #( required = abap_true title = `Title of inner_element` description = `Description of inner_element` ).
    cl_abap_unit_assert=>assert_equals( exp = abap_doc_exp act = abap_doc_act ).
  ENDMETHOD.

  METHOD call_reader_and_decode4.
    DATA(name_of_source) = `ZCL_AFF_TEST_TYPES`.
    DATA(element_name) = `STRUCTURE_WITH_NUMBERS-PACKED_WITH_MULTIPLE`.
    DATA(abap_doc_act) = cut->call_reader_and_decode( name_of_source = name_of_source element_name = element_name ).
    DATA abap_doc_exp TYPE zcl_aff_abap_doc_parser=>abap_doc.
    abap_doc_exp = VALUE #( title = 'Packed Number With Given Multiple' description = `Packed number with given multiple` exclusive_minimum = `0` maximum = `99999.90` multiple_of = `0.1` ).
    cl_abap_unit_assert=>assert_equals( exp = abap_doc_exp act = abap_doc_act ).
  ENDMETHOD.

  METHOD call_reader_and_decode5.
    DATA(name_of_source) = `ZCL_AFF_TEST_TYPES`.
    DATA(element_name) = `STRUCTURE_DIFFERENT_DEFAULT-FOUR_BYTE_INT`.
    DATA(abap_doc_act) = cut->call_reader_and_decode( name_of_source = name_of_source element_name = element_name ).
    DATA abap_doc_exp TYPE zcl_aff_abap_doc_parser=>abap_doc.
    abap_doc_exp = VALUE #( title = 'Four Byte Integer' description = 'Four byte integer'  default = '"5"' ).
    cl_abap_unit_assert=>assert_equals( exp = abap_doc_exp act = abap_doc_act ).
  ENDMETHOD.

  METHOD call_reader_and_decode6.
    DATA(name_of_source) = `ZCL_AFF_TEST_TYPES`.
    DATA(element_name) = `STRUCTURE_DIFFERENT_DEFAULT-ENUM_TYPE`.
    DATA(abap_doc_act) = cut->call_reader_and_decode( name_of_source = name_of_source element_name = element_name ).
    DATA abap_doc_exp TYPE zcl_aff_abap_doc_parser=>abap_doc.
    abap_doc_exp = VALUE #( title = 'Enum Type' description = 'Enum type' enumvalues_link = 'zcl_aff_test_types.data:co_class_category' default = '@link zcl_aff_test_types.data:co_class_category.exit_class' ).
    cl_abap_unit_assert=>assert_equals( exp = abap_doc_exp act = abap_doc_act ).
  ENDMETHOD.

  METHOD call_reader_and_decode7.
    DATA(name_of_source) = `ZCL_AFF_TEST_TYPES`.
    DATA(element_name) = `MY_STRUCTURE-MY_FIRST_ELEMENT`.
    DATA(abap_doc_act) = cut->call_reader_and_decode( name_of_source = name_of_source element_name = element_name ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'First Element'
      act = abap_doc_act-title ).
  ENDMETHOD.

  METHOD delete_first_of_struc_stack.
    DATA exp_table LIKE cut->stack_of_structure.
    INSERT VALUE #( name = `MY_STRUCTURE3` absolute_name = `\CLASS=ZCL_AFF_TEST_TYPES\TYPE=MY_STRUCTURE3` ) INTO cut->stack_of_structure INDEX 1.
    INSERT VALUE #( name = `MY_STRUCTURE3` absolute_name = `\CLASS=ZCL_AFF_TEST_TYPES\TYPE=MY_STRUCTURE3` ) INTO exp_table INDEX 1.
    INSERT VALUE #( name = `MY_NESTED_STRUCTURE` absolute_name = `\CLASS=ZCL_AFF_TEST_TYPES\TYPE=MY_NESTED_STRUCTURE` ) INTO cut->stack_of_structure INDEX 1.
    cut->delete_first_of_struc_stack( ).
    cl_abap_unit_assert=>assert_equals( exp = exp_table act = cut->stack_of_structure ).
  ENDMETHOD.

  METHOD get_all_path_information.
    INSERT VALUE #( name = `LIST` absolute_name = `\CLASS=ZCL_AFF_TEST_TYPES\TYPE=LIST` ) INTO cut->stack_of_structure INDEX 1.
    INSERT VALUE #( name = `LIST1` absolute_name = `\TYPE=%_T00004S00000109O0000013408` ) INTO cut->stack_of_structure INDEX 1.
    cut->get_all_path_information(
      EXPORTING
        name             = `ELEMENT_OF_LIST1`
      IMPORTING
        source_type      = DATA(source_type_act)
        source           = DATA(source_act)
        fullname_of_type = DATA(full_name_of_type_act) ).
    DATA(source_type_exp) = `CLASS`.
    DATA(source_exp) = `ZCL_AFF_TEST_TYPES`.
    DATA(full_name_of_type_exp) = `LIST-LIST1-ELEMENT_OF_LIST1`.
    cl_abap_unit_assert=>assert_equals( exp = source_type_exp act = source_type_act ).
    cl_abap_unit_assert=>assert_equals( exp = source_exp act = source_act ).
    cl_abap_unit_assert=>assert_equals( exp = full_name_of_type_exp act = full_name_of_type_act ).
  ENDMETHOD.

  METHOD get_abap_doc_for_absolute_name.
    DATA(absolute_name) = `\CLASS=ZCL_AFF_TEST_TYPES\TYPE=MY_STRUCTURE`.
    DATA(act_abap_doc) = cut->get_abap_doc_for_absolute_name( absolute_name = CONV #( absolute_name ) ).
    DATA exp_abap_doc TYPE zcl_aff_abap_doc_parser=>abap_doc.
    exp_abap_doc = VALUE #( title = `mySimpleStructure` description = `This is a simple structure` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
  ENDMETHOD.

  METHOD compare_abap_doc.
    DATA abap_doc_base TYPE zcl_aff_abap_doc_parser=>abap_doc.
    DATA abap_doc_additional TYPE zcl_aff_abap_doc_parser=>abap_doc.
    DATA exp_abap_doc TYPE zcl_aff_abap_doc_parser=>abap_doc.

    abap_doc_base = VALUE #( title = `This is the title` required = abap_true ).
    abap_doc_additional = VALUE #( description = `This should not be written` enumvalues_link = `This link should be written` ).
    exp_abap_doc = VALUE #( title = `This is the title` required = abap_true enumvalues_link = `This link should be written` ).
    cut->compare_abap_doc(
      EXPORTING
        abap_doc_additional = abap_doc_additional
      CHANGING
        abap_doc_base       = abap_doc_base ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = abap_doc_base ).

    abap_doc_base = VALUE #( enumvalues_link = 'This is a link' ).
    abap_doc_additional = VALUE #( title = `This is the new found title` description = `This is the new found description` enumvalues_link = `This link should not be written` ).
    exp_abap_doc = VALUE #( title = `This is the new found title` description = `This is the new found description` enumvalues_link = `This is a link` ).

    cut->compare_abap_doc(
      EXPORTING
        abap_doc_additional = abap_doc_additional
      CHANGING
        abap_doc_base       = abap_doc_base ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = abap_doc_base ).

  ENDMETHOD.

  METHOD get_struc_of_enum_values_cl.
    DATA(enumvalues_link) = `zcl_aff_test_types.data:enum_values`.
    cut->get_structure_of_enum_values(
      EXPORTING
        link_to_values      = enumvalues_link
        fullname_of_type    = ``
      IMPORTING
        structure_of_values = DATA(act_structure_of_values)
        name_of_source      = DATA(act_name_of_source)
        name_of_constant    = DATA(act_name_of_constant) ).
    DATA(exp_name_of_source) = `ZCL_AFF_TEST_TYPES`.
    DATA(exp_name_of_constant) = `ENUM_VALUES`.
    cl_abap_unit_assert=>assert_equals( exp = exp_name_of_constant act = act_name_of_constant ).
    cl_abap_unit_assert=>assert_equals( exp = exp_name_of_source act = act_name_of_source ).

    DATA exp_structure TYPE REF TO cl_abap_structdescr.
    DATA(ref) = zcl_aff_test_types=>enum_values.
    exp_structure = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( ref ) ).
    cl_abap_unit_assert=>assert_equals( exp = exp_structure->get_components( ) act = act_structure_of_values->get_components( ) ).
    cl_abap_unit_assert=>assert_equals( exp = exp_structure->kind act = act_structure_of_values->kind ).
    cl_abap_unit_assert=>assert_equals( exp = exp_structure->type_kind act = act_structure_of_values->type_kind ).
    cl_abap_unit_assert=>assert_equals( exp = exp_structure->struct_kind act = act_structure_of_values->struct_kind ).
  ENDMETHOD.


  METHOD callback_class_is_valid.
    DATA(class_name) = `ZCL_AFF_TEST_TYPES`.
    DATA(is_valid) = cut->is_callback_class_valid( class_name = class_name component_name = 'Component Name' ).
    DATA(log) = cut->zif_aff_writer~get_log( ).
    cl_abap_unit_assert=>assert_equals( exp = abap_true act = is_valid ).
    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = zif_aff_log=>c_message_type-info ).
  ENDMETHOD.


  METHOD callback_class_is_invalid.
    DATA(class_name) = `ZCL_AFF_WRITER`.
    DATA(is_valid) = cut->is_callback_class_valid( class_name = class_name component_name = 'Component Name' ).
    DATA(log) = cut->zif_aff_writer~get_log( ).
    cl_abap_unit_assert=>assert_equals( exp = abap_false act = is_valid ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = zif_aff_log=>co_msg106
                                                              exp_component_name = `Component Name`
                                                              exp_type           = zif_aff_log=>c_message_type-warning ).
  ENDMETHOD.

  METHOD validate_default.
    TYPES:
      BEGIN OF descr_default_valid_tuple,
        element_description TYPE REF TO cl_abap_elemdescr,
        default             TYPE string,
        is_valid            TYPE abap_boolean,
      END OF descr_default_valid_tuple.
    DATA table_of_tuples TYPE STANDARD TABLE OF descr_default_valid_tuple WITH EMPTY KEY.

    DATA date_time TYPE utclong.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( date_time ) )
                    default             = '9999-12-31T23:59:59.9999999'
                    is_valid            = abap_false ) INTO TABLE table_of_tuples.

    DATA time TYPE t.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( time ) )
                    default             = '20'
                    is_valid            = abap_true ) INTO TABLE table_of_tuples.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( time ) )
                    default             = '201500'
                    is_valid            = abap_true ) INTO TABLE table_of_tuples.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( time ) )
                     default             = '20150045'
                     is_valid            = abap_false ) INTO TABLE table_of_tuples.

    DATA date TYPE d.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( date ) )
                    default             = '20121201'
                    is_valid            = abap_true ) INTO TABLE table_of_tuples.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( date ) )
                    default             = '201212214'
                    is_valid            = abap_false ) INTO TABLE table_of_tuples.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( date ) )
                    default             = '201212'
                    is_valid            = abap_true ) INTO TABLE table_of_tuples.

    DATA numeric TYPE n LENGTH 3.
    numeric = '20'.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( numeric ) )
                    default             = '201'
                    is_valid            = abap_true ) INTO TABLE table_of_tuples.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( numeric ) )
                    default             = '20'
                    is_valid            = abap_true ) INTO TABLE table_of_tuples.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( numeric ) )
                    default             = '2021'
                    is_valid            = abap_false ) INTO TABLE table_of_tuples.

    DATA float TYPE f.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( float ) )
                    default             = '14.5e12'
                    is_valid            = abap_true ) INTO TABLE table_of_tuples.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( float ) )
                    default             = 'no float'
                    is_valid            = abap_false ) INTO TABLE table_of_tuples.

    DATA decfloat TYPE decfloat16.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( decfloat ) )
                     default             = '14.5e12'
                     is_valid            = abap_true ) INTO TABLE table_of_tuples.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( decfloat ) )
                     default             = 'no_float'
                     is_valid            = abap_false ) INTO TABLE table_of_tuples.

    DATA packed TYPE p LENGTH 3 DECIMALS 2.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( packed ) )
                    default             = 'a'
                    is_valid            = abap_false ) INTO TABLE table_of_tuples.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( packed ) )
                    default             = '4.534'
                    is_valid            = abap_false ) INTO TABLE table_of_tuples.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( packed ) )
                    default             = '3.25'
                    is_valid            = abap_true ) INTO TABLE table_of_tuples.

    DATA char TYPE c LENGTH 4.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( char ) )
                      default = 'a5b'
                      is_valid = abap_true ) INTO TABLE table_of_tuples.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( char ) )
                    default             = 'a5bca'
                    is_valid            = abap_false ) INTO TABLE table_of_tuples.

    DATA boolean TYPE abap_boolean.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( boolean ) )
                    default             = 'abap_true'
                    is_valid            = abap_true ) INTO TABLE table_of_tuples.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( boolean ) )
                    default             = 'abap_false'
                    is_valid            = abap_true ) INTO TABLE table_of_tuples.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( boolean ) )
                    default             = ''
                    is_valid            = abap_true ) INTO TABLE table_of_tuples.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( boolean ) )
                    default             = 'a'
                    is_valid            = abap_false ) INTO TABLE table_of_tuples.

    DATA integer1 TYPE int1.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( integer1 ) )
                    default             = '-5'
                    is_valid            = abap_false ) INTO TABLE table_of_tuples.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( integer1 ) )
                    default             = '10'
                    is_valid            = abap_true ) INTO TABLE table_of_tuples.

    DATA integer TYPE i.
    INSERT VALUE #( element_description = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( integer ) )
                    default             = '-5'
                    is_valid            = abap_true ) INTO TABLE table_of_tuples.

    LOOP AT table_of_tuples ASSIGNING FIELD-SYMBOL(<entry>).
      DATA(is_valid) = cut->is_default_value_valid( element_description = <entry>-element_description
                                                    default_value       = <entry>-default
                                                    fullname_of_type    = `TEST_VALUE` ).
      cl_abap_unit_assert=>assert_equals( exp = <entry>-is_valid act = is_valid ).
    ENDLOOP.
  ENDMETHOD.

  METHOD validate_source.
    cl_abap_unit_assert=>assert_true( cut->zif_aff_writer~validate( source = VALUE #( ) log = VALUE #( ) ) ).
  ENDMETHOD.

  METHOD handle_include.
    DATA data TYPE zcl_aff_test_types=>ty_include_type.
    cut->zif_aff_writer~open_include( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( data ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( cut->stack_of_structure ) ).
    cut->zif_aff_writer~close_include( ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( cut->stack_of_structure ) ).
  ENDMETHOD.


ENDCLASS.

class ltcl_type_writer definition deferred.

class ltcl_writer_testable definition final for testing
  duration short risk level harmless
  inheriting from zcl_aff_writer
  friends ltcl_type_writer.
  protected section.
    methods:
      write_element redefinition,
      open_structure redefinition,
      close_structure redefinition,
      open_table redefinition,
      write_tag redefinition,
      close_table redefinition.
endclass.

class ltcl_writer_testable implementation.
  method write_element ##NEEDED.
  endmethod.
  method close_structure ##NEEDED.
  endmethod.
  method close_table ##NEEDED.
  endmethod.
  method open_structure ##NEEDED.
  endmethod.
  method open_table ##NEEDED.
  endmethod.
  method write_tag.
    append |{ repeat( val = ` `  occ = indent_level * c_indent_number_characters ) }{ line }| to content.
  endmethod.

endclass.

class ltcl_type_writer definition final for testing
  duration short
  risk level harmless.

  private section.
    data:
      cut type ref to ltcl_writer_testable.

    methods: camel_case for testing raising cx_static_check,
      no_formatting_option for testing raising cx_static_check,
      name_mapping_found for testing raising cx_static_check,
      name_mapping_not_found for testing raising cx_static_check,
      name_mapping_and_camel_case for testing raising cx_static_check,
      get_output for testing raising cx_static_check,
      value_mapping_found for testing raising cx_static_check,
      value_mapping_not_found for testing raising cx_static_check,
      get_type_info_string_like for testing raising cx_static_check,
      get_type_info_boolean for testing raising cx_static_check,
      get_type_info_numeric for testing raising cx_static_check,
      get_type_info_date_time for testing raising cx_static_check,
      set_name_mappings for testing raising cx_static_check,
      set_abap_value_mappings for testing raising cx_static_check,
      set_formatting_option for testing raising cx_static_check,
      stack_stores_operations for testing raising cx_static_check,
      append_to_previous_line for testing raising cx_static_check,
      call_reader_and_decode for testing raising cx_static_check,
      delete_first_of_struc_stack for testing raising cx_static_check,
      get_all_path_information for testing raising cx_static_check,
      get_abap_doc_for_absolute_name for testing raising cx_static_check,
      compare_abap_doc for testing raising cx_static_check,
      callback_class_is_valid for testing raising cx_static_check,
      callback_class_is_invalid for testing raising cx_static_check,
      validate_default for testing raising cx_static_check,
      validate_source for testing raising cx_static_check,
      get_struc_of_enum_values_cl for testing raising cx_static_check,
      handle_include for testing raising cx_static_check,
      setup.

    methods: get_element_description
      importing data          type data
      returning value(result) type ref to cl_abap_elemdescr.
endclass.


class ltcl_type_writer implementation.

  method setup.
    cut = new ltcl_writer_testable( ).
  endmethod.

  method set_name_mappings.
    data(name_mappings) = value zif_aff_writer=>ty_name_mappings(
      ( abap = 'ABAP_ELEMENT' json = 'JSON_ELEMENT' )
    ).

    cut->zif_aff_writer~set_name_mappings( name_mappings ).

    cl_abap_unit_assert=>assert_equals( exp = name_mappings act = cut->name_mappings ).
  endmethod.

  method set_abap_value_mappings.
    data(abap_value_mappings) = value zif_aff_writer=>ty_abap_value_mappings(
      ( abap_element = 'ABAP_ELEMENT' value_mappings = value #( ( abap = '1' json = '2' ) ) )
    ).

    cut->zif_aff_writer~set_abap_value_mappings( abap_value_mappings ).

    cl_abap_unit_assert=>assert_equals( exp = abap_value_mappings act = cut->abap_value_mappings ).
  endmethod.

  method set_formatting_option.
    cut->zif_aff_writer~set_formatting_option( zif_aff_writer=>formatting_option-camel_case ).

    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>formatting_option-camel_case act = cut->formatting_option ).
  endmethod.

  method get_output.
    data(exp_output) = value rswsourcet( ( `line1` ) ( `line2` ) ).
    cut->output = exp_output.

    data(act_output) = cut->zif_aff_writer~get_output( ).

    cl_abap_unit_assert=>assert_equals( exp = exp_output act = act_output ).
  endmethod.

  method camel_case.
    cut->formatting_option = zif_aff_writer=>formatting_option-camel_case.

    data(act_name) = cut->map_and_format_name( 'MY_TEst_nAmE' ).

    cl_abap_unit_assert=>assert_equals( act = act_name exp = 'myTestName' msg = |Actual was { act_name }, but expected is 'myTestName'| ).
  endmethod.

  method no_formatting_option.
    cut->formatting_option = zif_aff_writer=>formatting_option-no_formatting.

    data(act_name) = cut->map_and_format_name( 'MY_TEst_nAmE' ).

    cl_abap_unit_assert=>assert_equals( act = act_name exp = 'MY_TEst_nAmE' msg = |Actual was { act_name }, but expected is 'MY_TEst_nAmE'| ).
  endmethod.

  method name_mapping_found.
    cut->name_mappings = value #(
      ( abap = 'my_test_name_abap'   json = 'myTestNameJson' )
      ( abap = 'MY_TEST_NAME_2_ABAP' json = 'myTestName2Json' ) ).

    data(act_name) = cut->map_and_format_name( 'MY_TEST_NAME_abap' ).

    cl_abap_unit_assert=>assert_equals( act = act_name exp = 'myTestNameJson' msg = |Actual was { act_name }, but expected is 'myTestNameJson'| ).
  endmethod.

  method name_mapping_not_found.
    cut->name_mappings = value #( ( abap = 'MY_TEST_NAME_ABAP'   json = 'myTestNameJson' ) ).

    data(act_name) = cut->map_and_format_name( 'NON_EXISTING_IN_MAPPING' ).

    cl_abap_unit_assert=>assert_equals( act = act_name exp = 'NON_EXISTING_IN_MAPPING' msg = |Actual was { act_name }, but expected is 'NON_EXISTING_IN_MAPPING'| ).
  endmethod.

  method name_mapping_and_camel_case.
    cut->formatting_option = zif_aff_writer=>formatting_option-camel_case.
    cut->name_mappings = value #( ( abap = 'MY_TEST_NAME_ABAP' json = 'my_test_name_json' ) ).

    data(act_name) = cut->map_and_format_name( 'MY_TEST_NAME_ABAP' ).

    cl_abap_unit_assert=>assert_equals( act = act_name exp = 'my_test_name_json' msg = |Actual was { act_name }, but expected is 'my_test_name_json'| ).
  endmethod.

  method value_mapping_found.
    data(value_mappings) = value zif_aff_writer=>ty_value_mappings( ( abap = 'X' json = 'true' ) ( abap = ' ' json = 'false' ) ).
    data(abap_value_mapping_1) = value zif_aff_writer=>ty_abap_value_mapping(
      abap_element   = 'MY_TEST_ELEMENT_1'
      target_type    = zif_aff_writer=>type_info-boolean
      value_mappings = value_mappings ).
    data(abap_value_mapping_2) = value zif_aff_writer=>ty_abap_value_mapping(
      abap_element   = 'my_test_element_2'
      target_type    = zif_aff_writer=>type_info-string ).

    cut->abap_value_mappings = value #( ( abap_value_mapping_1 ) ( abap_value_mapping_2 ) ).

    data(act_abap_value_mapping) = cut->get_value_mapping_for_element( 'MY_TEST_element_1' ).

    cl_abap_unit_assert=>assert_equals( exp = abap_value_mapping_1 act = act_abap_value_mapping ).
  endmethod.

  method value_mapping_not_found.
    data(exp_value_mappings) = value zif_aff_writer=>ty_value_mappings( ( abap = 'X' json = 'true' ) ( abap = ' ' json = 'false' ) ).
    cut->abap_value_mappings = value #( ( abap_element = 'MY_TEST_ELEMENT_2' value_mappings = exp_value_mappings ) ).

    data(act_value_mappings) = cut->get_value_mapping_for_element( 'MY_TEST_ELEMENT' ).

    cl_abap_unit_assert=>assert_initial( act_value_mappings ).
  endmethod.

  method get_type_info_string_like.
    data char type c length 1.
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-string act = cut->get_json_type_from_description( get_element_description( char ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-string act = cut->get_json_type_from_description( get_element_description( value string( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-string act = cut->get_json_type_from_description( get_element_description( value char01( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-string act = cut->get_json_type_from_description( get_element_description( value xstring( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-string act = cut->get_json_type_from_description( get_element_description( value zif_aff_writer=>enum_type_info( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-string act = cut->get_json_type_from_description( get_element_description( value numc04( ) ) ) ).
  endmethod.

  method get_type_info_boolean.
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-boolean act = cut->get_json_type_from_description( get_element_description( value abap_boolean( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-boolean act = cut->get_json_type_from_description( get_element_description( value abap_bool( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-boolean act = cut->get_json_type_from_description( get_element_description( value flag( ) ) ) ).
  endmethod.

  method get_type_info_numeric.
    data packed type p.
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-numeric act = cut->get_json_type_from_description( get_element_description( packed ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-numeric act = cut->get_json_type_from_description( get_element_description( value f( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-numeric act = cut->get_json_type_from_description( get_element_description( value int1( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-numeric act = cut->get_json_type_from_description( get_element_description( value int2( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-numeric act = cut->get_json_type_from_description( get_element_description( value int8( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-numeric act = cut->get_json_type_from_description( get_element_description( value decfloat16( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-numeric act = cut->get_json_type_from_description( get_element_description( value decfloat34( ) ) ) ).
  endmethod.

  method get_type_info_date_time.
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-date_time act = cut->get_json_type_from_description( get_element_description( value d( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-date_time act = cut->get_json_type_from_description( get_element_description( value t( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-date_time act = cut->get_json_type_from_description( get_element_description( value timestamp( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-date_time act = cut->get_json_type_from_description( get_element_description( value timestampl( ) ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>type_info-date_time act = cut->get_json_type_from_description( get_element_description( value utclong( ) ) ) ).
  endmethod.

  method stack_stores_operations.
    data table type standard table of tadir.

    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>operation-initial act = cut->last_operation( ) ).

    cut->zif_aff_writer~write_element( element_name = 'write_element' element_description = value #( ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>operation-write_element act = cut->last_operation( ) ).

    cut->zif_aff_writer~open_node( node_name = 'open_structure' node_description = cl_abap_typedescr=>describe_by_data( value tadir( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>operation-open_structure act = cut->last_operation( ) ).

    cut->zif_aff_writer~close_node( node_name = 'close_structure' node_description = cl_abap_typedescr=>describe_by_data( value tadir( ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>operation-close_structure act = cut->last_operation( ) ).

    cut->zif_aff_writer~open_node( node_name = 'open_table' node_description = cl_abap_typedescr=>describe_by_data( table ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>operation-open_table act = cut->last_operation( ) ).

    cut->zif_aff_writer~close_node( node_name = 'close_table' node_description = cl_abap_typedescr=>describe_by_data( table ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_aff_writer=>operation-close_table act = cut->last_operation( ) ).
  endmethod.

  method get_element_description.
    result = cast #( cl_abap_typedescr=>describe_by_data( data ) ).
  endmethod.

  method append_to_previous_line.
    cut->append_to_previous_line( '1+2' ).
    cut->append_to_previous_line( '=' ).
    cut->append_to_previous_line( '2+1' ).

    cut->write_tag( 'new line' ).
    cut->append_to_previous_line( ';' ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( cut->content ) ).
    cl_abap_unit_assert=>assert_equals( exp = '1+2=2+1' act = cut->content[ 1 ] ).
    cl_abap_unit_assert=>assert_equals( exp = 'new line;' act = cut->content[ 2 ] ).
  endmethod.

  method call_reader_and_decode.
    data(name_of_source) = `ZCL_AFF_TEST_TYPES`.
    data(element_name) = `CATEGORY`.
    data(abap_doc_act) = cut->call_reader_and_decode( name_of_source =  name_of_source element_name   = element_name ).
    data abap_doc_exp type zcl_aff_abap_doc_parser=>abap_doc.
    abap_doc_exp = value #( description = `This is an enum` title = `myCategory` enumvalues_link = `zcl_aff_test_types.data:enum_values` ).
    cl_abap_unit_assert=>assert_equals( exp = abap_doc_exp act = abap_doc_act ).

    element_name = `AFF_TEST_TYPE-INNER_STRUC`.
    abap_doc_act = cut->call_reader_and_decode( name_of_source =  name_of_source element_name   = element_name ).
    abap_doc_exp = value #( title = 'Title of inner_struc' description = 'Description of inner_struc' showalways = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = abap_doc_exp act = abap_doc_act ).

    element_name = `AFF_TEST_TYPE-INNER_STRUC-INNER_ELEMENT`.
    abap_doc_act = cut->call_reader_and_decode( name_of_source =  name_of_source element_name   = element_name ).
    abap_doc_exp = value #( required = abap_true title = `Title of inner_element` description = `Description of inner_element` ).
    cl_abap_unit_assert=>assert_equals( exp = abap_doc_exp act = abap_doc_act ).

    element_name = `STRUCTURE_WITH_NUMBERS-PACKED_WITH_MULTIPLE`.
    abap_doc_act = cut->call_reader_and_decode( name_of_source =  name_of_source element_name   = element_name ).
    abap_doc_exp = value #( title = 'Packed Number With Given Multiple' description = `Packed number with given multiple` exclusive_minimum = `0` maximum = `99999.90` multiple_of = `0.1` ).
    cl_abap_unit_assert=>assert_equals( exp = abap_doc_exp act = abap_doc_act ).

    element_name = `STRUCTURE_DIFFERENT_DEFAULT-FOUR_BYTE_INT`.
    abap_doc_act = cut->call_reader_and_decode( name_of_source =  name_of_source element_name   = element_name ).
    abap_doc_exp = value #( title = 'Four Byte Integer' description = 'Four byte integer'  default = '"5"' ).
    cl_abap_unit_assert=>assert_equals( exp = abap_doc_exp act = abap_doc_act ).

    element_name = `STRUCTURE_DIFFERENT_DEFAULT-ENUM_TYPE`.
    abap_doc_act = cut->call_reader_and_decode( name_of_source =  name_of_source element_name   = element_name ).
    abap_doc_exp = value #( title = 'Enum Type' description = 'Enum type' enumvalues_link = 'zcl_aff_test_types.data:co_class_category' default = '@link zcl_aff_test_types.data:co_class_category.exit_class' ).
    cl_abap_unit_assert=>assert_equals( exp = abap_doc_exp act = abap_doc_act ).

  endmethod.

  method delete_first_of_struc_stack.
    data exp_table like cut->stack_of_structure.
    insert value #( name = `MY_STRUCTURE3` absolute_name = `\CLASS=ZCL_AFF_TEST_TYPES\TYPE=MY_STRUCTURE3` ) into cut->stack_of_structure index 1.
    insert value #( name = `MY_STRUCTURE3` absolute_name = `\CLASS=ZCL_AFF_TEST_TYPES\TYPE=MY_STRUCTURE3` ) into exp_table index 1.
    insert value #( name = `MY_NESTED_STRUCTURE` absolute_name = `\CLASS=ZCL_AFF_TEST_TYPES\TYPE=MY_NESTED_STRUCTURE` ) into cut->stack_of_structure index 1.
    cut->delete_first_of_struc_stack( ).
    cl_abap_unit_assert=>assert_equals( exp = exp_table act = cut->stack_of_structure ).
  endmethod.

  method get_all_path_information.
    insert value #( name = `LIST` absolute_name = `\CLASS=ZCL_AFF_TEST_TYPES\TYPE=LIST` ) into cut->stack_of_structure index 1.
    insert value #( name = `LIST1` absolute_name = `\TYPE=%_T00004S00000109O0000013408` ) into cut->stack_of_structure index 1.
    cut->get_all_path_information(
      exporting
        name             = `ELEMENT_OF_LIST1`
      importing
        source_type      = data(source_type_act)
        source           = data(source_act)
        fullname_of_type = data(full_name_of_type_act)
    ).
    data(source_type_exp) = `CLASS`.
    data(source_exp) = `ZCL_AFF_TEST_TYPES`.
    data(full_name_of_type_exp) = `LIST-LIST1-ELEMENT_OF_LIST1`.
    cl_abap_unit_assert=>assert_equals( exp = source_type_exp act = source_type_act ).
    cl_abap_unit_assert=>assert_equals( exp = source_exp act = source_act ).
    cl_abap_unit_assert=>assert_equals( exp = full_name_of_type_exp act = full_name_of_type_act ).
  endmethod.

  method get_abap_doc_for_absolute_name.
    data(absolute_name) = `\CLASS=ZCL_AFF_TEST_TYPES\TYPE=MY_STRUCTURE`.
    data(act_abap_doc) = cut->get_abap_doc_for_absolute_name( absolute_name = conv #( absolute_name ) ).
    data exp_abap_doc type cl_aff_abap_doc_parser=>abap_doc.
    exp_abap_doc = value #( title = `mySimpleStructure` description = `This is a simple structure` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
  endmethod.

  method compare_abap_doc.
    data abap_doc_base type cl_aff_abap_doc_parser=>abap_doc.
    data abap_doc_additional type cl_aff_abap_doc_parser=>abap_doc.
    data exp_abap_doc type cl_aff_abap_doc_parser=>abap_doc.

    abap_doc_base = value #( title = `This is the title` required = abap_true ).
    abap_doc_additional = value #( description = `This should not be written` enumvalues_link = `This link should be written` ).
    exp_abap_doc = value #( title = `This is the title` required = abap_true enumvalues_link = `This link should be written` ).
    cut->compare_abap_doc(
      exporting
        abap_doc_additional = abap_doc_additional
      changing
        abap_doc_base       = abap_doc_base
    ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = abap_doc_base ).

    abap_doc_base = value #( enumvalues_link = 'This is a link' ).
    abap_doc_additional = value #( title = `This is the new found title` description = `This is the new found description` enumvalues_link = `This link should not be written` ).
    exp_abap_doc = value #( title = `This is the new found title` description = `This is the new found description` enumvalues_link = `This is a link` ).

    cut->compare_abap_doc(
      exporting
        abap_doc_additional = abap_doc_additional
      changing
        abap_doc_base       = abap_doc_base
    ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = abap_doc_base ).

  endmethod.

  method get_struc_of_enum_values_cl.
    data(enumvalues_link) = `zcl_aff_test_types.data:enum_values`.
    cut->get_structure_of_enum_values(
      exporting
        link_to_values      = enumvalues_link
        fullname_of_type    = ``
      importing
        structure_of_values = data(act_structure_of_values)
        name_of_source      = data(act_name_of_source)
        name_of_constant    = data(act_name_of_constant)
    ).
    data(exp_name_of_source) = `ZCL_AFF_TEST_TYPES`.
    data(exp_name_of_constant) = `ENUM_VALUES`.
    cl_abap_unit_assert=>assert_equals( exp = exp_name_of_constant act = act_name_of_constant ).
    cl_abap_unit_assert=>assert_equals( exp = exp_name_of_source act = act_name_of_source ).

    data exp_structure type ref to cl_abap_structdescr.
    data(ref) = zcl_aff_test_types=>enum_values.
    exp_structure = cast cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( ref ) ).
    cl_abap_unit_assert=>assert_equals( exp = exp_structure->get_components( ) act = act_structure_of_values->get_components( ) ).
    cl_abap_unit_assert=>assert_equals( exp = exp_structure->kind act = act_structure_of_values->kind ).
    cl_abap_unit_assert=>assert_equals( exp = exp_structure->type_kind act = act_structure_of_values->type_kind ).
    cl_abap_unit_assert=>assert_equals( exp = exp_structure->struct_kind act = act_structure_of_values->struct_kind ).
  endmethod.


  method callback_class_is_valid.
    data(class_name) = `ZCL_AFF_TEST_TYPES`.
    data(is_valid) = cut->is_callback_class_valid( class_name = class_name component_name = 'Component Name' ).
    data(log) = cut->zif_aff_writer~get_log( ).
    cl_abap_unit_assert=>assert_equals( exp = abap_true act = is_valid ).
    cl_aff_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = if_aff_log=>c_message_type-info ).
  endmethod.


  method callback_class_is_invalid.
    data(class_name) = `ZCL_AFF_WRITER`.
    data(is_valid) = cut->is_callback_class_valid( class_name = class_name component_name = 'Component Name' ).
    data(log) = cut->zif_aff_writer~get_log( ).
    cl_abap_unit_assert=>assert_equals( exp = abap_false act = is_valid ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 106
                                                                             attr1 = `Component Name` )
                                                      exp_type    = if_aff_log=>c_message_type-warning ).
  endmethod.

  method validate_default.
    types:
      begin of descr_default_valid_tuple,
        element_description type ref to cl_abap_elemdescr,
        default             type string,
        is_valid            type abap_boolean,
      end of descr_default_valid_tuple.
    data table_of_tuples type standard table of descr_default_valid_tuple with empty key.

    data date_time type utclong.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( date_time ) )
                    default             = '9999-12-31T23:59:59.9999999'
                    is_valid            = abap_false ) into table table_of_tuples.

    data time type t.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( time ) )
                    default             = '20'
                    is_valid            = abap_true ) into table table_of_tuples.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( time ) )
                    default             = '201500'
                    is_valid            = abap_true ) into table table_of_tuples.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( time ) )
                     default             = '20150045'
                     is_valid            = abap_false ) into table table_of_tuples.

    data date type d.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( date ) )
                    default             = '20121201'
                    is_valid            = abap_true ) into table table_of_tuples.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( date ) )
                    default             = '201212214'
                    is_valid            = abap_false ) into table table_of_tuples.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( date ) )
                    default             = '201212'
                    is_valid            = abap_true ) into table table_of_tuples.

    data numeric type n length 3.
    numeric = '20'.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( numeric ) )
                    default             = '201'
                    is_valid            = abap_true ) into table table_of_tuples.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( numeric ) )
                    default             = '20'
                    is_valid            = abap_true ) into table table_of_tuples.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( numeric ) )
                    default             = '2021'
                    is_valid            = abap_false ) into table table_of_tuples.

    data float type f.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( float ) )
                    default             = '14.5e12'
                    is_valid            = abap_true ) into table table_of_tuples.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( float ) )
                    default             = 'no float'
                    is_valid            = abap_false ) into table table_of_tuples.

    data decfloat type decfloat16.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( decfloat ) )
                     default             = '14.5e12'
                     is_valid            = abap_true ) into table table_of_tuples.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( decfloat ) )
                     default             = 'no_float'
                     is_valid            = abap_false ) into table table_of_tuples.

    data packed type p length 3 decimals 2.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( packed ) )
                    default             = 'a'
                    is_valid            = abap_false ) into table table_of_tuples.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( packed ) )
                    default             = '4.534'
                    is_valid            = abap_false ) into table table_of_tuples.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( packed ) )
                    default             = '3.25'
                    is_valid            = abap_true ) into table table_of_tuples.

    data char type c length 4.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( char ) )
                      default = 'a5b'
                      is_valid = abap_true ) into table table_of_tuples.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( char ) )
                    default             = 'a5bca'
                    is_valid            = abap_false ) into table table_of_tuples.

    data boolean type abap_boolean.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( boolean ) )
                    default             = 'abap_true'
                    is_valid            = abap_true ) into table table_of_tuples.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( boolean ) )
                    default             = 'abap_false'
                    is_valid            = abap_true ) into table table_of_tuples.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( boolean ) )
                    default             = ''
                    is_valid            = abap_true ) into table table_of_tuples.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( boolean ) )
                    default             = 'a'
                    is_valid            = abap_false ) into table table_of_tuples.

    data integer1 type int1.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( integer1 ) )
                    default             = '-5'
                    is_valid            = abap_false ) into table table_of_tuples.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( integer1 ) )
                    default             = '10'
                    is_valid            = abap_true ) into table table_of_tuples.

    data integer type i.
    insert value #( element_description = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( integer ) )
                    default             = '-5'
                    is_valid            = abap_true ) into table table_of_tuples.

    loop at table_of_tuples assigning field-symbol(<entry>).
      data(is_valid) = cut->is_default_value_valid( element_description = <entry>-element_description
                                                    default_value       = <entry>-default
                                                    fullname_of_type    = `TEST_VALUE` ).
      cl_abap_unit_assert=>assert_equals( exp = <entry>-is_valid act = is_valid ).
    endloop.
  endmethod.

  method validate_source.
    cl_abap_unit_assert=>assert_true( cut->zif_aff_writer~validate( source = value #( ) log = value #( ) ) ).
  endmethod.

  method handle_include.
    data data type zcl_aff_test_types=>ty_include_type.
    cut->zif_aff_writer~open_include( cast cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( data ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( cut->stack_of_structure ) ).
    cut->zif_aff_writer~close_include( ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( cut->stack_of_structure ) ).
  endmethod.


endclass.

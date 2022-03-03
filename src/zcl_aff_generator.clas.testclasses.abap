*interface lif_test_types.
*  types:
*    element type string.
*
*  types:
*    begin of structure,
*      element_1 type i,
*      element_2 type element,
*    end of structure.
*
*  types:
*    begin of include,
*      include_element_1 type string,
*      include_element_2 type i,
*    end of include.
*
*  types:
*    begin of structure_with_include.
*      include type include.
*  types element_1 type i.
*  types element_2 type element.
*  types end of structure_with_include.
*
*  types:
*    begin of include_in_include.
*      include type include.
*types end of include_in_include.
*
*  types:
*    begin of structure_include_in_include.
*      include type include_in_include.
*  types element type string.
*  types end of structure_include_in_include.
*
*  types:
*    begin of structure_in_structure,
*      structure type structure,
*      element   type element,
*    end of structure_in_structure.
*
*  types:
*    table_structure type standard table of structure with default key.
*
*  types:
*    table_build_in_type type standard table of string with default key.
*
*  types:
*    begin of structure_with_table,
*      table type table_structure,
*    end of structure_with_table.
*
*  types:
*    begin of include_table.
*      include type structure_with_table.
*  types include_element_1 type i.
*  types end of include_table.
*
*  types:
*    table_in_table type standard table of table_build_in_type with default key.
*
*  types:
*    begin of nested_table,
*      second_table type table_build_in_type,
*    end of nested_table,
*    first_table_type type standard table of nested_table with default key,
*    begin of struc_tab_struc_tab,
*      first_table type first_table_type,
*    end of struc_tab_struc_tab.
*
*  types:
*    begin of ty_component,
*      name        type seocmpname,
*      description type seodescr,
*    end of ty_component,
*    ty_components    type sorted table of ty_component with unique key name,
*    ty_subcomponents type sorted table of ty_component with unique key name,
*    begin of ty_method,
*      name        type seocmpname,
*      description type seodescr,
*      parameters  type ty_subcomponents,
*      exceptions  type ty_subcomponents,
*    end of ty_method,
*    ty_methods type sorted table of ty_method with unique key name,
*    begin of ty_event,
*      name        type seocmpname,
*      description type seodescr,
*      parameters  type ty_subcomponents,
*    end of ty_event,
*    ty_events type sorted table of ty_event with unique key name,
*    begin of ty_clif_properties,
*      attributes type ty_components,
*      methods    type ty_methods,
*      events     type ty_events,
*      types      type ty_components,
*    end of ty_clif_properties.
*
*  types:
*    begin of ty_class_properties,
*      format_version type if_aff_types_v1=>ty_format_version,
*      header         type if_aff_types_v1=>ty_header_60_src,
*      category       type vseoclass-category,
*      fixpt          type vseoclass-fixpt,
*      msg_id         type vseoclass-msg_id.
*      include type ty_clif_properties.
*types end of ty_class_properties.
*
*  types:
*    begin of ty_header,
*      description type c length 30,
*    end of ty_header.
*  types:
*    begin of ty_abap_type,
*      format_version  type string,
*      header          type ty_header,
*      other_component type i,
*    end of ty_abap_type.
*  types:
*    begin of ty_abap_type_no_header,
*      format_version  type string,
*      other_component type i,
*    end of ty_abap_type_no_header.
*  types:
*    begin of ty_abap_type_no_format,
*      header          type ty_header,
*      other_component type i,
*    end of ty_abap_type_no_format.
*
*endinterface.
*
*class lcl_unit_test_writer definition create public for testing inheriting from cl_aff_type_writer final.
*
*  public section.
*  protected section.
*    methods:
*      write_element redefinition,
*      open_structure redefinition,
*      close_structure redefinition,
*      open_table redefinition,
*      write_tag redefinition,
*      close_table redefinition.
*
*  private section.
*    data:
*      depth          type i value 0.
*
*endclass.
*
*class lcl_unit_test_writer implementation.
*
*  method write_element.
*    append |{ repeat( val = ` `  occ = 4 * depth ) }{ element_name } : { element_description->type_kind }| to output.
*  endmethod.
*
*  method close_structure.
*    append |{ repeat( val = ` `  occ = 4 * ( depth - 1 ) ) }CLOSE_STRUCTURE { structure_name }| to output.
*    depth -= 1.
*  endmethod.
*
*  method close_table.
*    append |{ repeat( val = ` `  occ = 4 * ( depth - 1 ) ) }CLOSE_TABLE { table_name }| to output.
*    depth -= 1.
*  endmethod.
*
*  method open_structure.
*    append |{ repeat( val = ` `  occ = 4 * depth ) }OPEN_STRUCTURE { structure_name }| to output.
*    depth += 1.
*  endmethod.
*
*  method open_table.
*    append |{ repeat( val = ` `  occ = 4 * depth ) }OPEN_TABLE { table_name }| to output.
*    depth += 1.
*  endmethod.
*
*  method write_tag ##NEEDED.
*  endmethod.
*
*
*endclass.
*
*class ltcl_type_generator definition final for testing
*  duration short
*  risk level harmless.
*
*  public section.
*    interfaces lif_test_types.
*
*  private section.
*    data:
*      cut        type ref to zcl_aff_generator,
*      exp_result type rswsourcet.
*
*    methods:
*      element for testing raising cx_static_check,
*      structure for testing raising cx_static_check,
*      include for testing raising cx_static_check,
*      table_build_in_type for testing raising cx_static_check,
*      include_in_include for testing raising cx_static_check,
*      structure_in_structure for testing raising cx_static_check,
*      table_structure for testing raising cx_static_check,
*      structure_with_table for testing raising cx_static_check,
*      include_table for testing raising cx_static_check,
*      table_in_table for testing raising cx_static_check,
*      struc_tab_struc_tab for testing raising cx_static_check,
*      unsupported_type for testing raising cx_static_check,
*      complex_structure_aff_class for testing raising cx_static_check,
*      mandatory_fields for testing raising cx_static_check,
*      no_header for testing raising cx_static_check,
*      no_format_version for testing raising cx_static_check,
*      no_structure for testing raising cx_static_check,
*      setup,
*      assert_output_equals
*        importing
*          act type rswsourcet
*          exp type rswsourcet.
*endclass.
*
*class zcl_aff_generator definition local friends ltcl_type_generator.
*
*class ltcl_type_generator implementation.
*
*  method setup.
*    cut = new zcl_aff_generator( new lcl_unit_test_writer( ) ).
*  endmethod.
*
*  method element.
*    data test_data type lif_test_types=>element.
*    data(act_result) = cut->generate_type( test_data ).
*
*    exp_result = value #( ( `ELEMENT : g` ) ).
*    assert_output_equals( exp = exp_result act = act_result ).
*  endmethod.
*
*  method structure.
*    data test_data type lif_test_types=>structure.
*    data(act_result) = cut->generate_type( test_data ).
*
*    exp_result = value #(
*      ( `OPEN_STRUCTURE STRUCTURE` )
*      ( `    ELEMENT_1 : I` )
*      ( `    ELEMENT_2 : g` )
*      ( `CLOSE_STRUCTURE STRUCTURE` )
*    ).
*    assert_output_equals( exp = exp_result act = act_result ).
*  endmethod.
*
*  method include.
*    data test_data type lif_test_types=>structure_with_include.
*    data(act_result) = cut->generate_type( test_data ).
*
*    exp_result = value #(
*      ( `OPEN_STRUCTURE STRUCTURE_WITH_INCLUDE` )
*      ( `    INCLUDE_ELEMENT_1 : g` )
*      ( `    INCLUDE_ELEMENT_2 : I` )
*      ( `    ELEMENT_1 : I` )
*      ( `    ELEMENT_2 : g` )
*      ( `CLOSE_STRUCTURE STRUCTURE_WITH_INCLUDE` )
*    ).
*    assert_output_equals( exp = exp_result act = act_result ).
*  endmethod.
*
*  method include_in_include.
*    data test_data type lif_test_types=>structure_include_in_include.
*    data(act_result) = cut->generate_type( test_data ).
*
*    exp_result = value #(
*      ( `OPEN_STRUCTURE STRUCTURE_INCLUDE_IN_INCLUDE` )
*      ( `    INCLUDE_ELEMENT_1 : g` )
*      ( `    INCLUDE_ELEMENT_2 : I` )
*      ( `    ELEMENT : g` )
*      ( `CLOSE_STRUCTURE STRUCTURE_INCLUDE_IN_INCLUDE` )
*    ).
*    assert_output_equals( exp = exp_result act = act_result ).
*  endmethod.
*
*  method structure_in_structure.
*    data test_data type lif_test_types=>structure_in_structure.
*    data(act_result) = cut->generate_type( test_data ).
*
*    exp_result = value #(
*      ( `OPEN_STRUCTURE STRUCTURE_IN_STRUCTURE` )
*      ( `    OPEN_STRUCTURE STRUCTURE` )
*      ( `        ELEMENT_1 : I` )
*      ( `        ELEMENT_2 : g` )
*      ( `    CLOSE_STRUCTURE STRUCTURE` )
*      ( `    ELEMENT : g` )
*      ( `CLOSE_STRUCTURE STRUCTURE_IN_STRUCTURE` )
*    ).
*    assert_output_equals( exp = exp_result act = act_result ).
*  endmethod.
*
*  method table_build_in_type.
*    data table_build_in_type type lif_test_types=>table_build_in_type.
*
*    data(act_result) = cut->generate_type( table_build_in_type ).
*
*    exp_result = value #(
*      ( `OPEN_TABLE TABLE_BUILD_IN_TYPE` )
*      ( `    STRING : g` )
*      ( `CLOSE_TABLE TABLE_BUILD_IN_TYPE` )
*    ).
*    assert_output_equals( exp = exp_result act = act_result ).
*  endmethod.
*
*  method table_structure.
*    data table_structure type lif_test_types=>table_structure.
*    data(act_result) = cut->generate_type( table_structure ).
*
*    exp_result = value #(
*      ( `OPEN_TABLE TABLE_STRUCTURE` )
*      ( `    OPEN_STRUCTURE STRUCTURE` )
*      ( `        ELEMENT_1 : I` )
*      ( `        ELEMENT_2 : g` )
*      ( `    CLOSE_STRUCTURE STRUCTURE` )
*      ( `CLOSE_TABLE TABLE_STRUCTURE` )
*    ).
*    assert_output_equals( exp = exp_result act = act_result ).
*  endmethod.
*
*  method structure_with_table.
*    data structure_with_table type lif_test_types=>structure_with_table.
*    data(act_result) = cut->generate_type( structure_with_table ).
*
*    exp_result = value #(
*      ( `OPEN_STRUCTURE STRUCTURE_WITH_TABLE` )
*      ( `    OPEN_TABLE TABLE` )
*      ( `        OPEN_STRUCTURE STRUCTURE` )
*      ( `            ELEMENT_1 : I` )
*      ( `            ELEMENT_2 : g` )
*      ( `        CLOSE_STRUCTURE STRUCTURE` )
*      ( `    CLOSE_TABLE TABLE` )
*      ( `CLOSE_STRUCTURE STRUCTURE_WITH_TABLE` )
*    ).
*    assert_output_equals( exp = exp_result act = act_result ).
*  endmethod.
*
*  method include_table.
*    data include_table type lif_test_types=>include_table.
*    data(act_result) = cut->generate_type( include_table ).
*
*    exp_result = value #(
*      ( `OPEN_STRUCTURE INCLUDE_TABLE` )
*      ( `    OPEN_TABLE TABLE` )
*      ( `        OPEN_STRUCTURE STRUCTURE` )
*      ( `            ELEMENT_1 : I` )
*      ( `            ELEMENT_2 : g` )
*      ( `        CLOSE_STRUCTURE STRUCTURE` )
*      ( `    CLOSE_TABLE TABLE` )
*      ( `    INCLUDE_ELEMENT_1 : I` )
*      ( `CLOSE_STRUCTURE INCLUDE_TABLE` )
*    ).
*    assert_output_equals( exp = exp_result act = act_result ).
*  endmethod.
*
*  method table_in_table.
*    data table_in_table type lif_test_types=>table_in_table.
*    data(act_result) = cut->generate_type( table_in_table ).
*
*    exp_result = value #(
*      ( `OPEN_TABLE TABLE_IN_TABLE` )
*      ( `    OPEN_TABLE TABLE_BUILD_IN_TYPE` )
*      ( `        STRING : g` )
*      ( `    CLOSE_TABLE TABLE_BUILD_IN_TYPE` )
*      ( `CLOSE_TABLE TABLE_IN_TABLE` )
*    ).
*    assert_output_equals( exp = exp_result act = act_result ).
*  endmethod.
*
*  method struc_tab_struc_tab.
*    data struc_tab_struc_tab type lif_test_types=>struc_tab_struc_tab.
*    data(act_result) = cut->generate_type( struc_tab_struc_tab ).
*
*    exp_result = value #(
*      ( `OPEN_STRUCTURE STRUC_TAB_STRUC_TAB` )
*      ( `    OPEN_TABLE FIRST_TABLE` )
*      ( `        OPEN_STRUCTURE NESTED_TABLE` )
*      ( `            OPEN_TABLE SECOND_TABLE` )
*      ( `                STRING : g` )
*      ( `            CLOSE_TABLE SECOND_TABLE` )
*      ( `        CLOSE_STRUCTURE NESTED_TABLE` )
*      ( `    CLOSE_TABLE FIRST_TABLE` )
*      ( `CLOSE_STRUCTURE STRUC_TAB_STRUC_TAB` )
*    ).
*    assert_output_equals( exp = exp_result act = act_result ).
*  endmethod.
*
*  method unsupported_type.
*    data class_reference type ref to zcl_aff_generator ##NEEDED.
*    try.
*        data(act_result) = cut->generate_type( class_reference ).
*        cl_abap_unit_assert=>fail( msg = 'Exception expected' ).
*      catch cx_aff_root into data(exception) ##NO_HANDLER.
*    endtry.
*
*    cl_abap_unit_assert=>assert_initial( act_result ).
*    cl_abap_unit_assert=>assert_equals( exp = 'SAFF_CORE' act = exception->if_t100_message~t100key-msgid ).
*    cl_abap_unit_assert=>assert_equals( exp = 100 act = exception->if_t100_message~t100key-msgno ).
*  endmethod.
*
*  method complex_structure_aff_class.
*    data aff_class type lif_test_types=>ty_class_properties.
*
*    data(act_result) = cut->generate_type( aff_class ).
*
*    exp_result = value #(
*      ( `OPEN_STRUCTURE TY_CLASS_PROPERTIES` )
*
*      ( `    FORMAT_VERSION : g` )
*
*      ( `    OPEN_STRUCTURE HEADER` )
*      ( `        DESCRIPTION : C` )
*      ( `        ORIGINAL_LANGUAGE : C` )
*      ( `        ABAP_LANGUAGE_VERSION : C` )
*      ( `    CLOSE_STRUCTURE HEADER` )
*
*      ( `    CATEGORY : N` )
*      ( `    FIXPT : C` )
*      ( `    MSG_ID : C` )
*
*      ( `    OPEN_TABLE ATTRIBUTES` )
*      ( `        OPEN_STRUCTURE TY_COMPONENT` )
*      ( `            NAME : C` )
*      ( `            DESCRIPTION : C` )
*      ( `        CLOSE_STRUCTURE TY_COMPONENT` )
*      ( `    CLOSE_TABLE ATTRIBUTES` )
*
*      ( `    OPEN_TABLE METHODS` )
*      ( `        OPEN_STRUCTURE TY_METHOD` )
*      ( `            NAME : C` )
*      ( `            DESCRIPTION : C` )
*      ( `            OPEN_TABLE PARAMETERS` )
*      ( `                OPEN_STRUCTURE TY_COMPONENT` )
*      ( `                    NAME : C` )
*      ( `                    DESCRIPTION : C` )
*      ( `                CLOSE_STRUCTURE TY_COMPONENT` )
*      ( `            CLOSE_TABLE PARAMETERS` )
*      ( `            OPEN_TABLE EXCEPTIONS` )
*      ( `                OPEN_STRUCTURE TY_COMPONENT` )
*      ( `                    NAME : C` )
*      ( `                    DESCRIPTION : C` )
*      ( `                CLOSE_STRUCTURE TY_COMPONENT` )
*      ( `            CLOSE_TABLE EXCEPTIONS` )
*      ( `        CLOSE_STRUCTURE TY_METHOD` )
*      ( `    CLOSE_TABLE METHODS` )
*
*      ( `    OPEN_TABLE EVENTS` )
*      ( `        OPEN_STRUCTURE TY_EVENT` )
*      ( `            NAME : C` )
*      ( `            DESCRIPTION : C` )
*      ( `            OPEN_TABLE PARAMETERS` )
*      ( `                OPEN_STRUCTURE TY_COMPONENT` )
*      ( `                    NAME : C` )
*      ( `                    DESCRIPTION : C` )
*      ( `                CLOSE_STRUCTURE TY_COMPONENT` )
*      ( `            CLOSE_TABLE PARAMETERS` )
*      ( `        CLOSE_STRUCTURE TY_EVENT` )
*      ( `    CLOSE_TABLE EVENTS` )
*
*      ( `    OPEN_TABLE TYPES` )
*      ( `        OPEN_STRUCTURE TY_COMPONENT` )
*      ( `            NAME : C` )
*      ( `            DESCRIPTION : C` )
*      ( `        CLOSE_STRUCTURE TY_COMPONENT` )
*      ( `    CLOSE_TABLE TYPES` )
*
*      ( `CLOSE_STRUCTURE TY_CLASS_PROPERTIES` )
*    ).
*    assert_output_equals( exp = exp_result act = act_result ).
*  endmethod.
*
*  method mandatory_fields.
*    data abap_type type lif_test_types=>ty_abap_type.
*    cut->generate_type( abap_type ).
*    data(log) = cut->get_log( ).
*    cl_aff_unit_test_helper=>assert_log_has_no_message( log ).
*  endmethod.
*
*  method no_header.
*    data no_header type lif_test_types=>ty_abap_type_no_header.
*    cut->generate_type( no_header ).
*    data(log) = cut->get_log( ).
*    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
*                                                      exp_message = value #( msgid = 'SAFF_CORE'
*                                                                             msgno = 124 )
*                                                      exp_type    = if_aff_log=>c_message_type-warning ).
*  endmethod.
*
*  method no_format_version.
*    data no_format_version type lif_test_types=>ty_abap_type_no_format.
*    cut->generate_type( no_format_version ).
*    data(log) = cut->get_log( ).
*    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
*                                                      exp_message = value #( msgid = 'SAFF_CORE'
*                                                                             msgno = 124 )
*                                                      exp_type    = if_aff_log=>c_message_type-warning ).
*  endmethod.
*
*  method no_structure.
*    data no_structure type lif_test_types=>table_in_table.
*    cut->generate_type( no_structure ).
*    data(log) = cut->get_log( ).
*    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
*                                                      exp_message = value #( msgid = 'SAFF_CORE'
*                                                                             msgno = 123 )
*                                                      exp_type    = if_aff_log=>c_message_type-warning ).
*  endmethod.
*
*
*  method assert_output_equals.
*    cl_abap_unit_assert=>assert_equals( exp = lines( exp ) act = lines( act ) msg = `Number of entries doesn't match expectation` ).
*    loop at exp assigning field-symbol(<exp_line>).
*      data(act_line) = act[ sy-tabix ].
*      cl_abap_unit_assert=>assert_equals( exp = <exp_line> act = act_line msg = |line { sy-tabix } doesn't match expectation| quit = if_abap_unit_constant=>quit-no ).
*    endloop.
*  endmethod.
*
*endclass.

INTERFACE lif_test_types.
  TYPES:
    element TYPE string.

  TYPES:
    BEGIN OF structure,
      element_1 TYPE i,
      element_2 TYPE element,
    END OF structure.

  TYPES:
    BEGIN OF include,
      include_element_1 TYPE string,
      include_element_2 TYPE i,
    END OF include.

  TYPES:
    BEGIN OF structure_with_include.
      INCLUDE TYPE include.
  TYPES element_1 TYPE i.
  TYPES element_2 TYPE element.
  TYPES END OF structure_with_include.

  TYPES:
    BEGIN OF include_in_include.
      INCLUDE TYPE include.
  TYPES END OF include_in_include.

  TYPES:
    BEGIN OF structure_include_in_include.
      INCLUDE TYPE include_in_include.
  TYPES element TYPE string.
  TYPES END OF structure_include_in_include.

  TYPES:
    BEGIN OF structure_in_structure,
      structure TYPE structure,
      element   TYPE element,
    END OF structure_in_structure.

  TYPES:
    table_structure TYPE STANDARD TABLE OF structure WITH DEFAULT KEY.

  TYPES:
    table_build_in_type TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

  TYPES:
    BEGIN OF structure_with_table,
      table TYPE table_structure,
    END OF structure_with_table.

  TYPES:
    BEGIN OF include_table.
      INCLUDE TYPE structure_with_table.
  TYPES include_element_1 TYPE i.
  TYPES END OF include_table.

  TYPES:
    table_in_table TYPE STANDARD TABLE OF table_build_in_type WITH DEFAULT KEY.

  TYPES:
    BEGIN OF nested_table,
      second_table TYPE table_build_in_type,
    END OF nested_table,
    first_table_type TYPE STANDARD TABLE OF nested_table WITH DEFAULT KEY,
    BEGIN OF struc_tab_struc_tab,
      first_table TYPE first_table_type,
    END OF struc_tab_struc_tab.

  TYPES:
    BEGIN OF ty_component,
      name        TYPE c LENGTH 30,
      description TYPE c LENGTH 60,
    END OF ty_component,
    ty_components    TYPE SORTED TABLE OF ty_component WITH UNIQUE KEY name,
    ty_subcomponents TYPE SORTED TABLE OF ty_component WITH UNIQUE KEY name,
    BEGIN OF ty_method,
      name        TYPE c LENGTH 30,
      description TYPE c LENGTH 60,
      parameters  TYPE ty_subcomponents,
      exceptions  TYPE ty_subcomponents,
    END OF ty_method,
    ty_methods TYPE SORTED TABLE OF ty_method WITH UNIQUE KEY name,
    BEGIN OF ty_event,
      name        TYPE c LENGTH 30,
      description TYPE c LENGTH 60,
      parameters  TYPE ty_subcomponents,
    END OF ty_event,
    ty_events TYPE SORTED TABLE OF ty_event WITH UNIQUE KEY name,
    BEGIN OF ty_clif_properties,
      attributes TYPE ty_components,
      methods    TYPE ty_methods,
      events     TYPE ty_events,
      types      TYPE ty_components,
    END OF ty_clif_properties.

  TYPES:
    BEGIN OF ty_header,
      description TYPE c LENGTH 30,
    END OF ty_header.

  TYPES:
    BEGIN OF ty_class_properties,
      format_version TYPE string,
      header         TYPE ty_header,
      category       TYPE n LENGTH 2,
      fixpt          TYPE c LENGTH 1,
      msg_id         TYPE c LENGTH 20.
      INCLUDE TYPE ty_clif_properties.
  TYPES END OF ty_class_properties.

  TYPES:
    BEGIN OF ty_abap_type,
      format_version  TYPE string,
      header          TYPE ty_header,
      other_component TYPE i,
    END OF ty_abap_type.
  TYPES:
    BEGIN OF ty_abap_type_no_header,
      format_version  TYPE string,
      other_component TYPE i,
    END OF ty_abap_type_no_header.
  TYPES:
    BEGIN OF ty_abap_type_no_format,
      header          TYPE ty_header,
      other_component TYPE i,
    END OF ty_abap_type_no_format.

ENDINTERFACE.

CLASS ltcl_unit_test_writer DEFINITION CREATE PUBLIC FOR TESTING INHERITING FROM zcl_aff_writer FINAL.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS:
      write_element REDEFINITION,
      open_structure REDEFINITION,
      close_structure REDEFINITION,
      open_table REDEFINITION,
      write_tag REDEFINITION,
      close_table REDEFINITION.

  PRIVATE SECTION.
    DATA:
      depth          TYPE i VALUE 0.

ENDCLASS.

CLASS ltcl_unit_test_writer IMPLEMENTATION.

  METHOD write_element.
    APPEND |{ repeat( val = ` `  occ = 4 * depth ) }{ element_name } : { element_description->type_kind }| TO output.
  ENDMETHOD.

  METHOD close_structure.
    APPEND |{ repeat( val = ` `  occ = 4 * ( depth - 1 ) ) }CLOSE_STRUCTURE { structure_name }| TO output.
    depth -= 1.
  ENDMETHOD.

  METHOD close_table.
    APPEND |{ repeat( val = ` `  occ = 4 * ( depth - 1 ) ) }CLOSE_TABLE { table_name }| TO output.
    depth -= 1.
  ENDMETHOD.

  METHOD open_structure.
    APPEND |{ repeat( val = ` `  occ = 4 * depth ) }OPEN_STRUCTURE { structure_name }| TO output.
    depth += 1.
  ENDMETHOD.

  METHOD open_table.
    APPEND |{ repeat( val = ` `  occ = 4 * depth ) }OPEN_TABLE { table_name }| TO output.
    depth += 1.
  ENDMETHOD.

  METHOD write_tag ##NEEDED.
  ENDMETHOD.


ENDCLASS.

CLASS ltcl_type_generator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    INTERFACES lif_test_types.

  PRIVATE SECTION.
    DATA:
      cut        TYPE REF TO zcl_aff_generator,
      exp_result TYPE string_table.

    METHODS:
      element FOR TESTING RAISING cx_static_check,
      structure FOR TESTING RAISING cx_static_check,
      include FOR TESTING RAISING cx_static_check,
      table_build_in_type FOR TESTING RAISING cx_static_check,
      include_in_include FOR TESTING RAISING cx_static_check,
      structure_in_structure FOR TESTING RAISING cx_static_check,
      table_structure FOR TESTING RAISING cx_static_check,
      structure_with_table FOR TESTING RAISING cx_static_check,
      include_table FOR TESTING RAISING cx_static_check,
      table_in_table FOR TESTING RAISING cx_static_check,
      struc_tab_struc_tab FOR TESTING RAISING cx_static_check,
      unsupported_type FOR TESTING RAISING cx_static_check,
      complex_structure_aff_class FOR TESTING RAISING cx_static_check,
      mandatory_fields FOR TESTING RAISING cx_static_check,
      no_header FOR TESTING RAISING cx_static_check,
      no_format_version FOR TESTING RAISING cx_static_check,
      no_structure FOR TESTING RAISING cx_static_check,
      setup,
      assert_output_equals
        IMPORTING
          act TYPE string_table
          exp TYPE string_table.
ENDCLASS.

CLASS zcl_aff_generator DEFINITION LOCAL FRIENDS ltcl_type_generator.

CLASS ltcl_type_generator IMPLEMENTATION.

  METHOD setup.
    cut = NEW zcl_aff_generator( NEW ltcl_unit_test_writer( ) ).
  ENDMETHOD.

  METHOD element.
    DATA test_data TYPE lif_test_types=>element.
    DATA(act_result) = cut->generate_type( test_data ).

    exp_result = VALUE #( ( `ELEMENT : g` ) ).
    assert_output_equals( exp = exp_result act = act_result ).
  ENDMETHOD.

  METHOD structure.
    DATA test_data TYPE lif_test_types=>structure.
    DATA(act_result) = cut->generate_type( test_data ).

    exp_result = VALUE #(
      ( `OPEN_STRUCTURE STRUCTURE` )
      ( `    ELEMENT_1 : I` )
      ( `    ELEMENT_2 : g` )
      ( `CLOSE_STRUCTURE STRUCTURE` ) ).
    assert_output_equals( exp = exp_result act = act_result ).
  ENDMETHOD.

  METHOD include.
    DATA test_data TYPE lif_test_types=>structure_with_include.
    DATA(act_result) = cut->generate_type( test_data ).

    exp_result = VALUE #(
      ( `OPEN_STRUCTURE STRUCTURE_WITH_INCLUDE` )
      ( `    INCLUDE_ELEMENT_1 : g` )
      ( `    INCLUDE_ELEMENT_2 : I` )
      ( `    ELEMENT_1 : I` )
      ( `    ELEMENT_2 : g` )
      ( `CLOSE_STRUCTURE STRUCTURE_WITH_INCLUDE` ) ).
    assert_output_equals( exp = exp_result act = act_result ).
  ENDMETHOD.

  METHOD include_in_include.
    DATA test_data TYPE lif_test_types=>structure_include_in_include.
    DATA(act_result) = cut->generate_type( test_data ).

    exp_result = VALUE #(
      ( `OPEN_STRUCTURE STRUCTURE_INCLUDE_IN_INCLUDE` )
      ( `    INCLUDE_ELEMENT_1 : g` )
      ( `    INCLUDE_ELEMENT_2 : I` )
      ( `    ELEMENT : g` )
      ( `CLOSE_STRUCTURE STRUCTURE_INCLUDE_IN_INCLUDE` ) ).
    assert_output_equals( exp = exp_result act = act_result ).
  ENDMETHOD.

  METHOD structure_in_structure.
    DATA test_data TYPE lif_test_types=>structure_in_structure.
    DATA(act_result) = cut->generate_type( test_data ).

    exp_result = VALUE #(
      ( `OPEN_STRUCTURE STRUCTURE_IN_STRUCTURE` )
      ( `    OPEN_STRUCTURE STRUCTURE` )
      ( `        ELEMENT_1 : I` )
      ( `        ELEMENT_2 : g` )
      ( `    CLOSE_STRUCTURE STRUCTURE` )
      ( `    ELEMENT : g` )
      ( `CLOSE_STRUCTURE STRUCTURE_IN_STRUCTURE` ) ).
    assert_output_equals( exp = exp_result act = act_result ).
  ENDMETHOD.

  METHOD table_build_in_type.
    DATA table_build_in_type TYPE lif_test_types=>table_build_in_type.

    DATA(act_result) = cut->generate_type( table_build_in_type ).

    exp_result = VALUE #(
      ( `OPEN_TABLE TABLE_BUILD_IN_TYPE` )
      ( `    STRING : g` )
      ( `CLOSE_TABLE TABLE_BUILD_IN_TYPE` ) ).
    assert_output_equals( exp = exp_result act = act_result ).
  ENDMETHOD.

  METHOD table_structure.
    DATA table_structure TYPE lif_test_types=>table_structure.
    DATA(act_result) = cut->generate_type( table_structure ).

    exp_result = VALUE #(
      ( `OPEN_TABLE TABLE_STRUCTURE` )
      ( `    OPEN_STRUCTURE STRUCTURE` )
      ( `        ELEMENT_1 : I` )
      ( `        ELEMENT_2 : g` )
      ( `    CLOSE_STRUCTURE STRUCTURE` )
      ( `CLOSE_TABLE TABLE_STRUCTURE` ) ).
    assert_output_equals( exp = exp_result act = act_result ).
  ENDMETHOD.

  METHOD structure_with_table.
    DATA structure_with_table TYPE lif_test_types=>structure_with_table.
    DATA(act_result) = cut->generate_type( structure_with_table ).

    exp_result = VALUE #(
      ( `OPEN_STRUCTURE STRUCTURE_WITH_TABLE` )
      ( `    OPEN_TABLE TABLE` )
      ( `        OPEN_STRUCTURE STRUCTURE` )
      ( `            ELEMENT_1 : I` )
      ( `            ELEMENT_2 : g` )
      ( `        CLOSE_STRUCTURE STRUCTURE` )
      ( `    CLOSE_TABLE TABLE` )
      ( `CLOSE_STRUCTURE STRUCTURE_WITH_TABLE` ) ).
    assert_output_equals( exp = exp_result act = act_result ).
  ENDMETHOD.

  METHOD include_table.
    DATA include_table TYPE lif_test_types=>include_table.
    DATA(act_result) = cut->generate_type( include_table ).

    exp_result = VALUE #(
      ( `OPEN_STRUCTURE INCLUDE_TABLE` )
      ( `    OPEN_TABLE TABLE` )
      ( `        OPEN_STRUCTURE STRUCTURE` )
      ( `            ELEMENT_1 : I` )
      ( `            ELEMENT_2 : g` )
      ( `        CLOSE_STRUCTURE STRUCTURE` )
      ( `    CLOSE_TABLE TABLE` )
      ( `    INCLUDE_ELEMENT_1 : I` )
      ( `CLOSE_STRUCTURE INCLUDE_TABLE` ) ).
    assert_output_equals( exp = exp_result act = act_result ).
  ENDMETHOD.

  METHOD table_in_table.
    DATA table_in_table TYPE lif_test_types=>table_in_table.
    DATA(act_result) = cut->generate_type( table_in_table ).

    exp_result = VALUE #(
      ( `OPEN_TABLE TABLE_IN_TABLE` )
      ( `    OPEN_TABLE TABLE_BUILD_IN_TYPE` )
      ( `        STRING : g` )
      ( `    CLOSE_TABLE TABLE_BUILD_IN_TYPE` )
      ( `CLOSE_TABLE TABLE_IN_TABLE` ) ).
    assert_output_equals( exp = exp_result act = act_result ).
  ENDMETHOD.

  METHOD struc_tab_struc_tab.
    DATA struc_tab_struc_tab TYPE lif_test_types=>struc_tab_struc_tab.
    DATA(act_result) = cut->generate_type( struc_tab_struc_tab ).

    exp_result = VALUE #(
      ( `OPEN_STRUCTURE STRUC_TAB_STRUC_TAB` )
      ( `    OPEN_TABLE FIRST_TABLE` )
      ( `        OPEN_STRUCTURE NESTED_TABLE` )
      ( `            OPEN_TABLE SECOND_TABLE` )
      ( `                STRING : g` )
      ( `            CLOSE_TABLE SECOND_TABLE` )
      ( `        CLOSE_STRUCTURE NESTED_TABLE` )
      ( `    CLOSE_TABLE FIRST_TABLE` )
      ( `CLOSE_STRUCTURE STRUC_TAB_STRUC_TAB` ) ).
    assert_output_equals( exp = exp_result act = act_result ).
  ENDMETHOD.

  METHOD unsupported_type.
    DATA class_reference TYPE REF TO zcl_aff_generator ##NEEDED.
    TRY.
        cut->generate_type( class_reference ).
        cl_abap_unit_assert=>fail( msg = 'Exception expected' ).
      CATCH zcx_aff_tools ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD complex_structure_aff_class.
    DATA aff_class TYPE lif_test_types=>ty_class_properties.

    DATA(act_result) = cut->generate_type( aff_class ).

    exp_result = VALUE #(
      ( `OPEN_STRUCTURE TY_CLASS_PROPERTIES` )

      ( `    FORMAT_VERSION : g` )

      ( `    OPEN_STRUCTURE HEADER` )
      ( `        DESCRIPTION : C` )
      ( `    CLOSE_STRUCTURE HEADER` )

      ( `    CATEGORY : N` )
      ( `    FIXPT : C` )
      ( `    MSG_ID : C` )

      ( `    OPEN_TABLE ATTRIBUTES` )
      ( `        OPEN_STRUCTURE TY_COMPONENT` )
      ( `            NAME : C` )
      ( `            DESCRIPTION : C` )
      ( `        CLOSE_STRUCTURE TY_COMPONENT` )
      ( `    CLOSE_TABLE ATTRIBUTES` )

      ( `    OPEN_TABLE METHODS` )
      ( `        OPEN_STRUCTURE TY_METHOD` )
      ( `            NAME : C` )
      ( `            DESCRIPTION : C` )
      ( `            OPEN_TABLE PARAMETERS` )
      ( `                OPEN_STRUCTURE TY_COMPONENT` )
      ( `                    NAME : C` )
      ( `                    DESCRIPTION : C` )
      ( `                CLOSE_STRUCTURE TY_COMPONENT` )
      ( `            CLOSE_TABLE PARAMETERS` )
      ( `            OPEN_TABLE EXCEPTIONS` )
      ( `                OPEN_STRUCTURE TY_COMPONENT` )
      ( `                    NAME : C` )
      ( `                    DESCRIPTION : C` )
      ( `                CLOSE_STRUCTURE TY_COMPONENT` )
      ( `            CLOSE_TABLE EXCEPTIONS` )
      ( `        CLOSE_STRUCTURE TY_METHOD` )
      ( `    CLOSE_TABLE METHODS` )

      ( `    OPEN_TABLE EVENTS` )
      ( `        OPEN_STRUCTURE TY_EVENT` )
      ( `            NAME : C` )
      ( `            DESCRIPTION : C` )
      ( `            OPEN_TABLE PARAMETERS` )
      ( `                OPEN_STRUCTURE TY_COMPONENT` )
      ( `                    NAME : C` )
      ( `                    DESCRIPTION : C` )
      ( `                CLOSE_STRUCTURE TY_COMPONENT` )
      ( `            CLOSE_TABLE PARAMETERS` )
      ( `        CLOSE_STRUCTURE TY_EVENT` )
      ( `    CLOSE_TABLE EVENTS` )

      ( `    OPEN_TABLE TYPES` )
      ( `        OPEN_STRUCTURE TY_COMPONENT` )
      ( `            NAME : C` )
      ( `            DESCRIPTION : C` )
      ( `        CLOSE_STRUCTURE TY_COMPONENT` )
      ( `    CLOSE_TABLE TYPES` )

      ( `CLOSE_STRUCTURE TY_CLASS_PROPERTIES` ) ).
    assert_output_equals( exp = exp_result act = act_result ).
  ENDMETHOD.

  METHOD mandatory_fields.
    DATA abap_type TYPE lif_test_types=>ty_abap_type.
    cut->generate_type( abap_type ).
    DATA(log) = cut->get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log ).
  ENDMETHOD.

  METHOD no_header.
    DATA no_header TYPE lif_test_types=>ty_abap_type_no_header.
    cut->generate_type( no_header ).
    DATA(log) = cut->get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = zif_aff_log=>co_msg124
                                                              exp_component_name = `TY_ABAP_TYPE_NO_HEADER`
                                                              exp_type           = zif_aff_log=>c_message_type-warning ).
  ENDMETHOD.

  METHOD no_format_version.
    DATA no_format_version TYPE lif_test_types=>ty_abap_type_no_format.
    cut->generate_type( no_format_version ).
    DATA(log) = cut->get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = zif_aff_log=>co_msg124
                                                              exp_component_name = `TY_ABAP_TYPE_NO_FORMAT`
                                                              exp_type           = zif_aff_log=>c_message_type-warning ).
  ENDMETHOD.

  METHOD no_structure.
    DATA no_structure TYPE lif_test_types=>table_in_table.
    cut->generate_type( no_structure ).
    DATA(log) = cut->get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = zif_aff_log=>co_msg123
                                                              exp_component_name = `TABLE_IN_TABLE`
                                                              exp_type           = zif_aff_log=>c_message_type-warning ).
  ENDMETHOD.


  METHOD assert_output_equals.
    cl_abap_unit_assert=>assert_equals( exp = lines( exp ) act = lines( act ) msg = `Number of entries doesn't match expectation` ).
    LOOP AT exp ASSIGNING FIELD-SYMBOL(<exp_line>).
      DATA(act_line) = act[ sy-tabix ].
      cl_abap_unit_assert=>assert_equals( exp = <exp_line> act = act_line msg = |line { sy-tabix } doesn't match expectation| quit = if_abap_unit_constant=>quit-no ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

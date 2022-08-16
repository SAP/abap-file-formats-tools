*"* use this source file for your ABAP unit test classes
CLASS ltcl_abap_doc_reader DEFINITION FINAL FOR TESTING
        DURATION SHORT
        RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA test_obj TYPE REF TO zcl_aff_abap_doc_reader.

    METHODS setup.
    METHODS get_abap_doc_4_element_types FOR TESTING RAISING cx_static_check.
    METHODS get_abap_doc_4_element_data FOR TESTING RAISING cx_static_check.
    METHODS get_abap_doc_4_sub_elem_types FOR TESTING RAISING cx_static_check.
    METHODS get_abap_doc_4_wrong_elem_name FOR TESTING.
    METHODS get_abap_doc_4_elem_wo_adoc FOR TESTING.
    METHODS get_simple FOR TESTING RAISING cx_static_check.
    METHODS get_structure FOR TESTING RAISING cx_static_check.
    METHODS get_structure_types FOR TESTING RAISING cx_static_check.
    METHODS structure_and_fields FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_abap_doc_reader IMPLEMENTATION.
  METHOD setup.
    DATA lt_clif_source TYPE string_table.

    lt_clif_source = VALUE #(
    ( |class CL_EC_WITH_COMMENTED_TYPES definition| )
    ( | public| )
    ( |  final| )
    ( |  create public .| )
    ( || )
    ( | public section.| )
    ( || )
    ( |    types:| )
    ( |      "! abap doc comment begin of ty_pub_structure| )
    ( |      begin " inline comment| )
    ( |       of  " inline comment| )
    ( |        ty_pub_structure, " inline comment ty_pub_structure| )
    ( |          "! ABAP Doc This is field A of the structure| )
    ( |          field_a type i, " inline comment field_a| )
    ( |          "! ABAP Doc This is field B of the structure| )
    ( |          field_b type string,| )
    ( |      end of ty_pub_structure .| )
    ( |    types:| )
    ( |      "! abap doc ty_tab_of_structure| )
    ( |      ty_tab_of_pub_structure " inline comment ty_tab_of_structure (type table of )| )
    ( |     " pure inline comment line 1| )
    ( |       type  " inline| )
    ( |         table of ty_pub_structure with default key .| )
    ( | | )
    ( |     data SUBRC type SY-SUBRC read-only .| )
    ( | protected section.| )
    ( | private section.| )
    ( |  "! Just simple data| )
    ( |  data abc type i.| )
    ( |ENDCLASS.| )
    ( || )
    ( || )
    ( |CLASS CL_EC_WITH_COMMENTED_TYPES IMPLEMENTATION.| )
    ( |ENDCLASS.| ) ).

    test_obj = zcl_aff_abap_doc_reader=>create_instance( source = lt_clif_source ).

  ENDMETHOD.

  METHOD get_abap_doc_4_element_types.
    DATA(result) = test_obj->get_abap_doc_for_element( element_name = 'ty_pub_structure' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'abap doc comment begin of ty_pub_structure'
      act = result ).
  ENDMETHOD.

  METHOD get_abap_doc_4_element_data.
    DATA(result) = test_obj->get_abap_doc_for_element( element_name = 'ABC' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'Just simple data'
      act = result ).
  ENDMETHOD.

  METHOD get_abap_doc_4_sub_elem_types.
    DATA(result) = test_obj->get_abap_doc_for_element( element_name = 'ty_pub_structure-field_a' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'ABAP Doc This is field A of the structure'
      act = result ).
  ENDMETHOD.

  METHOD get_abap_doc_4_wrong_elem_name.
    TRY.
        test_obj->get_abap_doc_for_element( element_name = 'ty_nicht_vorhanden' ).

        cl_abap_unit_assert=>fail( msg = 'Expected exception reporting wrong element name was not raised' ).
      CATCH zcx_aff_tools.
        RETURN.
      CATCH cx_root ##CATCH_ALL.
        cl_abap_unit_assert=>fail( msg = 'Unexpected exception type was raised' ).
    ENDTRY.

  ENDMETHOD.

  METHOD get_abap_doc_4_elem_wo_adoc.
    TRY.
        test_obj->get_abap_doc_for_element( element_name = 'SUBRC' ).

        cl_abap_unit_assert=>fail( msg = 'Expected exception reporting wrong element name was not raised' ).
      CATCH zcx_aff_tools.
        RETURN.
      CATCH cx_root ##CATCH_ALL.
        cl_abap_unit_assert=>fail( msg = 'Unexpected exception type was raised' ).
    ENDTRY.

  ENDMETHOD.

  METHOD get_simple.
    DATA(source) = VALUE string_table(
      ( |CLASS zcl_aff_test_types DEFINITION PUBLIC FINAL CREATE PUBLIC.| )
      ( |PUBLIC SECTION.| )
      ( |  TYPES:| )
      ( |    "! $hiddenabc| )
      ( |    unknown_annotation TYPE string.| )
      ( |ENDCLASS.| ) ).

    DATA(result) = zcl_aff_abap_doc_reader=>create_instance( source )->get_abap_doc_for_element( 'UNKNOWN_ANNOTATION' ).

    cl_abap_unit_assert=>assert_equals(
      exp = '$hiddenabc'
      act = result ).
  ENDMETHOD.

  METHOD get_structure.

    DATA(source) = VALUE string_table(
      ( |* simple structure| )
      ( |    TYPES:| )
      ( |      "! foo| )
      ( |      "! bar| )
      ( |      BEGIN OF my_structure,| )
      ( |        "! l1| )
      ( |        "! l2| )
      ( |        "! l3| )
      ( |        my_first_element  TYPE mystring,| )
      ( |        "! l4| )
      ( |        "! l5| )
      ( |        my_second_element TYPE i,| )
      ( |      END OF my_structure.| ) ).

    DATA(result) = zcl_aff_abap_doc_reader=>create_instance( source )->get_abap_doc_for_element( 'MY_STRUCTURE' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'foo bar'
      act = result ).

    result = zcl_aff_abap_doc_reader=>create_instance( source )->get_abap_doc_for_element( 'MY_STRUCTURE-MY_FIRST_ELEMENT' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'l1 l2 l3'
      act = result ).

    result = zcl_aff_abap_doc_reader=>create_instance( source )->get_abap_doc_for_element( 'MY_STRUCTURE-MY_SECOND_ELEMENT' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'l4 l5'
      act = result ).

  ENDMETHOD.

  METHOD structure_and_fields.

    DATA(source) = VALUE string_table(
      ( |    TYPES:| )
      ( |      BEGIN OF structure1,| )
      ( |        "! text1| )
      ( |        same_name TYPE i,| )
      ( |      END OF structure1.| )
      ( |    TYPES:| )
      ( |      BEGIN OF structure2,| )
      ( |        "! text2| )
      ( |        same_name TYPE i,| )
      ( |      END OF structure2.| ) ).

    DATA(result) = zcl_aff_abap_doc_reader=>create_instance( source )->get_abap_doc_for_element( 'STRUCTURE1-SAME_NAME' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'text1'
      act = result ).

    result = zcl_aff_abap_doc_reader=>create_instance( source )->get_abap_doc_for_element( 'STRUCTURE2-SAME_NAME' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'text2'
      act = result ).

  ENDMETHOD.

  METHOD get_structure_types.

    DATA(source) = VALUE string_table(
( |  TYPES:| )
( |    BEGIN OF ty_descriptions,| )
( |      "! hello| )
( |      types      TYPE string,| )
( |    END OF ty_descriptions.| ) ).

    DATA(result) = zcl_aff_abap_doc_reader=>create_instance( source )->get_abap_doc_for_element( 'TY_DESCRIPTIONS-TYPES' ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'hello'
      act = result ).

  ENDMETHOD.

ENDCLASS.

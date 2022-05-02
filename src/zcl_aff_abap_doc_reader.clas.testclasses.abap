*"* use this source file for your ABAP unit test classes
CLASS ltcl_abap_doc_reader DEFINITION FINAL FOR TESTING
        DURATION SHORT
        RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA test_obj TYPE REF TO zcl_aff_abap_doc_reader.

    METHODS setup.
    METHODS get_abap_doc_4_element_types FOR TESTING.
    METHODS get_abap_doc_4_element_data FOR TESTING.
    METHODS get_abap_doc_4_sub_elem_types FOR TESTING.
    METHODS get_abap_doc_4_wrong_elem_name FOR TESTING.
    METHODS get_abap_doc_4_elem_wo_adoc FOR TESTING.
ENDCLASS.

CLASS ltcl_abap_doc_reader IMPLEMENTATION.
  METHOD setup.
    DATA lt_clif_source TYPE zcl_aff_abap_doc_reader=>ty_source.

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
    TRY.
        test_obj->get_abap_doc_for_element(
    EXPORTING
    element_name = 'ty_pub_structure'     " TYPES
    RECEIVING
    result       = DATA(result) ).

        cl_abap_unit_assert=>assert_equals( exp = 'abap doc comment begin of ty_pub_structure'
    act = result ).
      CATCH cx_root ##NO_HANDLER ##CATCH_ALL.
    ENDTRY.

  ENDMETHOD.

  METHOD get_abap_doc_4_element_data.
    TRY.
        test_obj->get_abap_doc_for_element(
    EXPORTING
    element_name = 'ABC'                  " DATA
    RECEIVING
    result       = DATA(result) ).

        cl_abap_unit_assert=>assert_equals( exp = 'Just simple data'
    act = result ).
      CATCH cx_root ##NO_HANDLER ##CATCH_ALL.
    ENDTRY.
  ENDMETHOD.

  METHOD get_abap_doc_4_sub_elem_types.
    TRY.
        test_obj->get_abap_doc_for_element(
    EXPORTING
    element_name = 'ty_pub_structure-field_a'
    RECEIVING
    result       = DATA(result) ).
        cl_abap_unit_assert=>assert_equals( exp = 'ABAP Doc This is field A of the structure'
    act = result ).
      CATCH cx_root ##NO_HANDLER ##CATCH_ALL.
    ENDTRY.
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
ENDCLASS.

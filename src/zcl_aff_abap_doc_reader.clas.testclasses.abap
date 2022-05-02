*"* use this source file for your ABAP unit test classes
define append_to ##NEEDED.
  append &2 to &1.
end-of-definition.

class ltcl_abap_doc_reader definition final for testing
        duration short
        risk level harmless.

  private section.
    data test_obj type ref to zcl_aff_abap_doc_reader.

    methods setup.
    methods get_abap_doc_4_element_types for testing.
    methods get_abap_doc_4_element_data for testing.
    methods get_abap_doc_4_sub_elem_types for testing.
    methods get_abap_doc_4_wrong_elem_name for testing.
    methods get_abap_doc_4_elem_wo_adoc for testing.
endclass.

class ltcl_abap_doc_reader implementation.
  method setup.
    data lt_clif_source type zcl_aff_abap_doc_reader=>ty_source.

   append_to lt_clif_source:
      'class CL_EC_WITH_COMMENTED_TYPES definition',
      ' public',
      '  final',
      '  create public .',
      '',
      ' public section.',
      '',
      '    types:',
      '      "! abap doc comment begin of ty_pub_structure',
      '      begin " inline comment',
      '       of  " inline comment',
      '        ty_pub_structure, " inline comment ty_pub_structure',
      '          "! ABAP Doc This is field A of the structure',
      '          field_a type i, " inline comment field_a',
      '          "! ABAP Doc This is field B of the structure',
      '          field_b type string,',
      '      end of ty_pub_structure .',
      '    types:',
      '      "! abap doc ty_tab_of_structure',
      '      ty_tab_of_pub_structure " inline comment ty_tab_of_structure (type table of )',
      '     " pure inline comment line 1',
      '       type  " inline',
      '         table of ty_pub_structure with default key .',
      ' ',
      '     data SUBRC type SY-SUBRC read-only .',
      ' protected section.',
      ' private section.',
      '  "! Just simple data',
      '  data abc type i.',
      'ENDCLASS.',
      '',
      '',
      'CLASS CL_EC_WITH_COMMENTED_TYPES IMPLEMENTATION.',
      'ENDCLASS.'.

    test_obj = zcl_aff_abap_doc_reader=>create_instance( source = lt_clif_source ).

  endmethod.

  method get_abap_doc_4_element_types.
    try.
        test_obj->get_abap_doc_for_element(
          exporting
            element_name = 'ty_pub_structure'     " TYPES
          receiving
            result       = data(result)     ).

        cl_abap_unit_assert=>assert_equals( exp = 'abap doc comment begin of ty_pub_structure'
                                            act = result ).
      catch cx_root ##NO_HANDLER ##CATCH_ALL.
    endtry.

  endmethod.

  method get_abap_doc_4_element_data.
    try.
        test_obj->get_abap_doc_for_element(
          exporting
            element_name = 'ABC'                  " DATA
          receiving
            result       = data(result)   ).

        cl_abap_unit_assert=>assert_equals( exp = 'Just simple data'
                                            act = result ).
      catch cx_root ##NO_HANDLER ##CATCH_ALL.
    endtry.
  endmethod.

  method get_abap_doc_4_sub_elem_types.
    try.
        test_obj->get_abap_doc_for_element(
           exporting
             element_name = 'ty_pub_structure-field_a'
           receiving
             result       = data(result)      ).
        cl_abap_unit_assert=>assert_equals( exp = 'ABAP Doc This is field A of the structure'
                                            act = result ).
      catch cx_root ##NO_HANDLER ##CATCH_ALL.
    endtry.
  endmethod.

  method get_abap_doc_4_wrong_elem_name.
    try.
        test_obj->get_abap_doc_for_element(
          exporting
            element_name = 'ty_nicht_vorhanden'     " not existing
         ).

        cl_abap_unit_assert=>fail(
          exporting
            msg    = 'Expected exception reporting wrong element name was not raised'  ).
      catch cx_root into data(exc_ref) ##CATCH_ALL.
        if not ( exc_ref is instance of cx_oo_abap_doc_reader ).
          cl_abap_unit_assert=>fail(
          exporting
            msg    = 'Unexpected exception type was raised'  ).
        endif.
    endtry.

  endmethod.

  method get_abap_doc_4_elem_wo_adoc.
    try.
        test_obj->get_abap_doc_for_element(
          exporting
            element_name = 'SUBRC'     " DATA SUBRC hast not ABAP Doc
          ).

        cl_abap_unit_assert=>fail(
          exporting
            msg    = 'Expected exception reporting wrong element name was not raised'  ).
      catch cx_root into data(exc_ref) ##CATCH_ALL.
        if not ( exc_ref is instance of cx_oo_abap_doc_reader ).
          cl_abap_unit_assert=>fail(
          exporting
            msg    = 'Unexpected exception type was raised'  ).
        endif.
    endtry.

  endmethod.
endclass.

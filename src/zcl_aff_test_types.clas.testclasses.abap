
CLASS ltcl_sanity DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.
  PRIVATE SECTION.
    METHODS test FOR TESTING.
ENDCLASS.


CLASS ltcl_sanity IMPLEMENTATION.

  METHOD test.
    DATA lv_name TYPE string.
    lv_name = '\CLASS=ZCL_AFF_TEST_TYPES\TYPE=TY_FORMAT_VERSION'.

    DATA(descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( lv_name ) ).

    cl_abap_unit_assert=>assert_equals(
      act = descr->get_relative_name( )
      exp = 'TY_FORMAT_VERSION' ).
    cl_abap_unit_assert=>assert_equals(
      act = descr->absolute_name
      exp = lv_name ).

    DATA(field) = descr->get_component_type( 'FORMAT_VERSION' ).
    cl_abap_unit_assert=>assert_equals(
      act = field->get_relative_name( )
      exp = 'STRING' ).
    cl_abap_unit_assert=>assert_equals(
      act = field->absolute_name
      exp = '\TYPE=STRING' ).
  ENDMETHOD.

ENDCLASS.

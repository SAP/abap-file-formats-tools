CLASS ltcl_demo_greeting_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_aff_demo_greeting.

    METHODS setup.
    METHODS greet_with_name FOR TESTING.
    METHODS greet_empty_name FOR TESTING.

ENDCLASS.

CLASS ltcl_demo_greeting_test IMPLEMENTATION.

  METHOD setup.
    cut = NEW zcl_aff_demo_greeting( ).
  ENDMETHOD.

  METHOD greet_with_name.
    DATA(result) = cut->greet( `World` ).
    cl_abap_unit_assert=>assert_equals(
      act = result
      exp = `Hello, World!` ).
  ENDMETHOD.

  METHOD greet_empty_name.
    DATA(result) = cut->greet( `` ).
    cl_abap_unit_assert=>assert_equals(
      act = result
      exp = `Hello, !` ).
  ENDMETHOD.

ENDCLASS.

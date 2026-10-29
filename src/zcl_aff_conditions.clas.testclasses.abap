CLASS ltcl_serialization DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS defaults FOR TESTING RAISING cx_static_check.
    METHODS nested_rows FOR TESTING RAISING cx_static_check.
    METHODS enum_value FOR TESTING RAISING cx_static_check.
    METHODS numeric_condition FOR TESTING RAISING cx_static_check.
    METHODS no_conditions FOR TESTING RAISING cx_static_check.
    METHODS assert_contains
      IMPORTING output TYPE string_table
                expected TYPE string.
ENDCLASS.

CLASS ltcl_serialization IMPLEMENTATION.
  METHOD assert_contains.
    DATA(actual) = concat_lines_of( output ).
    REPLACE ALL OCCURRENCES OF '&apos;' IN actual WITH `'`.
    cl_abap_unit_assert=>assert_true( act = xsdbool( actual CS expected ) msg = expected ).
  ENDMETHOD.

  METHOD defaults.
    DATA(writer) = NEW zcl_aff_writer_xslt( ).
    DATA(generator) = NEW zcl_aff_generator( writer ).
    DATA(output) = generator->zif_aff_generator~generate_type( VALUE zcl_aff_test_types=>ty_cond_st_defaults( ) ).
    assert_contains( output = output expected = `s-check="(ref('.root.ENABLED')=C('X')) or (DETAIL!=C('fallback'))"` ).
    assert_contains( output = output expected = `s-check="(ref('.root.ENABLED')=C('X')) or (COUNT!=I(0))"` ).
    assert_contains( output = output expected = `s-check="(ref('.root.ENABLED')=C('X')) or (not-initial(FLAG))"` ).
    assert_contains( output = output expected = `<tt:cond s-check="not-initial(UNTOUCHED)" frq="?">` ).
    assert_contains( output = output expected = `<tt:assign to-ref="DETAIL" val="C('fallback')"/>` ).
  ENDMETHOD.

  METHOD nested_rows.
    DATA(writer) = NEW zcl_aff_writer_xslt( ).
    DATA(generator) = NEW zcl_aff_generator( writer ).
    DATA(output) = generator->zif_aff_generator~generate_type( VALUE zcl_aff_test_types=>ty_cond_st_nested( ) ).
    assert_contains( output = output expected = `ref('.root.SETTINGS.ENABLED')=C('X')` ).
    assert_contains( output = output expected = `<tt:loop name="affRow8" ref="ROWS">` ).
    assert_contains( output = output expected = `ref('$affRow8.ENABLED')=C('X')` ).
    assert_contains( output = output expected = `<tt:loop name="affRow15" ref="OTHER_ROWS">` ).
    assert_contains( output = output expected = `ref('$affRow15.ENABLED')=C('X')` ).
  ENDMETHOD.

  METHOD enum_value.
    DATA(writer) = NEW zcl_aff_writer_xslt( ).
    DATA(generator) = NEW zcl_aff_generator( writer ).
    DATA(output) = generator->zif_aff_generator~generate_type( VALUE zcl_aff_test_types=>ty_cond_st_enum( ) ).
    assert_contains( output = output expected = `s-check="(ref('.root.MODE')!=C('C')) or (DETAIL!=C('S'))"` ).
  ENDMETHOD.

  METHOD numeric_condition.
    DATA(writer) = NEW zcl_aff_writer_xslt( ).
    DATA(generator) = NEW zcl_aff_generator( writer ).
    DATA(output) = generator->zif_aff_generator~generate_type( VALUE zcl_aff_test_types=>ty_cond_zero_default( ) ).
    assert_contains( output = output expected = `s-check="(ref('.root.COUNT')=I(0)) or (not-initial(DETAIL))"` ).
  ENDMETHOD.

  METHOD no_conditions.
    DATA(writer) = NEW zcl_aff_writer_xslt( ).
    DATA(generator) = NEW zcl_aff_generator( writer ).
    DATA(output) = generator->zif_aff_generator~generate_type( VALUE zcl_aff_test_types=>ty_cond_table_path( ) ).
    assert_contains( output = output expected = `<tt:loop ref="SKILLS">` ).
    assert_contains( output = output expected = `<tt:cond s-check="not-initial(BEGINNER_DETAILS)" frq="?">` ).
  ENDMETHOD.
ENDCLASS.

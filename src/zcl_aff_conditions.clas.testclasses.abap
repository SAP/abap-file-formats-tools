CLASS ltcl_serialization DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS defaults FOR TESTING RAISING cx_static_check.
    METHODS implicit_initial FOR TESTING RAISING cx_static_check.
    METHODS membership FOR TESTING RAISING cx_static_check.
    METHODS invalid_membership FOR TESTING RAISING cx_static_check.
    METHODS nested_rows FOR TESTING RAISING cx_static_check.
    METHODS enum_value FOR TESTING RAISING cx_static_check.
    METHODS numeric_condition FOR TESTING RAISING cx_static_check.
    METHODS no_conditions FOR TESTING RAISING cx_static_check.
    METHODS unrelated_table FOR TESTING RAISING cx_static_check.
    METHODS assert_contains
      IMPORTING output TYPE string_table
                expected TYPE string.
ENDCLASS.

CLASS ltcl_serialization IMPLEMENTATION.
  METHOD assert_contains.
    DATA(actual) = concat_lines_of( output ).
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

  METHOD implicit_initial.
    DATA(writer) = NEW zcl_aff_writer_xslt( ).
    DATA(generator) = NEW zcl_aff_generator( writer ).
    DATA(output) = generator->zif_aff_generator~generate_type( VALUE zcl_aff_test_types=>ty_cond_domain( ) ).
    assert_contains( output = output expected = `s-check="(ref('.root.SETTINGS.BASE_DOMAIN.NAME')=C('')) or (not-initial(FORMAT))"` ).
  ENDMETHOD.

  METHOD membership.
    DATA(writer) = NEW zcl_aff_writer_xslt( ).
    DATA(generator) = NEW zcl_aff_generator( writer ).
    DATA(output) = generator->zif_aff_generator~generate_type( VALUE zcl_aff_test_types=>ty_cond_in_nested( ) ).
    assert_contains( output = output expected = `(ref('.root.SETTINGS.KIND')=C('static')) or (ref('.root.SETTINGS.KIND')=C('dynamic'))` ).
    assert_contains( output = output expected = `(ref('.root.SETTINGS.MODE')=C('S')) or (ref('.root.SETTINGS.MODE')=C('C'))` ).
    assert_contains( output = output expected = `(ref('.root.SETTINGS.COUNT')=I(-1)) or (ref('.root.SETTINGS.COUNT')=I(0))` ).
    assert_contains( output = output expected = `(ref('$affRow12.KIND')=C('static')) or (ref('$affRow12.KIND')=C('dynamic'))` ).
    assert_contains( output = output expected = `(ref('$affRow23.KIND')=C('static')) or (ref('$affRow23.KIND')=C('dynamic'))` ).
  ENDMETHOD.

  METHOD invalid_membership.
    DATA(conditions) = VALUE string_table(
      ( `in ()` ) ( `in ('a',)` ) ( `in (, 'a')` ) ( `in ('a' 'b')` )
      ( `in ('a', 'b') garbage` ) ( `in ('a', true)` ) ( `in ('a', unknown)` )
      ( `in ('a', {@link zcl_aff_test_types.data:co_condition_mode.cloud_mode})` ) ).
    LOOP AT conditions INTO DATA(condition).
      DATA(nodes) = VALUE zcl_aff_conditions=>nodes(
        ( type_key = 'ZCL_AFF_TEST_TYPES.TY_COND_IN' json_type = 'object' )
        ( parent = 1 name = 'kind' json_type = 'string' )
        ( parent = 1 name = 'detail' json_type = 'string'
          doc-required_when = `{@link zcl_aff_test_types.ty_cond_in.kind} ` && condition ) ).
      TRY.
          NEW zcl_aff_conditions( nodes )->compile( ).
          cl_abap_unit_assert=>fail( 'Invalid IN schema condition was accepted' ).
        CATCH zcx_aff_tools.
      ENDTRY.
      TRY.
          NEW zcl_aff_conditions( nodes )->compile_serialization( ).
          cl_abap_unit_assert=>fail( 'Invalid IN serialization condition was accepted' ).
        CATCH zcx_aff_tools.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD nested_rows.
    DATA(writer) = NEW zcl_aff_writer_xslt( ).
    DATA(generator) = NEW zcl_aff_generator( writer ).
    DATA(output) = generator->zif_aff_generator~generate_type( VALUE zcl_aff_test_types=>ty_cond_st_nested( ) ).
    assert_contains( output = output expected = `ref('.root.SETTINGS.ENABLED')=C('X')` ).
    assert_contains( output = output expected = `<tt:cond s-check="not-initial(SETTINGS)" frq="?">` ).
    assert_contains( output = output expected = `<tt:loop name="affRow8" ref="ROWS">` ).
    assert_contains( output = output expected = `ref('$affRow8.ENABLED')=C('X')` ).
    assert_contains( output = output expected = `<tt:loop name="affRow15" ref="OTHER_ROWS">` ).
    assert_contains( output = output expected = `ref('$affRow15.ENABLED')=C('X')` ).
    assert_contains( output = output expected = `<tt:loop ref="PLAIN_ROWS">` ).
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

  METHOD unrelated_table.
    DATA(writer) = NEW zcl_aff_writer_xslt( ).
    DATA(generator) = NEW zcl_aff_generator( writer ).
    DATA(output) = generator->zif_aff_generator~generate_type( VALUE zcl_aff_test_types=>ty_conditional( ) ).
    assert_contains( output = output expected = `<tt:cond s-check="not-initial(GENERAL_INFORMATION)" frq="?">` ).
    assert_contains( output = output expected = `<tt:cond s-check="not-initial(FAVORITES)" frq="?">` ).
  ENDMETHOD.
ENDCLASS.

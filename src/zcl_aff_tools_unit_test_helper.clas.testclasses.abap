CLASS ltcl_unit_test_helper DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      log TYPE REF TO zif_aff_log.

    METHODS: log_contains_msg_with_env FOR TESTING RAISING cx_static_check,
      log_contains_msg_without_env FOR TESTING RAISING cx_static_check,
      assert_no_message_severity FOR TESTING RAISING cx_static_check,
      assert_log_has_no_message FOR TESTING RAISING cx_static_check,
      change_environment_language,
      setup,
      teardown.
ENDCLASS.


CLASS ltcl_unit_test_helper IMPLEMENTATION.

  METHOD setup.
    log = NEW zcl_aff_log( ).
  ENDMETHOD.

  METHOD teardown.
    IF cl_abap_syst=>get_logon_language( ) = 'E'.
      SET LOCALE LANGUAGE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD log_contains_msg_with_env.
    change_environment_language( ).

    TRY.
        RAISE EXCEPTION TYPE zcx_aff_tools MESSAGE e100(z_aff_tools) WITH 'TEST_ATTR'.
      CATCH zcx_aff_tools INTO DATA(exception).
        log->add_exception( exception ).
    ENDTRY.

    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log         = log
                                                             exp_message = VALUE #( msgid = 'Z_AFF_TOOLS' msgno = '100' attr1 = 'TEST_ATTR' ) ).
  ENDMETHOD.

  METHOD log_contains_msg_without_env.
    change_environment_language( ).

    "environment language is not considered. Message still on English
    MESSAGE e100(z_aff_tools) WITH 'TEST_ATTR' INTO DATA(message) ##NEEDED.
    log->add_error( zcl_aff_log=>get_sy_message( ) ).

    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log         = log
                                                             exp_message = VALUE #( msgid = 'Z_AFF_TOOLS' msgno = '100' attr1 = 'TEST_ATTR' )
                                                             exp_type    = zif_aff_log=>c_message_type-error ).
  ENDMETHOD.

  METHOD assert_no_message_severity.
    log->add_warning( VALUE #( msgid = 'Z_AFF_TOOLS' msgno = 1 ) ).

    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = zif_aff_log=>c_message_type-error ).
  ENDMETHOD.

  METHOD assert_log_has_no_message.
    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log ).
  ENDMETHOD.

  METHOD change_environment_language.
    IF cl_abap_syst=>get_logon_language( ) = 'E'.
      SET LOCALE LANGUAGE 'D'.
    ELSE.
      cl_abap_unit_assert=>skip( msg = 'Skip test since the test is language depended' ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

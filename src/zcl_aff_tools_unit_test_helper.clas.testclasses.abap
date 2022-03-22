class ltcl_unit_test_helper definition final for testing
  duration short
  risk level harmless.

  private section.
    data:
      log type ref to zif_aff_log.

    methods: log_contains_msg_with_env for testing raising cx_static_check,
      log_contains_msg_without_env for testing raising cx_static_check,
      assert_no_message_severity for testing raising cx_static_check,
      assert_log_has_no_message for testing raising cx_static_check,
      change_environment_language,
      setup,
      teardown.
endclass.


class ltcl_unit_test_helper implementation.

  method setup.
    log = new zcl_aff_log( ).
  endmethod.

  method teardown.
    if cl_abap_syst=>get_logon_language( ) = 'E'.
      set locale language 'E'.
    endif.
  endmethod.

  method log_contains_msg_with_env.
    change_environment_language( ).

    try.
        raise exception type zcx_aff_tools message e100(z_aff_tools) with 'TEST_ATTR'.
      catch zcx_aff_tools into data(exception).
        log->add_exception( exception ).
    endtry.

    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log         = log
                                                             exp_message = value #( msgid = 'Z_AFF_TOOLS' msgno = '100' attr1 = 'TEST_ATTR' ) ).
  endmethod.

  method log_contains_msg_without_env.
    change_environment_language( ).

    "environment language is not considered. Message still on English
    message e100(z_aff_tools) with 'TEST_ATTR' into data(message) ##NEEDED.
    log->add_error( zcl_aff_log=>get_sy_message( ) ).

    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log         = log
                                                             exp_message = value #( msgid = 'Z_AFF_TOOLS' msgno = '100' attr1 = 'TEST_ATTR' )
                                                             exp_type    = zif_aff_log=>c_message_type-error ).
  endmethod.

  method assert_no_message_severity.
    log->add_warning( value #( msgid = 'Z_AFF_TOOLS' msgno = 1 ) ).

    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = zif_aff_log=>c_message_type-error ).
  endmethod.

  method assert_log_has_no_message.
    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log ).
  endmethod.

  method change_environment_language.
    if cl_abap_syst=>get_logon_language( ) = 'E'.
      set locale language 'D'.
    else.
      cl_abap_unit_assert=>skip( msg = 'Skip test since the test is language depended' ).
    endif.
  endmethod.

endclass.

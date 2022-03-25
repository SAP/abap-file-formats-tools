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
      log_contains_msg_with_comp FOR TESTING RAISING cx_static_check,
      setup.
ENDCLASS.


CLASS ltcl_unit_test_helper IMPLEMENTATION.

  METHOD setup.
    log = NEW zcl_aff_log( ).
  ENDMETHOD.

  METHOD log_contains_msg_with_env.
    TRY.
        RAISE EXCEPTION TYPE zcx_aff_tools MESSAGE e100(zaff_tools) WITH 'TEST_ATTR'.
      CATCH zcx_aff_tools INTO DATA(exception).
        log->add_exception( exception = exception component_name = '' ).
    ENDTRY.

    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log         = log
                                                             exp_message = VALUE #( msgid = 'ZAFF_TOOLS' msgno = '100' attr1 = 'TEST_ATTR' ) ).
  ENDMETHOD.

  METHOD log_contains_msg_without_env.
    MESSAGE e100(zaff_tools) WITH 'TEST_ATTR' INTO DATA(message) ##NEEDED.
    log->add_error( message = zcl_aff_log=>get_sy_message( ) component_name = 'TEST' ).

    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log         = log
                                                             exp_message = VALUE #( msgid = 'ZAFF_TOOLS' msgno = '100' attr1 = 'TEST_ATTR' )
                                                             exp_type    = zif_aff_log=>c_message_type-error ).
  ENDMETHOD.

  METHOD assert_no_message_severity.
    log->add_warning( message = VALUE #( msgid = 'ZAFF_TOOLS' msgno = 1 ) component_name = '' ).

    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = zif_aff_log=>c_message_type-error ).
  ENDMETHOD.

  METHOD assert_log_has_no_message.
    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log ).
  ENDMETHOD.

  METHOD log_contains_msg_with_comp.
    MESSAGE w100(zaff_tools) WITH 'TEST_ATTR' INTO DATA(message) ##NEEDED.
    log->add_warning( message = zcl_aff_log=>get_sy_message( ) component_name = 'EXAMPLE_COMPONENT' ).

    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log                = log
                                                             exp_message        = VALUE #( msgid = 'ZAFF_TOOLS' msgno = '100' attr1 = 'TEST_ATTR' )
                                                             exp_component_name = 'EXAMPLE_COMPONENT'
                                                             exp_type           = zif_aff_log=>c_message_type-warning ).
  ENDMETHOD.

ENDCLASS.

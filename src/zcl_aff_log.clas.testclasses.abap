"! Classic exception without t100 message
CLASS ltcx_classic_exception definition final for testing
  duration short
  risk level harmless
    inheriting from cx_static_check.
  public section.
endclass.

class ltcx_classic_exception implementation.
endclass.

class lcl_log_unit_test definition final for testing
  duration short
  risk level harmless.

  private section.
    data:
      log         type ref to zif_aff_log,
      message     type symsg,
      message_2   type symsg.

    methods: setup,
      add_info for testing raising cx_static_check,
      add_warning for testing raising cx_static_check,
      add_error for testing raising cx_static_check,
      add_t100_exception for testing raising cx_static_check,
      joins_log for testing raising cx_static_check,
      clears_log for testing raising cx_static_check,
      get_max_severity for testing raising cx_static_check,
      has_messages for testing raising cx_static_check,
      two_messages_for_one_object for testing raising cx_static_check,
      add_catched_exception for testing raising cx_static_check,
      add_classic_exception for testing raising cx_static_check,
      get_sy_message for testing raising cx_static_check,
      add_exception_as_info for testing raising cx_static_check,
      assert_message
        importing
          act_message type zif_aff_log=>ty_log_out
          type        type c
          exp_message type symsg
          object      type ref to if_aff_obj optional.
endclass.


class lcl_log_unit_test implementation.

  method setup.
    log = new zcl_aff_log( ).
    message = value #( msgid = 'Z_AFF_TOOLS' msgno = 100 msgv1 = 'TEST' ).
    message = value #( msgid = 'Z_AFF_TOOLS' msgno = 1 msgv1 = 'TEST' ).
  endmethod.


  method add_info.
    log->add_info( message ).

    data(messages) = log->get_messages( ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( messages ) ).
    assert_message( act_message = messages[ 1 ] type = 'I' exp_message = message ).
  endmethod.


  method add_warning.
    log->add_warning( message ).

    data(messages) = log->get_messages( ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( messages ) ).
    assert_message( act_message = messages[ 1 ] type = 'W' exp_message = message ).
  endmethod.


  method add_error.
    log->add_error( message ).

    data(messages) = log->get_messages( ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( messages ) ).
    assert_message( act_message = messages[ 1 ] type = 'E' exp_message = message ).
  endmethod.


  method add_t100_exception.
    data(previous) = new zcx_aff_tools( textid = value #( msgid = 'Z_AFF_TOOLS' msgno = 101 ) ).
    data(exception) = new zcx_aff_tools( textid = value #( msgid = 'Z_AFF_TOOLS' msgno = 100 ) previous = previous ).

    log->add_exception( exception ).

    data(messages) = log->get_messages( ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( messages ) ).
    assert_message( act_message = messages[ 1 ] type = 'E' exp_message = value #( msgid = 'Z_AFF_TOOLS' msgno = 100 ) ).
    assert_message( act_message = messages[ 2 ] type = 'E' exp_message = value #( msgid = 'Z_AFF_TOOLS' msgno = 101 ) ).
  endmethod.


  method add_catched_exception.
    try.
        raise exception type zcx_aff_tools message e100(Z_AFF_TOOLS) with '1' '2' '3' '4'.
      catch zcx_aff_tools into data(exception).
        log->add_exception( exception ).
    endtry.

    data(messages) = log->get_messages( ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( messages ) ).
    assert_message( act_message = messages[ 1 ]
                    type        = 'E'
                    exp_message = value #( msgid = 'Z_AFF_TOOLS' msgno = 100 msgv1 = '1' msgv2 = '2' msgv3 = '3' msgv4 = '4' ) ).
  endmethod.


  method add_classic_exception.
    data(exception) = new ltcx_classic_exception( ).

    log->add_exception( exception ).

    data(messages) = log->get_messages( ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( messages ) ).
    assert_message( act_message = messages[ 1 ]
                    type        = 'E'
                    exp_message = value #( msgid = '00' msgno = 1 msgv1 = exception->get_text( ) ) ).
  endmethod.


  method add_exception_as_info.
    data(previous) = new zcx_aff_tools( textid = value #( msgid = 'Z_AFF_TOOLS' msgno = 101 ) ).
    data(exception) = new zcx_aff_tools( textid = value #( msgid = 'Z_AFF_TOOLS' msgno = 100 ) previous = previous ).

    log->add_exception( exception = exception message_type = zif_aff_log=>c_message_type-info ).

    data(messages) = log->get_messages( ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( messages ) ).
    assert_message( act_message = messages[ 1 ] type = 'I' exp_message = value #( msgid = 'Z_AFF_TOOLS' msgno = 100 ) ).
    assert_message( act_message = messages[ 2 ] type = 'I' exp_message = value #( msgid = 'Z_AFF_TOOLS' msgno = 101 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'I' act = log->get_max_severity( ) ).
  endmethod.


  method joins_log.
    log->add_warning( message ).
    data(log2) = cast zif_aff_log( new zcl_aff_log( ) ).
    log2->add_error( message_2 ).

    log->join( log2 ).

    data(messages) = log->get_messages( ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( messages ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'E' act = log->get_max_severity( ) ).
    assert_message( act_message = messages[ 1 ] type = 'W' exp_message = message ).
    assert_message( act_message = messages[ 2 ] type = 'E' exp_message = message_2 ).
  endmethod.


  method clears_log.
    log->add_error( message ).
    log->clear( ).

    data(messages) = log->get_messages( ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( messages ) ).
  endmethod.


  method get_max_severity.
    cl_abap_unit_assert=>assert_equals( exp = '' act = log->get_max_severity( ) ).

    log->add_info( message ).
    cl_abap_unit_assert=>assert_equals( exp = 'I' act = log->get_max_severity( ) ).

    log->add_warning( message ).
    cl_abap_unit_assert=>assert_equals( exp = 'W' act = log->get_max_severity( ) ).

    log->add_error( message ).
    cl_abap_unit_assert=>assert_equals( exp = 'E' act = log->get_max_severity( ) ).

    log->add_exception( new zcx_aff_tools( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'E' act = log->get_max_severity( ) ).

    log->add_info( message ).
    log->add_warning( message ).
    cl_abap_unit_assert=>assert_equals( exp = 'E' act = log->get_max_severity( ) ).
  endmethod.


  method has_messages.
    cl_abap_unit_assert=>assert_false( log->has_messages( ) ).

    log->add_info( message ).
    cl_abap_unit_assert=>assert_true( log->has_messages( ) ).
  endmethod.


  method two_messages_for_one_object.
    log->add_info( message ).
    log->add_info( message_2 ).

    data(messages) = log->get_messages( ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( messages ) ).
  endmethod.


  method assert_message.
    cl_abap_unit_assert=>assert_equals( exp = type act = act_message-type ).
    cl_abap_unit_assert=>assert_equals( exp = value #( base exp_message msgty = type ) act = act_message-message ).
  endmethod.


  method get_sy_message.
    message i000(z_aff_tools) with '1' '2' '3' '4' into data(message) ##NEEDED.
    data(act_message) = zcl_aff_log=>get_sy_message( ).
    cl_abap_unit_assert=>assert_equals( exp = value symsg( msgid = 'Z_AFF_TOOLS' msgno = 000 msgty = 'I' msgv1 = '1' msgv2 = '2' msgv3 = '3' msgv4 = '4' ) act = act_message ).

    message e001(z_aff_tools) into message.
    act_message = zcl_aff_log=>get_sy_message( ).
    cl_abap_unit_assert=>assert_equals( exp = value symsg( msgid = 'Z_AFF_TOOLS' msgno = 001 msgty = 'E' ) act = act_message ).
  endmethod.

endclass.

class ltcl_aff_root definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      default_message for testing raising cx_static_check,
      exception_with_message for testing raising cx_static_check,
      message_is_supplied for testing raising cx_static_check.
endclass.

class ltcl_aff_root implementation.

  method default_message.
    data(exception) = new zcx_aff_tools( ).

    cl_abap_unit_assert=>assert_equals( exp = 'Z_AFF_TOOLS' act = exception->if_t100_message~t100key-msgid ).
    cl_abap_unit_assert=>assert_equals( exp = 001 act = exception->if_t100_message~t100key-msgno ).
  endmethod.

  method exception_with_message.
    data(exception) = new zcx_aff_tools( textid = value #( msgid = 'Z_AFF_TOOLS' msgno = 100 ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'Z_AFF_TOOLS' act = exception->if_t100_message~t100key-msgid ).
    cl_abap_unit_assert=>assert_equals( exp = 100 act = exception->if_t100_message~t100key-msgno ).
  endmethod.

  method message_is_supplied.
    data message type string.
    message = 'This is a very long test message for testing purposes. It has no further meaning'.
    data(exception) = new zcx_aff_tools( message = message ).

    cl_message_helper=>set_msg_vars_for_clike( message ).

    cl_abap_unit_assert=>assert_equals( exp = 'E' act = exception->if_t100_dyn_msg~msgty ).
    cl_abap_unit_assert=>assert_equals( exp = sy-msgv1 act = exception->if_t100_dyn_msg~msgv1 ).
    cl_abap_unit_assert=>assert_equals( exp = sy-msgv2 act = exception->if_t100_dyn_msg~msgv2 ).
    cl_abap_unit_assert=>assert_equals( exp = sy-msgv3 act = exception->if_t100_dyn_msg~msgv3 ).
    cl_abap_unit_assert=>assert_equals( exp = sy-msgv4 act = exception->if_t100_dyn_msg~msgv4 ).
    cl_abap_unit_assert=>assert_equals( exp = 'Z_AFF_TOOLS' act = exception->if_t100_message~t100key-msgid ).
    cl_abap_unit_assert=>assert_equals( exp = 000 act = exception->if_t100_message~t100key-msgno ).
    cl_abap_unit_assert=>assert_equals( exp = 'IF_T100_DYN_MSG~MSGV1' act = exception->if_t100_message~t100key-attr1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'IF_T100_DYN_MSG~MSGV2' act = exception->if_t100_message~t100key-attr2 ).
    cl_abap_unit_assert=>assert_equals( exp = 'IF_T100_DYN_MSG~MSGV3' act = exception->if_t100_message~t100key-attr3 ).
    cl_abap_unit_assert=>assert_equals( exp = 'IF_T100_DYN_MSG~MSGV4' act = exception->if_t100_message~t100key-attr4 ).
    cl_abap_unit_assert=>assert_equals( exp = message act = exception->get_text( ) ).
  endmethod.

endclass.

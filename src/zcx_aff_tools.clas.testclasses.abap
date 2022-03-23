CLASS ltcl_aff_root DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      default_message FOR TESTING RAISING cx_static_check,
      exception_with_message FOR TESTING RAISING cx_static_check,
      message_is_supplied FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_aff_root IMPLEMENTATION.

  METHOD default_message.
    DATA(exception) = NEW zcx_aff_tools( ).

    cl_abap_unit_assert=>assert_equals( exp = 'ZAFF_TOOLS' act = exception->if_t100_message~t100key-msgid ).
    cl_abap_unit_assert=>assert_equals( exp = 001 act = exception->if_t100_message~t100key-msgno ).
  ENDMETHOD.

  METHOD exception_with_message.
    DATA(exception) = NEW zcx_aff_tools( textid = VALUE #( msgid = 'ZAFF_TOOLS' msgno = 100 ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'ZAFF_TOOLS' act = exception->if_t100_message~t100key-msgid ).
    cl_abap_unit_assert=>assert_equals( exp = 100 act = exception->if_t100_message~t100key-msgno ).
  ENDMETHOD.

  METHOD message_is_supplied.
    DATA message TYPE string.
    message = 'This is a very long test message for testing purposes. It has no further meaning'.
    DATA(exception) = NEW zcx_aff_tools( message = message ).

    cl_message_helper=>set_msg_vars_for_clike( message ).

    cl_abap_unit_assert=>assert_equals( exp = 'E' act = exception->if_t100_dyn_msg~msgty ).
    cl_abap_unit_assert=>assert_equals( exp = sy-msgv1 act = exception->if_t100_dyn_msg~msgv1 ).
    cl_abap_unit_assert=>assert_equals( exp = sy-msgv2 act = exception->if_t100_dyn_msg~msgv2 ).
    cl_abap_unit_assert=>assert_equals( exp = sy-msgv3 act = exception->if_t100_dyn_msg~msgv3 ).
    cl_abap_unit_assert=>assert_equals( exp = sy-msgv4 act = exception->if_t100_dyn_msg~msgv4 ).
    cl_abap_unit_assert=>assert_equals( exp = 'ZAFF_TOOLS' act = exception->if_t100_message~t100key-msgid ).
    cl_abap_unit_assert=>assert_equals( exp = 000 act = exception->if_t100_message~t100key-msgno ).
    cl_abap_unit_assert=>assert_equals( exp = 'IF_T100_DYN_MSG~MSGV1' act = exception->if_t100_message~t100key-attr1 ).
    cl_abap_unit_assert=>assert_equals( exp = 'IF_T100_DYN_MSG~MSGV2' act = exception->if_t100_message~t100key-attr2 ).
    cl_abap_unit_assert=>assert_equals( exp = 'IF_T100_DYN_MSG~MSGV3' act = exception->if_t100_message~t100key-attr3 ).
    cl_abap_unit_assert=>assert_equals( exp = 'IF_T100_DYN_MSG~MSGV4' act = exception->if_t100_message~t100key-attr4 ).
    cl_abap_unit_assert=>assert_equals( exp = message act = exception->get_text( ) ).
  ENDMETHOD.

ENDCLASS.

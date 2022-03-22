class zcx_aff_tools definition
  public
  inheriting from cx_static_check
  create public.

  public section.
    interfaces if_t100_dyn_msg .
    interfaces if_t100_message .

    methods constructor
      importing
        message   type string optional
        !textid   like if_t100_message=>t100key optional
        !previous like previous optional.

  protected section.
  private section.

endclass.

class zcx_aff_tools implementation.

  method constructor ##ADT_SUPPRESS_GENERATION.
    " trigger task
    call method super->constructor
      exporting
        previous = previous.
    clear me->textid.
    if message is not initial.
      cl_message_helper=>set_msg_vars_for_clike( message ).
      if_t100_message~t100key = value #( msgid = 'Z_AFF_TOOLS'
                                         msgno = 000
                                         attr1 = 'IF_T100_DYN_MSG~MSGV1'
                                         attr2 = 'IF_T100_DYN_MSG~MSGV2'
                                         attr3 = 'IF_T100_DYN_MSG~MSGV3'
                                         attr4 = 'IF_T100_DYN_MSG~MSGV4' ).
      if_t100_dyn_msg~msgty = 'E'.
      if_t100_dyn_msg~msgv1 = sy-msgv1.
      if_t100_dyn_msg~msgv2 = sy-msgv2.
      if_t100_dyn_msg~msgv3 = sy-msgv3.
      if_t100_dyn_msg~msgv4 = sy-msgv4.
    elseif textid is initial.
      if_t100_message~t100key = value #( msgid = 'Z_AFF_TOOLS' msgno = 001 ).
    else.
      if_t100_message~t100key = textid.
    endif.
  endmethod.

endclass.

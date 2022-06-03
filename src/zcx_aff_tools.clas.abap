CLASS zcx_aff_tools DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    METHODS constructor
      IMPORTING
        message   TYPE string OPTIONAL
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS zcx_aff_tools IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    " trigger task
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF message IS NOT INITIAL.
      cl_message_helper=>set_msg_vars_for_clike( message ).
      if_t100_message~t100key = VALUE #( attr1 = 'IF_T100_DYN_MSG~MSGV1'
                                         attr2 = 'IF_T100_DYN_MSG~MSGV2'
                                         attr3 = 'IF_T100_DYN_MSG~MSGV3'
                                         attr4 = 'IF_T100_DYN_MSG~MSGV4' ).
      if_t100_dyn_msg~msgty = 'E'.
      if_t100_dyn_msg~msgv1 = sy-msgv1.
      if_t100_dyn_msg~msgv2 = sy-msgv2.
      if_t100_dyn_msg~msgv3 = sy-msgv3.
      if_t100_dyn_msg~msgv4 = sy-msgv4.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

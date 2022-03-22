class zcl_aff_log definition
  public
  final
  create public.

  public section.
    interfaces zif_aff_log.

    class-methods:
      "! Writes the actual system message fields into the returned structure
      "!
      "! @parameter result | The actual system message
      get_sy_message
        returning value(result) type symsg.

  protected section.

  private section.
    data:
      messages     type zif_aff_log=>tt_log_out,
      max_severity type symsgty.

    methods:
      add_message
        importing
          type    type symsgty
          message type symsg,
      set_max_severity
        importing
          type type symsgty.
endclass.


class zcl_aff_log implementation.


  method zif_aff_log~get_messages.
    messages = me->messages.
  endmethod.


  method zif_aff_log~add_info.
    set_max_severity( zif_aff_log=>c_message_type-info ).
    add_message( type = zif_aff_log=>c_message_type-info message = message ).
  endmethod.


  method zif_aff_log~add_warning.
    set_max_severity( zif_aff_log=>c_message_type-warning ).
    add_message( type = zif_aff_log=>c_message_type-warning message = message ).
  endmethod.


  method zif_aff_log~add_error.
    set_max_severity( zif_aff_log=>c_message_type-error ).
    add_message( type = zif_aff_log=>c_message_type-error message = message ).
  endmethod.


  method zif_aff_log~add_exception.
    set_max_severity( message_type ).

    if exception->get_text( ) is not initial.
      cl_message_helper=>set_msg_vars_for_if_msg( exception ).
      add_message( type = message_type message = get_sy_message( ) ).
    endif.

    if exception->previous is bound.
      zif_aff_log~add_exception( exception = exception->previous message_type = message_type ).
    endif.
  endmethod.


  method add_message.

    message
      id message-msgid
      type type
      number message-msgno
      with message-msgv1 message-msgv2 message-msgv3 message-msgv4
      into data(text).

    append value #( type    = type
                    text    = text
                    message = get_sy_message( ) ) to me->messages.
  endmethod.


  method zif_aff_log~join.
    append lines of log_to_join->get_messages( ) to me->messages.
    set_max_severity( log_to_join->get_max_severity( ) ).
  endmethod.


  method zif_aff_log~clear.
    clear me->messages.
  endmethod.


  method zif_aff_log~get_max_severity.
    max_severity = me->max_severity.
  endmethod.


  method zif_aff_log~has_messages.
    has_messages = boolc( me->messages is not initial ).
  endmethod.


  method get_sy_message.
    result = value #(
      msgid = sy-msgid
      msgno = sy-msgno
      msgty = sy-msgty
      msgv1 = sy-msgv1
      msgv2 = sy-msgv2
      msgv3 = sy-msgv3
      msgv4 = sy-msgv4 ).
  endmethod.


  method set_max_severity.
    if type = zif_aff_log=>c_message_type-error.
      max_severity = zif_aff_log=>c_message_type-error.
    elseif type = zif_aff_log=>c_message_type-warning.
      if max_severity <> zif_aff_log=>c_message_type-error.
        max_severity = zif_aff_log=>c_message_type-warning.
      endif.
    elseif type = zif_aff_log=>c_message_type-info.
      if max_severity is initial.
        max_severity = zif_aff_log=>c_message_type-info.
      endif.
    endif.
  endmethod.

endclass.

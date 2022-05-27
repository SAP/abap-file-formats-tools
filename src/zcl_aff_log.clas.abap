CLASS zcl_aff_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_aff_log.

    CLASS-METHODS:
      "! Writes the actual system message fields into the returned structure
      "!
      "! @parameter result | The actual system message
      get_sy_message
        RETURNING VALUE(result) TYPE symsg.
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      messages     TYPE zif_aff_log=>tt_log_out,
      max_severity TYPE symsgty.

    METHODS:
      add_message
        IMPORTING
          type           TYPE symsgty
          message        TYPE symsg
          component_name TYPE string,
      set_max_severity
        IMPORTING
          type TYPE symsgty.
ENDCLASS.



CLASS ZCL_AFF_LOG IMPLEMENTATION.


  METHOD zif_aff_log~get_messages.
    messages = me->messages.
  ENDMETHOD.


  METHOD zif_aff_log~add_info.
    set_max_severity( zif_aff_log=>c_message_type-info ).
    add_message( type = zif_aff_log=>c_message_type-info message = message component_name = component_name ).
  ENDMETHOD.


  METHOD zif_aff_log~add_warning.
    set_max_severity( zif_aff_log=>c_message_type-warning ).
    add_message( type = zif_aff_log=>c_message_type-warning message = message component_name = component_name ).
  ENDMETHOD.


  METHOD zif_aff_log~add_error.
    set_max_severity( zif_aff_log=>c_message_type-error ).
    add_message( type = zif_aff_log=>c_message_type-error message = message component_name = component_name ).
  ENDMETHOD.


  METHOD zif_aff_log~add_exception.
    set_max_severity( message_type ).

    IF exception->get_text( ) IS NOT INITIAL.
      cl_message_helper=>set_msg_vars_for_if_msg( exception ).
      add_message( type = message_type message = get_sy_message( ) component_name = component_name ).
    ENDIF.

    IF exception->previous IS BOUND.
      zif_aff_log~add_exception( exception = exception->previous message_type = message_type component_name = component_name ).
    ENDIF.
  ENDMETHOD.


  METHOD add_message.

    MESSAGE
      ID message-msgid
      TYPE type
      NUMBER message-msgno
      WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4
      INTO DATA(text).

    APPEND VALUE #( component_name = component_name
                    type         = type
                    text         = text
                    message      = get_sy_message( ) ) TO me->messages.
  ENDMETHOD.

  METHOD zif_aff_log~add_message_dev.
    set_max_severity( type ).

    APPEND VALUE #( component_name = component_name
                    type         = type
                    text         = message
                    message      = value #( ) ) TO me->messages.
  ENDMETHOD.

  METHOD zif_aff_log~join.
    APPEND LINES OF log_to_join->get_messages( ) TO me->messages.
    set_max_severity( log_to_join->get_max_severity( ) ).
  ENDMETHOD.


  METHOD zif_aff_log~clear.
    CLEAR me->messages.
  ENDMETHOD.


  METHOD zif_aff_log~get_max_severity.
    max_severity = me->max_severity.
  ENDMETHOD.


  METHOD zif_aff_log~has_messages.
    has_messages = xsdbool( me->messages IS NOT INITIAL ).
  ENDMETHOD.


  METHOD get_sy_message.
    result = VALUE #(
      msgid = sy-msgid
      msgno = sy-msgno
      msgty = sy-msgty
      msgv1 = sy-msgv1
      msgv2 = sy-msgv2
      msgv3 = sy-msgv3
      msgv4 = sy-msgv4 ).
  ENDMETHOD.


  METHOD set_max_severity.
    IF type = zif_aff_log=>c_message_type-error.
      max_severity = zif_aff_log=>c_message_type-error.
    ELSEIF type = zif_aff_log=>c_message_type-warning.
      IF max_severity <> zif_aff_log=>c_message_type-error.
        max_severity = zif_aff_log=>c_message_type-warning.
      ENDIF.
    ELSEIF type = zif_aff_log=>c_message_type-info.
      IF max_severity IS INITIAL.
        max_severity = zif_aff_log=>c_message_type-info.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_aff_log~add_message_dev.
    set_max_severity( type ).

    APPEND VALUE #( component_name = component_name
                    type         = type
                    text         = message
                    message      = value #( ) ) TO me->messages.
  ENDMETHOD.
ENDCLASS.

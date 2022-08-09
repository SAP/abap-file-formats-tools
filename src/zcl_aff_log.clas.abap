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

    METHODS:
      constructor.
  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_msg,
             msgno TYPE i,
             str1  TYPE c LENGTH 50,
             str2  TYPE c LENGTH 50,
             str3  TYPE c LENGTH 50,
             str4  TYPE c LENGTH 50,
           END OF ty_msg.

    TYPES: tt_msg TYPE STANDARD TABLE OF ty_msg WITH DEFAULT KEY.

    DATA:
      messages      TYPE zif_aff_log=>tt_log_out,
      message_table TYPE tt_msg,
      max_severity  TYPE symsgty.


    METHODS:
      add_message_for_exception
        IMPORTING
          type           TYPE symsgty
          message        TYPE symsg
          component_name TYPE string,
      add_message
        IMPORTING
          type           TYPE symsgty
          message_text   TYPE string
          component_name TYPE string,
      set_max_severity
        IMPORTING
          type TYPE symsgty.
ENDCLASS.



CLASS ZCL_AFF_LOG IMPLEMENTATION.


  METHOD add_message.
    set_max_severity( type ).
    APPEND VALUE #( component_name = component_name
                    type         = type
                    message_text = message_text ) TO me->messages.
  ENDMETHOD.


  METHOD add_message_for_exception.

    MESSAGE
      ID message-msgid
      TYPE type
      NUMBER message-msgno
      WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4
      INTO DATA(text).

    APPEND VALUE #( component_name = component_name
                    type           = type
                    message_text   = text
                    message        = get_sy_message( ) ) TO me->messages.
  ENDMETHOD.


  METHOD constructor.
    APPEND VALUE #( msgno = 0 ) TO message_table.
    APPEND VALUE #( msgno = 102 str1 = `The JSON type`  str2 = `is not supported by the XSLT writer` ) TO message_table ##NO_TEXT.
    APPEND VALUE #( msgno = 103 str1 = `Class/Interface type`  str2 = `given in ABAP Doc link doesn't exist` ) TO message_table ##NO_TEXT.
    APPEND VALUE #( msgno = 104 str1 = `Constant`  str2 = `given in ABAP Doc link doesn't exist` ) TO message_table ##NO_TEXT.
    APPEND VALUE #( msgno = 105 str1 = `Component`  str2 = `of constant` str3 = `in ABAP Doc link doesn't exist` ) TO message_table ##NO_TEXT.
    APPEND VALUE #( msgno = 107 str1 = `There are several occurrences of annotation`  str2 = `. First valid is used` ) TO message_table ##NO_TEXT.
    APPEND VALUE #( msgno = 108 str1 = `Annotation`  str2 = `is unknown` ) TO message_table ##NO_TEXT.
    APPEND VALUE #( msgno = 109 str1 = `Annotation`  str2 = `was used incorrectly` ) TO message_table ##NO_TEXT.
    APPEND VALUE #( msgno = 110 str1 = `No number was provided for annotation` ) TO message_table ##NO_TEXT.
    APPEND VALUE #( msgno = 111 str1 = `Link in annotation`  str2 = `is incorrect` ) TO message_table ##NO_TEXT.
    APPEND VALUE #( msgno = 112 str1 = `If $required is set, $showAlways is redundant` ) TO message_table ##NO_TEXT.
    APPEND VALUE #( msgno = 117 str1 = `Annotation $default for type`  str2 = `is not supported` ) TO message_table ##NO_TEXT.
    APPEND VALUE #( msgno = 119 str1 = ``  str2 = `is missing` ) TO message_table ##NO_TEXT.
    APPEND VALUE #( msgno = 122 str1 = `Type of constant`  str2 = `does not match type of` ) TO message_table ##NO_TEXT.
    APPEND VALUE #( msgno = 125 str1 = `Description exceeds`  str2 = `characters and might be too long` ) TO message_table ##NO_TEXT.
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


  METHOD zif_aff_log~add_error.
    add_message( type = zif_aff_log=>c_message_type-error message_text = message_text component_name = component_name ).
  ENDMETHOD.


  METHOD zif_aff_log~add_exception.
    set_max_severity( message_type ).

    IF exception->get_text( ) IS NOT INITIAL.
      cl_message_helper=>set_msg_vars_for_if_msg( exception ).
      add_message_for_exception( type = message_type message = get_sy_message( ) component_name = component_name ).
    ENDIF.

    IF exception->previous IS BOUND.
      zif_aff_log~add_exception( exception = exception->previous message_type = message_type component_name = component_name ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_aff_log~add_info.
    add_message( type = zif_aff_log=>c_message_type-info message_text = message_text component_name = component_name ).
  ENDMETHOD.


  METHOD zif_aff_log~add_warning.
    add_message( type = zif_aff_log=>c_message_type-warning message_text = message_text component_name = component_name ).
  ENDMETHOD.


  METHOD zif_aff_log~clear.
    CLEAR me->messages.
  ENDMETHOD.


  METHOD zif_aff_log~get_max_severity.
    max_severity = me->max_severity.
  ENDMETHOD.


  METHOD zif_aff_log~get_messages.
    messages = me->messages.
  ENDMETHOD.


  METHOD zif_aff_log~get_message_text.
    IF line_exists( message_table[ msgno = msgno ] ).
      DATA(message_entry) = VALUE #( message_table[ msgno = msgno ] ).
      message_text = message_entry-str1 && ` ` && msgv1 && ` ` &&
                message_entry-str2 && ` ` && msgv2 && ` ` &&
                message_entry-str3 && ` ` && msgv3 && ` ` &&
                message_entry-str4 && ` ` && msgv4.
      CONDENSE message_text.
    ENDIF.
  ENDMETHOD.


  METHOD zif_aff_log~has_messages.
    has_messages = xsdbool( me->messages IS NOT INITIAL ).
  ENDMETHOD.


  METHOD zif_aff_log~join.
    APPEND LINES OF log_to_join->get_messages( ) TO me->messages.
    set_max_severity( log_to_join->get_max_severity( ) ).
  ENDMETHOD.
ENDCLASS.

CLASS zcl_aff_tools_unit_test_helper DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS:
      "! Checks whether the message is contained in the log.
      assert_log_contains_msg
        IMPORTING
          log                TYPE REF TO zif_aff_log
          exp_message        TYPE scx_t100key
          exp_type           TYPE symsgty DEFAULT zif_aff_log=>c_message_type-error
          exp_component_name TYPE string OPTIONAL,

      "! Checks whether the text string is contained in the log.
      assert_log_contains_text
        IMPORTING
          log                TYPE REF TO zif_aff_log
          exp_text           TYPE string
          exp_type           TYPE symsgty DEFAULT zif_aff_log=>c_message_type-error
          exp_component_name TYPE string OPTIONAL,

      "! Asserts that both string tables are equal, ignoring all spaces.
      assert_equals_ignore_spaces
        IMPORTING
          act_data TYPE string_table
          exp_data TYPE string_table,

      "! Asserts that no message with a severity higher or equal than the threshold is contained in the log.
      "! If a corresponding message is contained, a critical assertion is thrown and the messages are displayed in the details
      "!
      "! @parameter log | The log
      "! @parameter message_severity_threshold | Severity threshold. All messages with higher or equal severity are reported
      assert_log_has_no_message
        IMPORTING
          log                        TYPE REF TO zif_aff_log
          message_severity_threshold TYPE symsgty DEFAULT zif_aff_log=>c_message_type-warning,

      "! Asserts that string tables are equal.
      assert_string_tabs
        IMPORTING
          exp               TYPE string_table
          act               TYPE string_table
          ignore_case       TYPE abap_bool DEFAULT abap_false
          ignore_spaces     TYPE abap_bool DEFAULT abap_false
          ignore_linebreaks TYPE abap_bool DEFAULT abap_false.

  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_aff_tools_unit_test_helper IMPLEMENTATION.

  METHOD assert_log_contains_msg.
    DATA(act_messages) = log->get_messages( ).
    DATA(msg) = VALUE symsg(
      msgid = exp_message-msgid
      msgno = exp_message-msgno
      msgty = exp_type
      msgv1 = exp_message-attr1
      msgv2 = exp_message-attr2
      msgv3 = exp_message-attr3
      msgv4 = exp_message-attr4 ).
    IF exp_component_name IS SUPPLIED.
      IF NOT line_exists( act_messages[ type = exp_type message = msg component_name = exp_component_name ] ).
        cl_abap_unit_assert=>fail( msg = 'The expected message is not contained in the log' ).
      ENDIF.
    ELSEIF NOT line_exists( act_messages[ type = exp_type message = msg ] ).
      cl_abap_unit_assert=>fail( msg = 'The expected message is not contained in the log' ).
    ENDIF.
  ENDMETHOD.


  METHOD assert_log_contains_text.
    DATA(act_messages) = log->get_messages( ).
    IF exp_component_name IS SUPPLIED.
      IF NOT line_exists( act_messages[ type = exp_type message_text = exp_text component_name = exp_component_name ] ).
        cl_abap_unit_assert=>fail( msg = 'The expected message is not contained in the log' ).
      ENDIF.
    ELSEIF NOT line_exists( act_messages[ type = exp_type message_text = exp_text ] ).
      cl_abap_unit_assert=>fail( msg = 'The expected message is not contained in the log' ).
    ENDIF.

  ENDMETHOD.

  METHOD assert_log_has_no_message.
    DATA types_to_report TYPE STANDARD TABLE OF symsgty.

    CASE message_severity_threshold.
      WHEN zif_aff_log=>c_message_type-info.
        types_to_report = VALUE #( ( zif_aff_log=>c_message_type-info ) ( zif_aff_log=>c_message_type-warning ) ( zif_aff_log=>c_message_type-error ) ).
      WHEN zif_aff_log=>c_message_type-warning.
        types_to_report = VALUE #( ( zif_aff_log=>c_message_type-warning ) ( zif_aff_log=>c_message_type-error ) ).
      WHEN zif_aff_log=>c_message_type-error.
        types_to_report = VALUE #( ( zif_aff_log=>c_message_type-error ) ).
      WHEN OTHERS.
        types_to_report = VALUE #( ( zif_aff_log=>c_message_type-info ) ( zif_aff_log=>c_message_type-warning ) ( zif_aff_log=>c_message_type-error ) ).
    ENDCASE.

    DATA(max_severity) = log->get_max_severity( ).
    IF line_exists( types_to_report[ table_line = max_severity ] ).
      DATA detail TYPE string.
      LOOP AT log->get_messages( ) ASSIGNING FIELD-SYMBOL(<message>).
        IF line_exists( types_to_report[ table_line = <message>-type ] ).
          detail = detail && <message>-message_text && cl_abap_char_utilities=>newline.
        ENDIF.
      ENDLOOP.
      cl_abap_unit_assert=>fail( msg = |Log contains messages with severity >= { message_severity_threshold }| detail = detail ).
    ENDIF.
  ENDMETHOD.


  METHOD assert_equals_ignore_spaces.
    DATA(exp) = exp_data.
    DATA(act) = act_data.
    LOOP AT exp ASSIGNING FIELD-SYMBOL(<exp_line>).
      CONDENSE <exp_line> NO-GAPS.
    ENDLOOP.
    LOOP AT act ASSIGNING FIELD-SYMBOL(<act_line>).
      CONDENSE <act_line> NO-GAPS.
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals( exp = exp act = act ).
  ENDMETHOD.


  METHOD assert_string_tabs.
    DATA(exp_work_copy) = exp.
    DATA(act_work_copy) = act.

    IF ignore_spaces = abap_true.
      LOOP AT exp_work_copy ASSIGNING FIELD-SYMBOL(<exp_line>).
        CONDENSE <exp_line> NO-GAPS.
      ENDLOOP.
      LOOP AT act_work_copy ASSIGNING FIELD-SYMBOL(<act_line>).
        CONDENSE <act_line> NO-GAPS.
      ENDLOOP.
    ENDIF.

    IF ignore_case = abap_true.
      LOOP AT exp_work_copy ASSIGNING <exp_line>.
        <exp_line> = to_lower( <exp_line> ).
      ENDLOOP.
      LOOP AT act_work_copy ASSIGNING <act_line>.
        <act_line> = to_lower( <act_line> ).
      ENDLOOP.
    ENDIF.

    IF ignore_linebreaks = abap_true.
      CONCATENATE LINES OF exp_work_copy INTO DATA(exp_string).
      CONCATENATE LINES OF act_work_copy INTO DATA(act_string).
      cl_abap_unit_assert=>assert_equals( exp = exp_string act = act_string msg = 'Expected and actual abap source does not match' ).
      RETURN.
    ENDIF.

    cl_abap_unit_assert=>assert_equals( exp = exp_work_copy act = act_work_copy msg = 'Expected and actual abap source does not match' ).
  ENDMETHOD.


ENDCLASS.

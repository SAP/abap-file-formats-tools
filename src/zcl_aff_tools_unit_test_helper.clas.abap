class zcl_aff_tools_unit_test_helper definition final for testing
  duration short
  risk level harmless
  public.

  public section.

    class-methods:
      "! Checks whether the message is contained in the log.
      assert_log_contains_msg
        importing
          log         type ref to zif_aff_log
          exp_message type scx_t100key
          exp_type    type symsgty default zif_aff_log=>c_message_type-error,

      "! Asserts that both string tables are equal, ignoring all spaces.
      assert_equals_ignore_spaces
        importing
          act_data type string_table
          exp_data type string_table,

      "! Asserts that no message with a severity higher or equal than the threshold is contained in the log.
      "! If a corresponding message is contained, a critical assertion is thrown and the messages are displayed in the details
      "!
      "! @parameter log | The log
      "! @parameter message_severity_threshold | Severity threshold. All messages with higher or equal severity are reported
      assert_log_has_no_message
        importing
          log                        type ref to zif_aff_log
          message_severity_threshold type symsgty default zif_aff_log=>c_message_type-warning,

      "! Asserts that string tables are equal.
      assert_string_tabs
        importing
          exp               type string_table
          act               type string_table
          ignore_case       type abap_bool default abap_false
          ignore_spaces     type abap_bool default abap_false
          ignore_linebreaks type abap_bool default abap_false.

  protected section.
  private section.
endclass.



class zcl_aff_tools_unit_test_helper implementation.


  method assert_log_contains_msg.
    data(act_messages) = log->get_messages( ).
    data(msg) = value symsg(
      msgid = exp_message-msgid
      msgno = exp_message-msgno
      msgty = exp_type
      msgv1 = exp_message-attr1
      msgv2 = exp_message-attr2
      msgv3 = exp_message-attr3
      msgv4 = exp_message-attr4 ).
    if not line_exists( act_messages[ type = exp_type message = msg ] ).
      cl_abap_unit_assert=>fail( msg = 'The expected message is not contained in the log' ).
    endif.
  endmethod.

  method assert_log_has_no_message.
    data types_to_report type standard table of symsgty.

    case message_severity_threshold.
      when if_aff_log=>c_message_type-info.
        types_to_report = value #( ( if_aff_log=>c_message_type-info ) ( if_aff_log=>c_message_type-warning ) ( if_aff_log=>c_message_type-error ) ).
      when if_aff_log=>c_message_type-warning.
        types_to_report = value #( ( if_aff_log=>c_message_type-warning ) ( if_aff_log=>c_message_type-error ) ).
      when if_aff_log=>c_message_type-error.
        types_to_report = value #( ( if_aff_log=>c_message_type-error ) ).
      when others.
        types_to_report = value #( ( if_aff_log=>c_message_type-info ) ( if_aff_log=>c_message_type-warning ) ( if_aff_log=>c_message_type-error ) ).
    endcase.

    data(max_severity) = log->get_max_severity( ).
    if line_exists( types_to_report[ table_line = max_severity ] ).
      data detail type string.
      loop at log->get_messages( ) assigning field-symbol(<message>).
        if line_exists( types_to_report[ table_line = <message>-type ] ).
          detail = detail && <message>-text && cl_abap_char_utilities=>newline.
        endif.
      endloop.
      cl_abap_unit_assert=>fail( msg = |Log contains messages with severity >= { message_severity_threshold }| detail = detail ).
    endif.
  endmethod.


  method assert_equals_ignore_spaces.
    data(exp) = exp_data.
    data(act) = act_data.
    loop at exp assigning field-symbol(<exp_line>).
      condense <exp_line> no-gaps.
    endloop.
    loop at act assigning field-symbol(<act_line>).
      condense <act_line> no-gaps.
    endloop.
    cl_abap_unit_assert=>assert_equals( exp = exp act = act ).
  endmethod.


  method assert_string_tabs.
    data(exp_work_copy) = exp.
    data(act_work_copy) = act.

    if ignore_spaces = abap_true.
      loop at exp_work_copy assigning field-symbol(<exp_line>).
        condense <exp_line> no-gaps.
      endloop.
      loop at act_work_copy assigning field-symbol(<act_line>).
        condense <act_line> no-gaps.
      endloop.
    endif.

    if ignore_case = abap_true.
      loop at exp_work_copy assigning <exp_line>.
        <exp_line> = to_lower( <exp_line> ).
      endloop.
      loop at act_work_copy assigning <act_line>.
        <act_line> = to_lower( <act_line> ).
      endloop.
    endif.

    if ignore_linebreaks = abap_true.
      concatenate lines of exp_work_copy into data(exp_string).
      concatenate lines of act_work_copy into data(act_string).
      cl_abap_unit_assert=>assert_equals( exp = exp_string act = act_string msg = 'Expected and actual abap source does not match' ).
      return.
    endif.

    cl_abap_unit_assert=>assert_equals( exp = exp_work_copy act = act_work_copy msg = 'Expected and actual abap source does not match' ).
  endmethod.


endclass.

class zcl_aff_abap_doc_parser definition
  public
  final
  create public .

  public section.
    constants: begin of abap_doc_annotation,
                 callback_class    type string value `$callbackClass`,
                 default           type string value `$default`,
                 values            type string value `$values`,
                 required          type string value `$required`,
                 show_always       type string value `$showAlways`,
                 minimum           type string value `$minimum`,
                 maximum           type string value `$maximum`,
                 exclusive_minimum type string value `$exclusiveMinimum`,
                 exclusive_maximum type string value `$exclusiveMaximum`,
                 max_length        type string value `$maxLength`,
                 min_length        type string value `$minLength`,
                 multiple_of       type string value `$multipleOf`,
               end of abap_doc_annotation.

    types:
      begin of abap_doc,
        required          type abap_bool,
        showalways        type abap_bool,
        title             type string,
        description       type string,
        enumvalues_link   type string,
        minimum           type string,
        maximum           type string,
        exclusive_minimum type string,
        exclusive_maximum type string,
        multiple_of       type string,
        default           type string,
        min_length        type string,
        max_length        type string,
        callback_class    type string,
      end of abap_doc.

    methods: parse
      importing
        component_name        type string
        to_parse              type string
      changing
        log                   type ref to if_aff_log
      returning
        value(found_abap_doc) type abap_doc.

  protected section.
  private section.
    types:
      begin of ty_mixed_table_entry,
        offset  type i,
        length  type i,
        is_link type abap_boolean,
      end of ty_mixed_table_entry,
      tt_mixed_table_entry type sorted table of ty_mixed_table_entry with unique key offset.

    data abap_doc_string type string.
    data parser_log type ref to if_aff_log.
    data component_name type string.
    data decoded_abap_doc type abap_doc.
    data description_warning_is_needed type abap_boolean.

    methods: parse_title,
      parse_description,
      remove_leading_trailing_spaces
        changing string_to_work_on type string,
      parse_annotations,
      parse_callback_class,
      get_annotation_value
        importing
          length                  type i
          offset                  type i
          to_decode               type string
          length_of_annotation    type i
          remove_whitespaces      type abap_boolean
        returning
          value(annotation_value) type string,
      parse_default,
      parse_enum_values,
      parse_required,
      parse_show_always,
      parse_number_annotations
        importing
          key_word type string,
      get_number_annotation
        importing
          annotation_name type string
        returning
          value(number)   type string,
      check_next_word
        importing
          offset        type i
          text_to_check type string,
      write_description_message.

endclass.


class zcl_aff_abap_doc_parser implementation.


  method parse.
    clear description_warning_is_needed.
    clear decoded_abap_doc.
    abap_doc_string = to_parse.
    me->component_name =  component_name.
    parser_log = log.
    parse_title( ).
    parse_annotations( ).
    parse_description( ).
    found_abap_doc = decoded_abap_doc.
    write_description_message( ).
  endmethod.


  method parse_title.
    replace all occurrences of pcre `[\s]*(<p[\s]+class="shorttext([\s]+synchronized)?"([\s]+lang="[a-zA-Z]{2}")?[\s]*>)[\s]*`
        in abap_doc_string with `<p class="shorttext">` ##NO_TEXT.
    find all occurrences of pcre `<p\sclass="shorttext">.*?</p>` in abap_doc_string results data(result_table).
    if lines( result_table ) = 0.
      return.
    elseif lines( result_table ) > 1.
      message i107(saff_core) with `'Title'` component_name into data(message) ##NEEDED.
      parser_log->add_info( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
    endif.
    data(offset) = result_table[ 1 ]-offset.
    if offset <> 0.
      message i113(saff_core) with component_name into message ##NEEDED.
      parser_log->add_info( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
    endif.
    data(length) = result_table[ 1 ]-length.
    data(title) = abap_doc_string+offset(length).
    replace `</p>` in title with ``.
    replace `<p class="shorttext">` in title with ``.
    remove_leading_trailing_spaces( changing string_to_work_on = title ).
    decoded_abap_doc-title = title.
    replace all occurrences of pcre `[\s]*<p\sclass="shorttext">.*?</p>[\s]*` in abap_doc_string with ``.
  endmethod.


  method parse_description.
    find first occurrence of pcre `(\$callbackClass|\$default|\$values|\$required|\$showAlways|\$minimum|\$maximum|\$exclusiveMinimum|\$exclusiveMaximum|\$multipleOf|\$maxLength|\$minLength)`
      in abap_doc_string match offset data(offset).
    if sy-subrc = 0.
      data(description) = abap_doc_string+0(offset).
      remove_leading_trailing_spaces( changing string_to_work_on = description ).
      decoded_abap_doc-description = description.
    else.
      remove_leading_trailing_spaces( changing string_to_work_on = abap_doc_string ).
      decoded_abap_doc-description = abap_doc_string.
    endif.
  endmethod.


  method parse_annotations.
    find all occurrences of pcre `\$[a-zA-Z]+` in abap_doc_string results data(result_table) ##NO_TEXT.
    data(modified_abap_doc_string) = abap_doc_string.
    loop at result_table assigning field-symbol(<entry>).
      data(offset) = <entry>-offset.
      data(length) = <entry>-length.
      data(key_word) = abap_doc_string+offset(length).
      case key_word.
        when abap_doc_annotation-callback_class.
          parse_callback_class( ).
        when abap_doc_annotation-default.
          parse_default( ).
        when abap_doc_annotation-values.
          parse_enum_values( ).
        when abap_doc_annotation-required.
          parse_required( ).
        when abap_doc_annotation-show_always.
          parse_show_always( ).
        when abap_doc_annotation-minimum or abap_doc_annotation-maximum or abap_doc_annotation-exclusive_minimum or abap_doc_annotation-exclusive_maximum
             or abap_doc_annotation-max_length or abap_doc_annotation-multiple_of or abap_doc_annotation-min_length.
          parse_number_annotations( key_word = key_word ).
        when others.
          replace key_word in modified_abap_doc_string with ''.
          message w108(saff_core) with key_word component_name into data(message) ##NEEDED.
          parser_log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
      endcase.
    endloop.
    abap_doc_string = modified_abap_doc_string.
  endmethod.

  method parse_callback_class.
    if decoded_abap_doc-callback_class is not initial.
      return.
    endif.
    data(string_to_parse) = abap_doc_string.
    replace all occurrences of pcre `\$callbackClass[\s]*(:[\s]*)?\{[\s]*@link` in string_to_parse with `\$callbackClass\{@link`.
    find all occurrences of pcre `\$callbackClass\{@link[^\}]+\}` in string_to_parse results data(result_table).
    if lines( result_table ) = 0.
      message w109(saff_core) with abap_doc_annotation-callback_class component_name into data(message) ##NEEDED.
      parser_log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
      return.
    endif.
    if lines( result_table ) > 1.
      message i107(saff_core) with abap_doc_annotation-callback_class component_name into message ##NEEDED.
      parser_log->add_info( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
    endif.
    data(offset_found) = result_table[ 1 ]-offset.
    data(length_found) = result_table[ 1 ]-length.
    decoded_abap_doc-callback_class = get_annotation_value( length = length_found - 1 offset = offset_found to_decode = string_to_parse length_of_annotation = 20 remove_whitespaces = abap_true ).
    loop at result_table assigning field-symbol(<entry>).
      check_next_word( offset = <entry>-offset + <entry>-length text_to_check = string_to_parse ).
    endloop.
  endmethod.

  method get_annotation_value.
    data(step) = offset + length_of_annotation.
    data(length_of_annotation_value) = length - length_of_annotation.
    data(value) = to_decode+step(length_of_annotation_value).
    if remove_whitespaces = abap_true.
      remove_leading_trailing_spaces( changing string_to_work_on = value ).
    endif.
    annotation_value = value.
  endmethod.


  method parse_default.
    if decoded_abap_doc-default is not initial.
      return.
    endif.
    data(string_to_parse) = abap_doc_string.
    replace all occurrences of pcre `\$default[\s]*(:[\s]*)?'` in string_to_parse with `\$default'`.
    replace all occurrences of pcre `\$default[\s]*(:[\s]*)?\{[\s]*@link` in string_to_parse with `\$default\{@link`.

    find all occurrences of pcre `\$default'[^']*'` in string_to_parse results data(result_table_value).
    find all occurrences of pcre `\$default\{@link[^\}]+\}` in string_to_parse results data(result_table_link).

    data mixed_result_table type tt_mixed_table_entry.
    loop at result_table_value assigning field-symbol(<default_value>).
      insert value #( offset = <default_value>-offset length = <default_value>-length is_link = abap_false ) into table mixed_result_table.
    endloop.
    loop at result_table_link assigning field-symbol(<default_link>).
      insert value #( offset = <default_link>-offset length = <default_link>-length is_link = abap_true ) into table mixed_result_table.
    endloop.

    if lines( mixed_result_table ) = 0.
      message w109(saff_core) with abap_doc_annotation-default component_name into data(message) ##NEEDED.
      parser_log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
      return.
    endif.
    if lines( mixed_result_table ) > 1.
      message i107(saff_core) with abap_doc_annotation-default component_name into message ##NEEDED.
      parser_log->add_info( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
    endif.
    data(warning_set) = abap_false.
    loop at mixed_result_table assigning field-symbol(<entry>).
      check_next_word( offset = <entry>-offset + <entry>-length text_to_check = string_to_parse ).
      if <entry>-is_link = abap_false and decoded_abap_doc-default is initial.
        decoded_abap_doc-default = `"` && get_annotation_value( length = <entry>-length - 1 offset = <entry>-offset to_decode = string_to_parse length_of_annotation = 9 remove_whitespaces = abap_false ) && `"`.
      elseif <entry>-is_link = abap_true and decoded_abap_doc-default is initial.
        data(link) = get_annotation_value( length =  <entry>-length - 1 offset = <entry>-offset to_decode = string_to_parse length_of_annotation = 9 remove_whitespaces = abap_true ).
        data(link_for_testing) = link.
        replace all occurrences of pcre `\s` in link_for_testing with ``.
        replace all occurrences of pcre `(@link|data:)` in link_for_testing with ``.
        split link_for_testing at '.' into table data(splitted).
        if lines( splitted ) = 3.
          decoded_abap_doc-default = link.
        elseif warning_set = abap_false.
          message w111(saff_core) with abap_doc_annotation-default component_name into message ##NEEDED.
          parser_log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
          warning_set = abap_true.
        endif.
      endif.
    endloop.

  endmethod.


  method parse_enum_values.
    if decoded_abap_doc-enumvalues_link is not initial.
      return.
    endif.
    data(string_to_parse) = abap_doc_string.
    replace all occurrences of pcre `\$values[\s]*(:[\s]*)?\{[\s]*@link` in string_to_parse with `\$values\{@link`.
    find all occurrences of pcre `\$values\{@link([^\}]+)\}` in string_to_parse results data(result_table).
    if lines( result_table ) = 0.
      message w109(saff_core) with abap_doc_annotation-values component_name into data(message) ##NEEDED.
      parser_log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
      return.
    endif.
    if lines( result_table ) > 1.
      message i107(saff_core) with abap_doc_annotation-values component_name into message ##NEEDED.
      parser_log->add_info( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
    endif.
    data(warning_written) = abap_false.
    loop at result_table assigning field-symbol(<entry>).
      data(offset_found) = <entry>-offset.
      data(length_found) = <entry>-length.
      data(link) = get_annotation_value( length = length_found - 1  offset = offset_found to_decode = string_to_parse length_of_annotation = 13 remove_whitespaces = abap_true ).
      check_next_word( offset = offset_found + length_found text_to_check = string_to_parse ).
      data(link_for_testing) = link.
      replace all occurrences of pcre `\s` in link_for_testing with ``.
      replace all occurrences of pcre `data:` in link_for_testing with ``.
      split link_for_testing at '.' into table data(splitted).
      if lines( splitted ) = 2 and decoded_abap_doc-enumvalues_link is initial.
        decoded_abap_doc-enumvalues_link = link.
      elseif lines( splitted ) <> 2 and warning_written = abap_false.
        message w111(saff_core) with abap_doc_annotation-values component_name into message ##NEEDED.
        parser_log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
        warning_written = abap_true.
      endif.
    endloop.
  endmethod.


  method parse_required.
    if decoded_abap_doc-required is not initial.
      return.
    endif.
    find all occurrences of abap_doc_annotation-required in abap_doc_string results data(result_table).
    if lines( result_table ) > 1.
      message i107(saff_core) with abap_doc_annotation-required component_name into data(message) ##NEEDED.
      parser_log->add_info( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
    endif.
    decoded_abap_doc-required = abap_true.
    loop at result_table assigning field-symbol(<entry>).
      check_next_word( offset = <entry>-offset + <entry>-length text_to_check = abap_doc_string ).
    endloop.
  endmethod.


  method parse_show_always.
    if decoded_abap_doc-showalways is not initial.
      return.
    endif.
    find all occurrences of abap_doc_annotation-show_always in abap_doc_string results data(result_table).
    if lines( result_table ) > 1.
      message i107(saff_core) with abap_doc_annotation-show_always component_name into data(message) ##NEEDED.
      parser_log->add_info( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
    endif.
    decoded_abap_doc-showalways = abap_true.
    loop at result_table assigning field-symbol(<entry>).
      check_next_word( offset = <entry>-offset + <entry>-length text_to_check = abap_doc_string ).
    endloop.
  endmethod.


  method parse_number_annotations.
    case key_word.
      when abap_doc_annotation-minimum.
        if decoded_abap_doc-minimum is initial.
          decoded_abap_doc-minimum = get_number_annotation( annotation_name = key_word ).
        endif.
      when abap_doc_annotation-maximum.
        if decoded_abap_doc-maximum is initial.
          decoded_abap_doc-maximum = get_number_annotation( annotation_name = key_word ).
        endif.
      when abap_doc_annotation-exclusive_minimum.
        if decoded_abap_doc-exclusive_minimum is initial.
          decoded_abap_doc-exclusive_minimum = get_number_annotation( annotation_name = key_word ).
        endif.
      when abap_doc_annotation-exclusive_maximum.
        if decoded_abap_doc-exclusive_maximum is initial.
          decoded_abap_doc-exclusive_maximum = get_number_annotation( annotation_name = key_word ).
        endif.
      when abap_doc_annotation-multiple_of.
        if decoded_abap_doc-multiple_of is initial.
          decoded_abap_doc-multiple_of = get_number_annotation( annotation_name = key_word ).
        endif.
      when abap_doc_annotation-min_length.
        if decoded_abap_doc-min_length is initial.
          decoded_abap_doc-min_length = get_number_annotation( annotation_name = key_word ).
        endif.
      when abap_doc_annotation-max_length.
        if decoded_abap_doc-max_length is initial.
          decoded_abap_doc-max_length = get_number_annotation( annotation_name = key_word ).
        endif.
    endcase.
  endmethod.


  method get_number_annotation.
    data(abap_doc) = abap_doc_string.
    data(dummy_annotation) = `$dummyannotation`.
    replace all occurrences of annotation_name in abap_doc with dummy_annotation.
    replace all occurrences of pcre `\$dummyannotation[\s]*(:[\s]*)?` in abap_doc with `\$dummyannotation`.
    find all occurrences of pcre `\$dummyannotation[^\s]+` in abap_doc results data(result_table).
    if lines( result_table ) = 0.
      message w109(saff_core) with abap_doc_annotation-values component_name into data(message) ##NEEDED.
      parser_log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
      return.
    endif.
    if lines( result_table ) > 1.
      message i107(saff_core) with annotation_name component_name into message ##NEEDED.
      parser_log->add_info( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
    endif.
    data(annotation_length) = strlen( dummy_annotation ).
    data(regex_of_number_expressions) = cl_abap_regex=>create_pcre( pattern     = `(\+|-)?[0-9]+(.[0-9]+)?(e(\+|-)?[0-9]+)?`
                                                                    ignore_case = abap_true ).
    data(warning_written) = abap_false.
    loop at result_table assigning field-symbol(<entry>).
      data(offset_found) = <entry>-offset.
      data(length_found) = <entry>-length.
      data(begin_of_number) = offset_found + annotation_length.
      data(length_of_number) = length_found - annotation_length.
      data(number_candidate) = abap_doc+begin_of_number(length_of_number).
      remove_leading_trailing_spaces( changing string_to_work_on = number_candidate ).
      data(matcher) = regex_of_number_expressions->create_matcher( text = number_candidate ).
      data(match) = matcher->match( ).
      check_next_word( offset = offset_found + length_found text_to_check = abap_doc ).
      if match = abap_true and number is initial.
        number = number_candidate.
      elseif match = abap_false and warning_written = abap_false.
        message w110(saff_core) with annotation_name component_name into message ##NEEDED.
        parser_log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
        warning_written = abap_true.
      endif.
    endloop.
  endmethod.


  method remove_leading_trailing_spaces.
    shift string_to_work_on right deleting trailing space.
    shift string_to_work_on left deleting leading space.
  endmethod.


  method check_next_word.
    if description_warning_is_needed = abap_true.
      return.
    endif.
    data(current_offset) = offset.
    data next_word type string.
    data next_char type c.

    while next_char = space and current_offset < strlen( text_to_check ).
      next_char = text_to_check+current_offset(1).
      current_offset += 1.
    endwhile.
    next_word = next_char.
    if current_offset >= strlen( text_to_check ).
      return.
    endif.
    data(regex_of_letter) = cl_abap_regex=>create_pcre( pattern = `[a-zA-Z]` ) ##NO_TEXT.
    do.
      next_char = text_to_check+current_offset(1).
      current_offset += 1.
      next_word = next_word && next_char.
      if regex_of_letter->create_matcher( text = next_char )->match( ) = abap_false or current_offset >= strlen( text_to_check ).
        exit.
      endif.
    enddo.
    remove_leading_trailing_spaces( changing string_to_work_on = next_word ).
    if strlen( next_word ) = 1 or next_word+0(1) <> `$`.
      description_warning_is_needed = abap_true.
    endif.
  endmethod.


  method write_description_message.
    if description_warning_is_needed = abap_true and decoded_abap_doc-description is initial.
      message w115(saff_core) with component_name into data(message) ##NEEDED.
      parser_log->add_warning( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
    elseif description_warning_is_needed = abap_true and decoded_abap_doc-description is not initial.
      message i116(saff_core) with component_name into message ##NEEDED.
      parser_log->add_info( message = cl_aff_log=>get_sy_message( ) object = value #( ) ).
    endif.
  endmethod.

endclass.

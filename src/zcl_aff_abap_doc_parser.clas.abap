"!@testing CL_AFF_ABAP_DOC_PARSER
CLASS ltcl_aff_abap_doc_parser DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA parser TYPE REF TO zcl_aff_abap_doc_parser.
    DATA log TYPE REF TO zif_aff_log.
    DATA exp_abap_doc TYPE zcl_aff_abap_doc_parser=>abap_doc.
    METHODS setup.
    METHODS title_and_description FOR TESTING RAISING cx_static_check.
    METHODS default_minimum FOR TESTING RAISING cx_static_check.
    METHODS required_max_exclmin FOR TESTING RAISING cx_static_check.
    METHODS showalways_exclmax_multipleof FOR TESTING RAISING cx_static_check.
    METHODS enum_values FOR TESTING RAISING cx_static_check.
    METHODS callback_class FOR TESTING RAISING cx_static_check.
    METHODS default_with_link FOR TESTING RAISING cx_static_check.
    METHODS too_many_titles_and_showalways FOR TESTING RAISING cx_static_check.
    METHODS too_many_number_annotations FOR TESTING RAISING cx_static_check.
    METHODS too_many_default_mixed FOR TESTING RAISING cx_static_check.
    METHODS too_many_default_link FOR TESTING RAISING cx_static_check.
    METHODS too_many_default_value FOR TESTING RAISING cx_static_check.
    METHODS too_many_value_links FOR TESTING RAISING cx_static_check.
    METHODS too_many_callbackclasses FOR TESTING RAISING cx_static_check.
    METHODS too_many_required_annotations FOR TESTING RAISING cx_static_check.
    METHODS unknown_annotation FOR TESTING RAISING cx_static_check.
    METHODS wrong_usage_callback_class FOR TESTING RAISING cx_static_check.
    METHODS wrong_usage_default FOR TESTING RAISING cx_static_check.
    METHODS wrong_usage_enum_values FOR TESTING RAISING cx_static_check.
    METHODS wrong_value_number_annotation FOR TESTING RAISING cx_static_check.
    METHODS wrong_links FOR TESTING RAISING cx_static_check.
    METHODS description_at_false_position FOR TESTING RAISING cx_static_check.
    METHODS text_between_annotations FOR TESTING RAISING cx_static_check.
    METHODS title_at_wrong_position FOR TESTING RAISING cx_static_check.
    METHODS overwriting_enum_value FOR TESTING RAISING cx_static_check.
    METHODS too_many_enum_values FOR TESTING RAISING cx_static_check.


ENDCLASS.

CLASS ltcl_aff_abap_doc_parser IMPLEMENTATION.

  METHOD setup.
    parser = NEW zcl_aff_abap_doc_parser( ).
    log = NEW zcl_aff_log( ).
  ENDMETHOD.

  METHOD title_and_description.
    DATA(abap_doc_to_parse) = `<p class="shorttext">Title  </p> This is the description.`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `This is the description.` title = `Title`).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = zif_aff_log=>c_message_type-info ).
  ENDMETHOD.


  METHOD default_minimum.
    DATA(abap_doc_to_parse) = `<p class="shorttext">Title</p> This is the description. $minimum 12 $default '20'`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `This is the description.` title = `Title` minimum = `12` default = `"20"`).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = zif_aff_log=>c_message_type-info ).
  ENDMETHOD.


  METHOD required_max_exclmin.
    DATA(abap_doc_to_parse) = `<p class="shorttext">Title</p> This is the description. $exclusiveMinimum 12 $maximum 20 $required`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `This is the description.` title = `Title` exclusive_minimum = `12` maximum = `20` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = zif_aff_log=>c_message_type-info ).
  ENDMETHOD.


  METHOD showalways_exclmax_multipleof.
    DATA(abap_doc_to_parse) = `<p class="shorttext">Title</p> This is the description. $exclusiveMaximum 12 $multipleOf 2 $showAlways `.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `This is the description.` title = `Title` exclusive_maximum = `12` multiple_of = `2` showalways = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = zif_aff_log=>c_message_type-info ).
  ENDMETHOD.


  METHOD enum_values.
    DATA(abap_doc_to_parse) = `<p class="shorttext">Title</p> This is the description. $values    {    @link    cl_aff_test_types_for_writer.data:enum_values    }`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( title = `Title` description = `This is the description.` enumvalues_link = `cl_aff_test_types_for_writer.data:enum_values` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = zif_aff_log=>c_message_type-info ).
  ENDMETHOD.


  METHOD callback_class.
    DATA(abap_doc_to_parse) = `<p class="shorttext">Title</p> This is the description. $callbackClass {     @link    cl_aff_test_types_for_writer    } `.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( title = `Title` description = `This is the description.` callback_class = `cl_aff_test_types_for_writer` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = zif_aff_log=>c_message_type-info ).
  ENDMETHOD.


  METHOD default_with_link.
    DATA(abap_doc_to_parse) = `<p class="shorttext">Title</p> This is the description. $default {@link cl_aff_test_types_for_writer.data:enum_values.classic_badi } `.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( title = `Title` description = `This is the description.` default = `@link cl_aff_test_types_for_writer.data:enum_values.classic_badi` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = zif_aff_log=>c_message_type-info ).
  ENDMETHOD.


  METHOD too_many_titles_and_showalways.
    DATA(abap_doc_to_parse) = `<p class="shorttext">Title</p> <p class="shorttext">Title2</p> This is the description $default {@link cl_aff_test_types_for_writer.data:enum_values.classic_badi }  $showAlways $minimum 2 $showAlways`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( title = `Title`  description = `This is the description` showalways = abap_true minimum = `2` default = `@link cl_aff_test_types_for_writer.data:enum_values.classic_badi` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = `There are several occurrences of annotation 'Title' . First valid is used`
                                                              exp_type           = zif_aff_log=>c_message_type-info
                                                              exp_component_name = `Component Name` ).

    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = |There are several occurrences of annotation { zcl_aff_abap_doc_parser=>abap_doc_annotation-show_always } . First valid is used|
                                                              exp_type           = zif_aff_log=>c_message_type-info
                                                              exp_component_name = `Component Name` ).
  ENDMETHOD.

  METHOD too_many_number_annotations.
    DATA(abap_doc_to_parse) = `Here are too many number annotations $minimum 4 $maximum 9 $maximum 19 $minimum 3 `.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Here are too many number annotations`  minimum = '4' maximum = '9' ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = |There are several occurrences of annotation { zcl_aff_abap_doc_parser=>abap_doc_annotation-minimum } . First valid is used|
                                                              exp_type           = zif_aff_log=>c_message_type-info
                                                              exp_component_name = `Component Name` ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = |There are several occurrences of annotation { zcl_aff_abap_doc_parser=>abap_doc_annotation-maximum } . First valid is used|
                                                              exp_type           = zif_aff_log=>c_message_type-info
                                                              exp_component_name = `Component Name` ).
  ENDMETHOD.

  METHOD too_many_default_mixed.
    DATA(abap_doc_to_parse) = `Here are too many defaults  $required $default '10' $minimum 4 $default {@link cl_aff_test_types_for_writer.data:enum_values.classic_badi } $default '11'`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Here are too many defaults`  minimum = '4' default = `"10"` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = |There are several occurrences of annotation { zcl_aff_abap_doc_parser=>abap_doc_annotation-default } . First valid is used|
                                                              exp_type           = zif_aff_log=>c_message_type-info
                                                              exp_component_name = `Component Name` ).
  ENDMETHOD.

  METHOD too_many_default_link.
    DATA(abap_doc_to_parse) = `Here are too many defaults  $required $default {@link cl_aff_test_types_for_writer.data:enum_values.classic_badi } $minimum 4 $default {@link cl_aff_test_types_for_writer.data:enum_values.classic_badi } `.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Here are too many defaults`  minimum = '4' default = `@link cl_aff_test_types_for_writer.data:enum_values.classic_badi` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = |There are several occurrences of annotation { zcl_aff_abap_doc_parser=>abap_doc_annotation-default } . First valid is used|
                                                              exp_type           = zif_aff_log=>c_message_type-info
                                                              exp_component_name = `Component Name` ).
  ENDMETHOD.

  METHOD too_many_default_value.
    DATA(abap_doc_to_parse) = `Here are too many defaults  $required $default '10' $minimum 4 $default '19' `.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Here are too many defaults`  minimum = '4' default = `"10"` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = |There are several occurrences of annotation { zcl_aff_abap_doc_parser=>abap_doc_annotation-default } . First valid is used|
                                                              exp_type           = zif_aff_log=>c_message_type-info
                                                              exp_component_name = `Component Name` ).
  ENDMETHOD.

  METHOD too_many_value_links.
    DATA(abap_doc_to_parse) = `Here are two many value links. $values { @link    cl_aff_test_types_for_writer.data:enum_values    } $values { @link    cl_aff_test_types_for_writer.data:enum_values } $required`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Here are two many value links.` enumvalues_link = `cl_aff_test_types_for_writer.data:enum_values` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = |There are several occurrences of annotation { zcl_aff_abap_doc_parser=>abap_doc_annotation-values } . First valid is used|
                                                              exp_type           = zif_aff_log=>c_message_type-info
                                                              exp_component_name = `Component Name` ).
  ENDMETHOD.

  METHOD too_many_callbackclasses.
    DATA(abap_doc_to_parse) = `Here are too many callbackclass links. $callbackClass { @link cl_aff_test_types_for_writer } $minimum 4 $callbackClass {  @link  cl_aff_test_types_for_writer  }`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Here are too many callbackclass links.` callback_class = `cl_aff_test_types_for_writer` minimum = '4').
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = |There are several occurrences of annotation { zcl_aff_abap_doc_parser=>abap_doc_annotation-callback_class } . First valid is used|
                                                              exp_type           = zif_aff_log=>c_message_type-info
                                                              exp_component_name = `Component Name` ).
  ENDMETHOD.

  METHOD too_many_required_annotations.
    DATA(abap_doc_to_parse) = `Here are too many required annotations. $required $minLength 5 $required $maxLength 10`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Here are too many required annotations.` required = abap_true min_length = '5' max_length = '10').
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = |There are several occurrences of annotation { zcl_aff_abap_doc_parser=>abap_doc_annotation-required } . First valid is used|
                                                              exp_type           = zif_aff_log=>c_message_type-info
                                                              exp_component_name = `Component Name` ).
  ENDMETHOD.

  METHOD unknown_annotation.
    DATA(abap_doc_to_parse) = `Here is a unknown annoataion. $required $unknown`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Here is a unknown annoataion.` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text(
      log                = log
      exp_text           = `Annotation $unknown is unknown`
      exp_type           = zif_aff_log=>c_message_type-warning
      exp_component_name = `Component Name` ).
  ENDMETHOD.

  METHOD wrong_usage_callback_class.
    DATA(abap_doc_to_parse) = `Wrong usage of callbackClass annotation. $callbackClass { cl_aff_test_types_for_writer } $default '4' `.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Wrong usage of callbackClass annotation.` default = '"4"').
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text(
      log                = log
      exp_text           = `Annotation $callbackClass was used incorrectly`
      exp_type           = zif_aff_log=>c_message_type-warning
      exp_component_name = `Component Name` ).
  ENDMETHOD.

  METHOD wrong_usage_default.
    DATA(abap_doc_to_parse) = `Wrong usage of default  $required $default 10 `.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name1`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Wrong usage of default` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).

    abap_doc_to_parse = `Wrong usage of default  $required $default {cl_aff_test_types_for_writer.data:enum_values.classic_badi } `.
    act_abap_doc = parser->parse(
      EXPORTING
        component_name = `Component Name2`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Wrong usage of default` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).

    zcl_aff_tools_unit_test_helper=>assert_log_contains_text(
      log                = log
      exp_text           = `Annotation $default was used incorrectly`
      exp_type           = zif_aff_log=>c_message_type-warning
      exp_component_name = `Component Name1` ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text(
      log                = log
      exp_text           = `Annotation $default was used incorrectly`
      exp_type           = zif_aff_log=>c_message_type-warning
      exp_component_name = `Component Name2` ).
  ENDMETHOD.

  METHOD wrong_usage_enum_values.
    DATA(abap_doc_to_parse) = `Wrong usage of values. $values { cl_aff_test_types_for_writer.data:enum_values} $required`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Wrong usage of values.` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text(
      log                = log
      exp_text           = `Annotation $values was used incorrectly`
      exp_type           = zif_aff_log=>c_message_type-warning
      exp_component_name = `Component Name` ).

  ENDMETHOD.

  METHOD wrong_value_number_annotation.
    DATA(abap_doc_to_parse) = `Wrong usage of minimum and maximum. $minimum '2' $maximum basjkasjdsa $default '3' `.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Wrong usage of minimum and maximum.` default = '"3"' ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = |No number was provided for annotation { zcl_aff_abap_doc_parser=>abap_doc_annotation-minimum }|
                                                              exp_type           = zif_aff_log=>c_message_type-warning
                                                              exp_component_name = `Component Name` ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = |No number was provided for annotation { zcl_aff_abap_doc_parser=>abap_doc_annotation-maximum }|
                                                              exp_type           = zif_aff_log=>c_message_type-warning
                                                              exp_component_name = `Component Name` ).
  ENDMETHOD.

  METHOD wrong_links.
    DATA(abap_doc_to_parse) = `Wrong links for default and values. $values{@link cl_aff_test_types_for_writer.data:enum_values.component} $default{@link cl_aff_test_types_for_writer.data:enum_values}`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Wrong links for default and values.` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = |Link in annotation { zcl_aff_abap_doc_parser=>abap_doc_annotation-values } is incorrect|
                                                              exp_type           = zif_aff_log=>c_message_type-warning
                                                              exp_component_name = `Component Name` ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = |Link in annotation { zcl_aff_abap_doc_parser=>abap_doc_annotation-default } is incorrect|
                                                              exp_type           = zif_aff_log=>c_message_type-warning
                                                              exp_component_name = `Component Name` ).
  ENDMETHOD.

  METHOD description_at_false_position.
    DATA(abap_doc_to_parse) = `<p class="shorttext">Title</p> $minimum 12 This is the description at wrong position $default '20'`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( title = `Title` minimum = `12` default = `"20"`).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = zif_aff_log=>co_msg115
                                                              exp_component_name = `Component Name`
                                                              exp_type           = zif_aff_log=>c_message_type-warning ).
  ENDMETHOD.

  METHOD text_between_annotations.
    DATA(abap_doc_to_parse) = `<p class="shorttext">Title</p> Here is text between annotation $default {@link cl_aff_test_types_for_writer.data:enum_values.classic_badi} Some unused text $required`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Here is text between annotation` title = `Title` required = abap_true default = `@link cl_aff_test_types_for_writer.data:enum_values.classic_badi`).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = zif_aff_log=>co_msg116
                                                              exp_component_name = `Component Name`
                                                              exp_type           = zif_aff_log=>c_message_type-info ).
  ENDMETHOD.

  METHOD title_at_wrong_position.
    DATA(abap_doc_to_parse) = `Description first <p class="shorttext">This is the title at wrong position</p> $values{@link cl_aff_test_types_for_writer.data:enum_values} Unused Text`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Description first` title = `This is the title at wrong position` enumvalues_link = `cl_aff_test_types_for_writer.data:enum_values` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = zif_aff_log=>co_msg116
                                                              exp_component_name = `Component Name`
                                                              exp_type           = zif_aff_log=>c_message_type-info ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = zif_aff_log=>co_msg113
                                                              exp_component_name = `Component Name`
                                                              exp_type           = zif_aff_log=>c_message_type-info ).
  ENDMETHOD.

  METHOD overwriting_enum_value.
    DATA(abap_doc_to_parse) = `<p class="shorttext">Field With Overwritten Enum Value</p> Field with overwritten enum value $enumValue 'ownValue'`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Field with overwritten enum value` title = `Field With Overwritten Enum Value` enum_value = `ownValue` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = zif_aff_log=>c_message_type-info ).
  ENDMETHOD.

  METHOD too_many_enum_values.
    DATA(abap_doc_to_parse) = `<p class="shorttext">Field With Overwritten Enum Value</p> Field with overwritten enum value $enumValue 'ownValue' blablabl $enumValue 'ownValue2'`.
    DATA(act_abap_doc) = parser->parse(
      EXPORTING
        component_name = `Component Name`
        to_parse       = abap_doc_to_parse
      CHANGING
        log            = log ).
    exp_abap_doc = VALUE #( description = `Field with overwritten enum value` title = `Field With Overwritten Enum Value` enum_value = `ownValue` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = |There are several occurrences of annotation { zcl_aff_abap_doc_parser=>abap_doc_annotation-enum_value } . First valid is used|
                                                              exp_type           = zif_aff_log=>c_message_type-info
                                                              exp_component_name = `Component Name` ).
  ENDMETHOD.

ENDCLASS.

"!@testing CL_AFF_ABAP_DOC_PARSER
class ltcl_aff_abap_doc_parser definition final for testing
  duration short
  risk level harmless.
  private section.
    data parser type ref to zcl_aff_abap_doc_parser.
    data log type ref to if_aff_log.
    data exp_abap_doc type zcl_aff_abap_doc_parser=>abap_doc.
    methods setup.
    methods title_and_description for testing raising cx_static_check.
    methods default_minimum for testing raising cx_static_check.
    methods required_max_exclmin for testing raising cx_static_check.
    methods showalways_exclmax_multipleof for testing raising cx_static_check.
    methods enum_values for testing raising cx_static_check.
    methods callback_class for testing raising cx_static_check.
    methods default_with_link for testing raising cx_static_check.
    methods too_many_titles_and_showalways for testing raising cx_static_check.
    methods too_many_number_annotations for testing raising cx_static_check.
    methods too_many_default_mixed for testing raising cx_static_check.
    methods too_many_default_link for testing raising cx_static_check.
    methods too_many_default_value for testing raising cx_static_check.
    methods too_many_value_links for testing raising cx_static_check.
    methods too_many_callbackclasses for testing raising cx_static_check.
    methods too_many_required_annotations for testing raising cx_static_check.
    methods unknown_annotation for testing raising cx_static_check.
    methods wrong_usage_callback_class for testing raising cx_static_check.
    methods wrong_usage_default for testing raising cx_static_check.
    methods wrong_usage_enum_values for testing raising cx_static_check.
    methods wrong_value_number_annotation for testing raising cx_static_check.
    methods wrong_links for testing raising cx_static_check.
    methods description_at_false_position for testing raising cx_static_check.
    methods text_between_annotations for testing raising cx_static_check.
    methods title_at_wrong_position for testing raising cx_static_check.


endclass.

class ltcl_aff_abap_doc_parser implementation.

  method setup.
    parser = new zcl_aff_abap_doc_parser( ).
    log = new cl_aff_log( ).
  endmethod.

  method title_and_description.
    data(abap_doc_to_parse) = `<p class="shorttext">Title  </p> This is the description.`.
    data(act_abap_doc) = parser->parse(
                           exporting
                             component_name = `Component Name`
                             to_parse       = abap_doc_to_parse
                           changing
                             log            = log
                         ).
    exp_abap_doc = value #( description = `This is the description.` title = `Title`).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = if_aff_log=>c_message_type-info ).
  endmethod.


  method default_minimum.
    data(abap_doc_to_parse) = `<p class="shorttext">Title</p> This is the description. $minimum 12 $default '20'`.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `This is the description.` title = `Title` minimum = `12` default = `"20"`).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = if_aff_log=>c_message_type-info ).
  endmethod.


  method required_max_exclmin.
    data(abap_doc_to_parse) = `<p class="shorttext">Title</p> This is the description. $exclusiveMinimum 12 $maximum 20 $required`.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `This is the description.` title = `Title` exclusive_minimum = `12` maximum = `20` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = if_aff_log=>c_message_type-info ).
  endmethod.


  method showalways_exclmax_multipleOf.
    data(abap_doc_to_parse) = `<p class="shorttext">Title</p> This is the description. $exclusiveMaximum 12 $multipleOf 2 $showAlways `.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `This is the description.` title = `Title` exclusive_maximum = `12` multiple_of = `2` showalways = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = if_aff_log=>c_message_type-info ).
  endmethod.


  method enum_values.
    data(abap_doc_to_parse) = `<p class="shorttext">Title</p> This is the description. $values    {    @link    cl_aff_test_types_for_writer.data:enum_values    }`.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( title = `Title` description = `This is the description.` enumvalues_link = `cl_aff_test_types_for_writer.data:enum_values` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = if_aff_log=>c_message_type-info ).
  endmethod.


  method callback_class.
    data(abap_doc_to_parse) = `<p class="shorttext">Title</p> This is the description. $callbackClass {     @link    cl_aff_test_types_for_writer    } `.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( title = `Title` description = `This is the description.` callback_class = `cl_aff_test_types_for_writer` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = if_aff_log=>c_message_type-info ).
  endmethod.


  method default_with_link.
    data(abap_doc_to_parse) = `<p class="shorttext">Title</p> This is the description. $default {@link cl_aff_test_types_for_writer.data:enum_values.classic_badi } `.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( title = `Title` description = `This is the description.` default = `@link cl_aff_test_types_for_writer.data:enum_values.classic_badi` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = if_aff_log=>c_message_type-info ).
  endmethod.


  method too_many_titles_and_showalways.
    data(abap_doc_to_parse) = `<p class="shorttext">Title</p> <p class="shorttext">Title2</p> This is the description $default {@link cl_aff_test_types_for_writer.data:enum_values.classic_badi }  $showAlways $minimum 2 $showAlways`.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( title = `Title`  description = `This is the description` showalways = abap_true minimum = `2` default = `@link cl_aff_test_types_for_writer.data:enum_values.classic_badi` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 107
                                                                             attr1 = `'Title'`
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-info ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 107
                                                                             attr1 = zcl_aff_abap_doc_parser=>abap_doc_annotation-show_always
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-info ).
  endmethod.

  method too_many_number_annotations.
    data(abap_doc_to_parse) = `Here are too many number annotations $minimum 4 $maximum 9 $maximum 19 $minimum 3 `.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `Here are too many number annotations`  minimum = '4' maximum = '9' ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 107
                                                                             attr1 = cl_aff_abap_doc_parser=>abap_doc_annotation-minimum
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-info ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 107
                                                                             attr1 = cl_aff_abap_doc_parser=>abap_doc_annotation-maximum
                                                                             attr2 = `Component Name` )
                                                       exp_type   =  if_aff_log=>c_message_type-info ).
  endmethod.

  method too_many_default_mixed.
    data(abap_doc_to_parse) = `Here are too many defaults  $required $default '10' $minimum 4 $default {@link cl_aff_test_types_for_writer.data:enum_values.classic_badi } $default '11'`.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `Here are too many defaults`  minimum = '4' default = `"10"` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 107
                                                                             attr1 = zcl_aff_abap_doc_parser=>abap_doc_annotation-default
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-info ).
  endmethod.

  method too_many_default_link.
    data(abap_doc_to_parse) = `Here are too many defaults  $required $default {@link cl_aff_test_types_for_writer.data:enum_values.classic_badi } $minimum 4 $default {@link cl_aff_test_types_for_writer.data:enum_values.classic_badi } `.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `Here are too many defaults`  minimum = '4' default = `@link cl_aff_test_types_for_writer.data:enum_values.classic_badi` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 107
                                                                             attr1 = zcl_aff_abap_doc_parser=>abap_doc_annotation-default
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-info ).
  endmethod.

  method too_many_default_value.
    data(abap_doc_to_parse) = `Here are too many defaults  $required $default '10' $minimum 4 $default '19' `.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `Here are too many defaults`  minimum = '4' default = `"10"` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 107
                                                                             attr1 = zcl_aff_abap_doc_parser=>abap_doc_annotation-default
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-info ).
  endmethod.

  method too_many_value_links.
    data(abap_doc_to_parse) = `Here are two many value links. $values { @link    cl_aff_test_types_for_writer.data:enum_values    } $values { @link    cl_aff_test_types_for_writer.data:enum_values } $required`.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `Here are two many value links.` enumvalues_link = `cl_aff_test_types_for_writer.data:enum_values` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 107
                                                                             attr1 = zcl_aff_abap_doc_parser=>abap_doc_annotation-values
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-info ).
  endmethod.

  method too_many_callbackclasses.
    data(abap_doc_to_parse) = `Here are too many callbackclass links. $callbackClass { @link cl_aff_test_types_for_writer } $minimum 4 $callbackClass {  @link  cl_aff_test_types_for_writer  }`.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `Here are too many callbackclass links.` callback_class = `cl_aff_test_types_for_writer` minimum = '4').
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 107
                                                                             attr1 = zcl_aff_abap_doc_parser=>abap_doc_annotation-callback_class
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-info ).
  endmethod.

  method too_many_required_annotations.
    data(abap_doc_to_parse) = `Here are too many required annotations. $required $minLength 5 $required $maxLength 10`.
    data(act_abap_doc) = parser->parse(
                                 exporting
                                   component_name = `Component Name`
                                   to_parse       = abap_doc_to_parse
                                 changing
                                   log            = log
                               ).
    exp_abap_doc = value #( description = `Here are too many required annotations.` required = abap_true min_length = '5' max_length = '10').
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 107
                                                                             attr1 = zcl_aff_abap_doc_parser=>abap_doc_annotation-required
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-info ).
  endmethod.

  method unknown_annotation.
    data(abap_doc_to_parse) = `Here is a unknown annoataion. $required $unknown`.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `Here is a unknown annoataion.` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 108
                                                                             attr1 = `$unknown`
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-warning ).
  endmethod.

  method wrong_usage_callback_class.
    data(abap_doc_to_parse) = `Wrong usage of callbackClass annotation. $callbackClass { cl_aff_test_types_for_writer } $default '4' `.
    data(act_abap_doc) = parser->parse(
                              exporting
                                component_name = `Component Name`
                                to_parse       = abap_doc_to_parse
                              changing
                                log            = log
                            ).
    exp_abap_doc = value #( description = `Wrong usage of callbackClass annotation.` default = '"4"').
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 109
                                                                             attr1 = zcl_aff_abap_doc_parser=>abap_doc_annotation-callback_class
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-warning ).
  endmethod.

  method wrong_usage_default.
    data(abap_doc_to_parse) = `Wrong usage of default  $required $default 10 `.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name1`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `Wrong usage of default` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).

    abap_doc_to_parse = `Wrong usage of default  $required $default {cl_aff_test_types_for_writer.data:enum_values.classic_badi } `.
    act_abap_doc = parser->parse(
                               exporting
                                 component_name = `Component Name2`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `Wrong usage of default` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).

    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 109
                                                                             attr1 = zcl_aff_abap_doc_parser=>abap_doc_annotation-default
                                                                             attr2 = `Component Name1` )
                                                      exp_type    =  if_aff_log=>c_message_type-warning ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 109
                                                                             attr1 = zcl_aff_abap_doc_parser=>abap_doc_annotation-default
                                                                             attr2 = `Component Name2` )
                                                      exp_type    =  if_aff_log=>c_message_type-warning ).
  endmethod.

  method wrong_usage_enum_values.
    data(abap_doc_to_parse) = `Wrong usage of values. $values { cl_aff_test_types_for_writer.data:enum_values} $required`.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `Wrong usage of values.` required = abap_true ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 109
                                                                             attr1 = zcl_aff_abap_doc_parser=>abap_doc_annotation-values
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-warning ).
  endmethod.

  method wrong_value_number_annotation.
    data(abap_doc_to_parse) = `Wrong usage of minimum and maximum. $minimum '2' $maximum basjkasjdsa $default '3' `.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `Wrong usage of minimum and maximum.` default = '"3"' ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 110
                                                                             attr1 = zcl_aff_abap_doc_parser=>abap_doc_annotation-minimum
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-warning ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 110
                                                                             attr1 = zcl_aff_abap_doc_parser=>abap_doc_annotation-maximum
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-warning ).
  endmethod.

  method wrong_links.
    data(abap_doc_to_parse) = `Wrong links for default and values. $values{@link cl_aff_test_types_for_writer.data:enum_values.component} $default{@link cl_aff_test_types_for_writer.data:enum_values}`.
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `Wrong links for default and values.` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 111
                                                                             attr1 = zcl_aff_abap_doc_parser=>abap_doc_annotation-values
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-warning ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 111
                                                                             attr1 = zcl_aff_abap_doc_parser=>abap_doc_annotation-default
                                                                             attr2 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-warning ).
  endmethod.

  method description_at_false_position.
    data(abap_doc_to_parse) = `<p class="shorttext">Title</p> $minimum 12 This is the description at wrong position $default '20'`  .
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( title = `Title` minimum = `12` default = `"20"`).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 115
                                                                             attr1 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-warning ).
  endmethod.

  method text_between_annotations.
    data(abap_doc_to_parse) = `<p class="shorttext">Title</p> Here is text between annotation $default {@link cl_aff_test_types_for_writer.data:enum_values.classic_badi} Some unused text $required`  .
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `Here is text between annotation` title = `Title` required = abap_true default = `@link cl_aff_test_types_for_writer.data:enum_values.classic_badi`).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 116
                                                                             attr1 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-info ).
  endmethod.

  method title_at_wrong_position.
    data(abap_doc_to_parse) = `Description first <p class="shorttext">This is the title at wrong position</p> $values{@link cl_aff_test_types_for_writer.data:enum_values} Unused Text`  .
    data(act_abap_doc) = parser->parse(
                               exporting
                                 component_name = `Component Name`
                                 to_parse       = abap_doc_to_parse
                               changing
                                 log            = log
                             ).
    exp_abap_doc = value #( description = `Description first` title = `This is the title at wrong position` enumvalues_link = `cl_aff_test_types_for_writer.data:enum_values` ).
    cl_abap_unit_assert=>assert_equals( exp = exp_abap_doc act = act_abap_doc ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 116
                                                                             attr1 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-info ).
    cl_aff_unit_test_helper=>assert_log_contains_msg( log         = log
                                                      exp_message = value #( msgid = 'SAFF_CORE'
                                                                             msgno = 113
                                                                             attr1 = `Component Name` )
                                                      exp_type    =  if_aff_log=>c_message_type-info ).
  endmethod.

endclass.

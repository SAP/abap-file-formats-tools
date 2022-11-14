INTERFACE lif_test_types.
  TYPES:
    element      TYPE string.

  TYPES:
    BEGIN OF structure,
      element_1 TYPE i,
      element_2 TYPE element,
    END OF structure.

  TYPES:
    BEGIN OF include,
      include_element_1 TYPE string,
      include_element_2 TYPE i,
    END OF include.

  TYPES:
    BEGIN OF structure_with_include.
      INCLUDE TYPE include.
  TYPES element_1 TYPE i.
  TYPES element_2 TYPE element.
  TYPES END OF structure_with_include.

  TYPES:
    BEGIN OF include_in_include.
      INCLUDE TYPE include.
  TYPES END OF include_in_include.

  TYPES:
    BEGIN OF structure_include_in_include.
      INCLUDE TYPE include_in_include.
  TYPES element TYPE string.
  TYPES END OF structure_include_in_include.

  TYPES:
    table_structure TYPE STANDARD TABLE OF structure WITH DEFAULT KEY.

  TYPES:
    table_build_in_type TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

  TYPES:
    BEGIN OF structure_with_table,
      table TYPE table_structure,
    END OF structure_with_table.

  TYPES:
    BEGIN OF include_table.
      INCLUDE TYPE structure_with_table.
  TYPES include_element_1 TYPE i.
  TYPES END OF include_table.

  TYPES:
    table_in_table TYPE STANDARD TABLE OF table_build_in_type WITH DEFAULT KEY.

  TYPES:
    BEGIN OF nested_table,
      second_table TYPE table_build_in_type,
    END OF nested_table,
    first_table_type TYPE STANDARD TABLE OF nested_table WITH DEFAULT KEY,
    BEGIN OF struc_tab_struc_tab,
      first_table TYPE first_table_type,
    END OF struc_tab_struc_tab.

  TYPES: langu TYPE sy-langu.
  TYPES:
    BEGIN OF structure_with_language,
      language  TYPE sy-langu,
      language2 TYPE langu,
    END OF structure_with_language.

ENDINTERFACE.

"!@testing ZCL_AFF_GENERATOR
CLASS ltcl_type_writer_xslt DEFINITION FINAL FOR TESTING
  DURATION MEDIUM
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      exp_transformation TYPE string_table,
      cut                TYPE REF TO zcl_aff_writer_xslt,
      test_generator     TYPE REF TO zcl_aff_generator,
      st_root_name       TYPE string VALUE 'root' ##NO_TEXT.

    METHODS: structure_with_incl FOR TESTING RAISING cx_static_check,
      structure_include_in_include FOR TESTING RAISING cx_static_check,
      table_structure FOR TESTING RAISING cx_static_check,
      include_table FOR TESTING RAISING cx_static_check,
      table_in_table FOR TESTING RAISING cx_static_check,
      struc_tab_struc_tab FOR TESTING RAISING cx_static_check,
      type_timestamp FOR TESTING RAISING cx_static_check,
      type_boolean FOR TESTING RAISING cx_static_check,
      type_numeric FOR TESTING RAISING cx_static_check,
      type_string FOR TESTING RAISING cx_static_check,
      open_unsupported_node FOR TESTING RAISING cx_static_check,
      close_unsupported_node FOR TESTING RAISING cx_static_check,
      date_time_element FOR TESTING RAISING cx_static_check,
      structure_with_language FOR TESTING RAISING cx_static_check,
      validate_valid_xslt FOR TESTING RAISING cx_static_check,
      validate_invalid_xslt FOR TESTING RAISING cx_static_check,

      setup,
      validate_output
        IMPORTING
          act TYPE string_table.

ENDCLASS.

CLASS zcl_aff_writer_xslt DEFINITION LOCAL FRIENDS ltcl_type_writer_xslt.

CLASS ltcl_type_writer_xslt IMPLEMENTATION.

  METHOD setup.
    cut = NEW zcl_aff_writer_xslt( me->st_root_name ).
    test_generator = NEW zcl_aff_generator( cut ).
  ENDMETHOD.


  METHOD date_time_element.
    DATA(act_output) = test_generator->generate_type( VALUE d( ) ).
    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `    <str>` )
        ( `        <tt:value option="format(dateTimeOffset)"/>` )
        ( `    </str >` )
        ( `</tt:cond>` ) ).
    validate_output( act_output ).
  ENDMETHOD.

  METHOD structure_with_incl.
    DATA test_type TYPE lif_test_types=>structure_with_include.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="not-initial(INCLUDE_ELEMENT_1)" frq="?">` )
        ( `        <str name="includeElement1">` )
        ( `          <tt:value ref="INCLUDE_ELEMENT_1"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="not-initial(INCLUDE_ELEMENT_2)" frq="?">` )
        ( `        <num name="includeElement2">` )
        ( `          <tt:value ref="INCLUDE_ELEMENT_2" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="not-initial(ELEMENT_1)" frq="?">` )
        ( `        <num name="element1">` )
        ( `          <tt:value ref="ELEMENT_1" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="not-initial(ELEMENT_2)" frq="?">` )
        ( `        <str name="element2">` )
        ( `          <tt:value ref="ELEMENT_2"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'element1;element2;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).

    validate_output( act_output ).
  ENDMETHOD.

  METHOD structure_include_in_include.
    DATA test_type TYPE lif_test_types=>structure_include_in_include.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="not-initial(INCLUDE_ELEMENT_1)" frq="?">` )
        ( `        <str name="includeElement1">` )
        ( `          <tt:value ref="INCLUDE_ELEMENT_1"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="not-initial(INCLUDE_ELEMENT_2)" frq="?">` )
        ( `        <num name="includeElement2">` )
        ( `          <tt:value ref="INCLUDE_ELEMENT_2" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="not-initial(ELEMENT)" frq="?">` )
        ( `        <str name="element">` )
        ( `          <tt:value ref="ELEMENT"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'element;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).

    validate_output( act_output ).
  ENDMETHOD.

  METHOD include_table.
    DATA test_type TYPE lif_test_types=>include_table.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="not-initial(TABLE)" frq="?">` )
        ( `        <array name="table">` )
        ( `          <tt:loop ref="TABLE">` )
        ( `            <tt:group>` )
        ( `              <tt:cond>` )
        ( `                <object>` )
        ( `                  <tt:group>` )
        ( `                    <tt:cond s-check="not-initial(ELEMENT_1)" frq="?">` )
        ( `                      <num name="element1">` )
        ( `                        <tt:value ref="ELEMENT_1" option="format(alpha)"/>` )
        ( `                      </num>` )
        ( `                    </tt:cond>` )
        ( `                    <tt:cond s-check="not-initial(ELEMENT_2)" frq="?">` )
        ( `                      <str name="element2">` )
        ( `                        <tt:value ref="ELEMENT_2"/>` )
        ( `                      </str>` )
        ( `                    </tt:cond>` )
        ( `                    <tt:d-cond frq="*">` )
        ( `                       <_ tt:lax="on">` )
        ( `                        <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `                          <tt:with-parameter name="MEMBERS" val="'element1;element2;'"/>` )
        ( `                        </tt:call-method>` )
        ( `                        <tt:skip/>` )
        ( `                      </_>` )
        ( `                    </tt:d-cond>` )
        ( `                    <tt:d-cond frq="?">` )
        ( `                      <__/>` )
        ( `                    </tt:d-cond>` )
        ( `                  </tt:group>` )
        ( `                </object>` )
        ( `              </tt:cond>` )
        ( `            </tt:group>` )
        ( `          </tt:loop>` )
        ( `        </array>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="not-initial(INCLUDE_ELEMENT_1)" frq="?">` )
        ( `        <num name="includeElement1">` )
        ( `          <tt:value ref="INCLUDE_ELEMENT_1" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'includeElement1;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).

    validate_output( act_output ).
  ENDMETHOD.

  METHOD struc_tab_struc_tab.
    DATA test_type TYPE lif_test_types=>struc_tab_struc_tab.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="not-initial(FIRST_TABLE)" frq="?">` )
        ( `        <array name="firstTable">` )
        ( `          <tt:loop ref="FIRST_TABLE">` )
        ( `            <tt:group>` )
        ( `              <tt:cond>` )
        ( `                <object>` )
        ( `                  <tt:group>` )
        ( `                    <tt:cond s-check="not-initial(SECOND_TABLE)" frq="?">` )
        ( `                      <array name="secondTable">` )
        ( `                        <tt:loop ref="SECOND_TABLE"> ` )
        ( `                          <tt:group>` )
        ( `                            <tt:cond>` )
        ( `                              <str>` )
        ( `                                <tt:value/>` )
        ( `                              </str>` )
        ( `                            </tt:cond>` )
        ( `                          </tt:group>` )
        ( `                        </tt:loop>` )
        ( `                      </array>` )
        ( `                    </tt:cond>` )
        ( `                    <tt:d-cond frq="*">` )
        ( `                       <_ tt:lax="on">` )
        ( `                        <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `                          <tt:with-parameter name="MEMBERS" val="'secondTable;'"/>` )
        ( `                        </tt:call-method>` )
        ( `                        <tt:skip/>` )
        ( `                      </_>` )
        ( `                    </tt:d-cond>` )
        ( `                    <tt:d-cond frq="?">` )
        ( `                      <__/>` )
        ( `                    </tt:d-cond>` )
        ( `                  </tt:group>` )
        ( `                </object>` )
        ( `              </tt:cond>` )
        ( `            </tt:group>` )
        ( `          </tt:loop>` )
        ( `        </array>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'firstTable;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).

    validate_output( act_output ).
  ENDMETHOD.

  METHOD table_in_table.
    DATA test_type TYPE lif_test_types=>table_in_table.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `    <array>` )
        ( `        <tt:loop>` )
        ( `            <tt:group>` )
        ( `                <tt:cond>` )
        ( `                    <array>` )
        ( `                        <tt:loop>` )
        ( `                            <tt:group>` )
        ( `                                <tt:cond>` )
        ( `                                    <str>` )
        ( `                                        <tt:value/>` )
        ( `                                    </str>` )
        ( `                                </tt:cond>` )
        ( `                            </tt:group>` )
        ( `                        </tt:loop>` )
        ( `                    </array>` )
        ( `                </tt:cond>` )
        ( `            </tt:group>` )
        ( `        </tt:loop>` )
        ( `    </array>` )
        ( `</tt:cond>` ) ).

    validate_output( act_output ).
  ENDMETHOD.

  METHOD table_structure.
    DATA test_type TYPE lif_test_types=>table_structure.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `    <array>` )
        ( `        <tt:loop>` )
        ( `            <tt:group>` )
        ( `                <tt:cond>` )
        ( `                    <object>` )
        ( `                        <tt:group>` )
        ( `                            <tt:cond s-check="not-initial(ELEMENT_1)" frq="?">` )
        ( `                                <num name="element1">` )
        ( `                                    <tt:value ref="ELEMENT_1" option="format(alpha)"/>` )
        ( `                                </num>` )
        ( `                            </tt:cond>` )
        ( `                            <tt:cond s-check="not-initial(ELEMENT_2)" frq="?">` )
        ( `                                <str name="element2">` )
        ( `                                    <tt:value ref="ELEMENT_2"/>` )
        ( `                                </str>` )
        ( `                            </tt:cond>` )
        ( `              <tt:d-cond frq="*">` )
        ( `                 <_ tt:lax="on">` )
        ( `                  <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `                    <tt:with-parameter name="MEMBERS" val="'element1;element2;'"/>` )
        ( `                  </tt:call-method>` )
        ( `                  <tt:skip/>` )
        ( `                </_>` )
        ( `              </tt:d-cond>` )
        ( `              <tt:d-cond frq="?">` )
        ( `                <__/>` )
        ( `              </tt:d-cond>` )
        ( `            </tt:group>` )
        ( `          </object>` )
        ( `        </tt:cond>` )
        ( `            </tt:group>` )
        ( `        </tt:loop>` )
        ( `    </array>` )
        ( `</tt:cond>` ) ).

    validate_output( act_output ).
  ENDMETHOD.

  METHOD type_timestamp.
    DATA(act_output) = test_generator->generate_type( VALUE timestamp( ) ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `    <str>` )
        ( `        <tt:value option="format(dateTimeOffset)"/>` )
        ( `    </str>` )
        ( `</tt:cond>` ) ).

    validate_output( act_output ).
  ENDMETHOD.

  METHOD type_boolean.
    DATA(act_output) = test_generator->generate_type( VALUE abap_bool( ) ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `    <bool>` )
        ( `        <tt:value option="format(boolean)"/>` )
        ( `    </bool>` )
        ( `</tt:cond>` ) ).

    validate_output( act_output ).
  ENDMETHOD.

  METHOD type_numeric.
    DATA(act_output) = test_generator->generate_type( VALUE f( ) ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `    <num>` )
        ( `        <tt:value option="format(alpha)"/>` )
        ( `    </num>` )
        ( `</tt:cond>` ) ).

    validate_output( act_output ).
  ENDMETHOD.

  METHOD type_string.
    DATA char_10 TYPE c LENGTH 10.
    DATA(act_output) = test_generator->generate_type( char_10 ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `    <str>` )
        ( `        <tt:value/>` )
        ( `    </str>` )
        ( `</tt:cond>` ) ).

    validate_output( act_output ).
  ENDMETHOD.


  METHOD structure_with_language.
    DATA test_type TYPE lif_test_types=>structure_with_language.
    DATA(act_output) = test_generator->generate_type( test_type ).
    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="not-initial(LANGUAGE)" frq="?">` )
        ( `        <str name="language">` )
        ( `          <tt:call-method class="cl_aff_xslt_callback_language" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` )
        ( `            <tt:with-parameter name="language" ref="LANGUAGE"/>` )
        ( `          </tt:call-method>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="not-initial(LANGUAGE2)" frq="?">` )
        ( `        <str name="language2">` )
        ( `          <tt:call-method class="cl_aff_xslt_callback_language" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` )
        ( `            <tt:with-parameter name="language" ref="LANGUAGE2"/>` )
        ( `          </tt:call-method>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'language;language2;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).

    validate_output( act_output ).
  ENDMETHOD.

  METHOD validate_valid_xslt.
    DATA transformation TYPE string_table.
    DATA(log) = NEW zcl_aff_log( ).
    transformation = VALUE #(
              ( `<?sap.transform simple?>` )
              ( `<tt:transform xmlns:tt="http://www.sap.com/transformation-templates">` )
              ( `<tt:root name="root"/>` )
              ( `<tt:template>` )
              ( `<tt:ref name="root">` )
              ( `<tt:cond>` )
              ( `  <object>` )
              ( `    <tt:group>` )
              ( `      <tt:cond s-check="not-initial(LANGUAGE)">` )
              ( `        <str name="language">` )
              ( `          <tt:value ref="LANGUAGE" option="format(language)"/>` )
              ( `        </str>` )
              ( `      </tt:cond>` )
              ( `      <tt:cond s-check="not-initial(LANGUAGE2)">` )
              ( `        <str name="language2">` )
              ( `          <tt:value ref="LANGUAGE2" option="format(language)"/>` )
              ( `        </str>` )
              ( `      </tt:cond>` )
              ( `    </tt:group>` )
              ( `  </object>` )
              ( `</tt:cond>` )
              ( `</tt:ref>` )
              ( `</tt:template>` )
              ( `</tt:transform>` ) ).

    DATA(is_valid) = cut->zif_aff_writer~validate( source = transformation log = log ).

    cl_abap_unit_assert=>assert_true( is_valid ).
    cl_abap_unit_assert=>assert_false( log->zif_aff_log~has_messages( ) ).
  ENDMETHOD.

  METHOD validate_invalid_xslt.
    DATA transformation TYPE string_table.
    DATA(log) = NEW zcl_aff_log( ).
    transformation = VALUE #(
              ( `<?sap.transform simple?>` )
              ( `<tt:transform xmlns:tt="http://www.sap.com/transformation-templates">` )
              ( `<tt:root name="root"/>` )
              ( `<tt:template>` )
              ( `<tt:ref name="root">` )
              ( `<tt:cond>` )
              ( `  <object>` )
              ( `    <tt:group>` )
              ( `      <tt:cond s-check="not-initial(LANGUAGE)">` )
              ( `        <str name="LANGUAGE">` )
              ( `          <tt:value ref="LANGUAGE" option="format(language)"` ) "<-  missing closing tag />
              ( `        </str>` )
              ( `      </tt:cond>` )
              ( `      <tt:cond s-check="not-initial(LANGUAGE2)">` )
              ( `        <str name="LANGUAGE2">` )
              ( `          <tt:value ref="LANGUAGE2" option="format(language)"/>` )
              ( `        </str>` )
              ( `      </tt:cond>` )
              ( `    </tt:group>` )
              ( `  </object>` )
              ( `</tt:cond>` )
              ( `</tt:ref>` )
              ( `</tt:template>` )
              ( `</tt:transform>` ) ).

    DATA(is_valid) = cut->zif_aff_writer~validate( source = transformation log = log ).

    cl_abap_unit_assert=>assert_false( is_valid ).
    cl_abap_unit_assert=>assert_true( log->zif_aff_log~has_messages( ) ).
  ENDMETHOD.

  METHOD open_unsupported_node.
    TRY.
        cut->zif_aff_writer~open_node(
          node_description = cl_abap_typedescr=>describe_by_data( VALUE i( ) )
          node_name        = 'Unssuprted Type' ).
        cl_abap_unit_assert=>fail( msg = 'Exception expected' ).
      CATCH zcx_aff_tools ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD close_unsupported_node.
    TRY.
        cut->zif_aff_writer~close_node(
          node_description = cl_abap_typedescr=>describe_by_data( VALUE i( ) )
          node_name        = 'Unssuprted Type' ).
        cl_abap_unit_assert=>fail( msg = 'Exception expected' ).
      CATCH zcx_aff_tools ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD validate_output.
    DATA exp TYPE string_table.

    APPEND `<?sap.transform simple?>` TO exp.
    APPEND `<tt:transform xmlns:tt="http://www.sap.com/transformation-templates">` TO exp.
    APPEND |<tt:root name="{ st_root_name }"/>| TO exp.
    APPEND `<tt:template>` TO exp.
    APPEND |<tt:ref name="{ st_root_name }">| TO exp.

    APPEND LINES OF me->exp_transformation TO exp.

    APPEND `</tt:ref>` TO exp.
    APPEND `</tt:template>` TO exp.
    APPEND `</tt:transform>` TO exp.
    zcl_aff_tools_unit_test_helper=>assert_equals_ignore_spaces( exp_data = exp act_data = act ).
  ENDMETHOD.

ENDCLASS.

"!@testing ZCL_AFF_GENERATOR
CLASS ltcl_integration_test DEFINITION FINAL FOR TESTING
  DURATION MEDIUM
  RISK LEVEL DANGEROUS.

  PRIVATE SECTION.
    CONSTANTS:
      c_xslt_prefix     TYPE string VALUE `ZAFF_TRANSFORMATION_GEN_` ##NO_TEXT,
      c_ext_xslt_source TYPE c LENGTH 2 VALUE 'XT'.

    CLASS-DATA st_execution_counter TYPE i.

    DATA exp_json TYPE string_table.

    METHODS: structure_with_incl FOR TESTING RAISING cx_static_check,
      structure_include_in_include FOR TESTING RAISING cx_static_check,
      table_structure FOR TESTING RAISING cx_static_check,
      include_table FOR TESTING RAISING cx_static_check,
      table_in_table FOR TESTING RAISING cx_static_check,
      struc_tab_struc_tab FOR TESTING RAISING cx_static_check,
      type_timestamp FOR TESTING RAISING cx_static_check,
      type_boolean FOR TESTING RAISING cx_static_check,
      type_numeric FOR TESTING RAISING cx_static_check,
      type_string FOR TESTING RAISING cx_static_check,
      structure_with_language FOR TESTING RAISING cx_static_check,
      from_json_to_abap
        IMPORTING
          json   TYPE xstring
        EXPORTING
          result TYPE data
        RAISING
          cx_static_check
          cx_sxml_illegal_argument_error,

      from_abap_to_json
        IMPORTING
          test_type     TYPE data
        EXPORTING
          VALUE(result) TYPE string_table
          VALUE(json)   TYPE xstring
        RAISING
          cx_static_check
          cx_sxml_illegal_argument_error,
      teardown,
      assert_json_equals
        IMPORTING
          actual_json_stringtab   TYPE string_table
          expected_json_stringtab TYPE string_table.
ENDCLASS.


CLASS ltcl_integration_test IMPLEMENTATION.

  METHOD teardown.
    CLEAR exp_json.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD from_abap_to_json.
    DATA(cut) = NEW zcl_aff_writer_xslt( 'root' ).

    DATA(test_generator) = NEW zcl_aff_generator( cut ).
    DATA(st_content) = test_generator->generate_type( test_type ).

    DATA(st_name) = CONV progname( c_xslt_prefix && st_execution_counter ).
    st_name = |{ st_name WIDTH = 30 PAD = '=' }{ c_ext_xslt_source }|.

    INSERT REPORT st_name FROM st_content EXTENSION TYPE c_ext_xslt_source.

    DATA(json_writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    json_writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_linebreaks ).
    json_writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_indent value = '2' ).
    CALL TRANSFORMATION (st_name) SOURCE root = test_type RESULT XML json_writer.

    DATA(string) = cl_abap_codepage=>convert_from( json_writer->get_output( ) ).
    string = string && cl_abap_char_utilities=>newline.
    SPLIT string AT cl_abap_char_utilities=>newline INTO TABLE result.

    json = json_writer->get_output( ).

    st_execution_counter += 1.
  ENDMETHOD.

  METHOD from_json_to_abap.

    DATA(counter) = st_execution_counter - 1.
    DATA(st_name) = CONV progname( c_xslt_prefix && counter ).
    st_name = |{ st_name WIDTH = 30 PAD = '=' }{ c_ext_xslt_source }|.

    DATA st_result TYPE abap_trans_resbind_tab.
    FIELD-SYMBOLS <st_result> LIKE LINE OF st_result.

    CLEAR result.
    APPEND INITIAL LINE TO st_result ASSIGNING <st_result>.
    <st_result>-name = 'ROOT'.
    GET REFERENCE OF result  INTO <st_result>-value.

    DATA(json_reader) = cl_sxml_string_reader=>create( json ).
    TRY.
        CALL TRANSFORMATION (st_name)
          SOURCE XML json_reader
          RESULT (st_result).
      CATCH cx_root INTO DATA(exception).
        DELETE REPORT st_name.
        cl_abap_unit_assert=>fail( exception->get_text( ) ).
    ENDTRY.
    DELETE REPORT st_name.

  ENDMETHOD.


  METHOD table_structure.

    DATA test_type TYPE lif_test_types=>table_structure.
    test_type = VALUE #(
      ( element_1 = 1 element_2 = 'first_element' )
      ( element_1 = 2 element_2 = 'second_element' ) ).

    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring) ).

    exp_json = VALUE #(
        ( `[` )
        ( ` {` )
        ( `  "element1":1,` )
        ( `  "element2":"first_element"` )
        ( ` },` )
        ( ` {` )
        ( `  "element1":2,` )
        ( `  "element2":"second_element"` )
        ( ` }` )
        ( `]` ) ).

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type ).

  ENDMETHOD.

  METHOD include_table.
    DATA test_type TYPE lif_test_types=>include_table.
    test_type = VALUE #(
        table = VALUE #(
            ( element_1 = 1 element_2 = 'obj1_element_2_value' )
            ( element_1 = 2 element_2 = 'obj2_element_2_value' )
        )
        include_element_1 = 1 ).

    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring) ).
    exp_json = VALUE #(
        ( `{` )
        ( ` "table":` )
        ( ` [` )
        ( `  {` )
        ( `   "element1":1,` )
        ( `   "element2":"obj1_element_2_value"` )
        ( `  },` )
        ( `  {` )
        ( `   "element1":2,` )
        ( `   "element2":"obj2_element_2_value"` )
        ( `  }` )
        ( ` ],` )
        ( ` "includeElement1":1` )
        ( `}` ) ).

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type ).
  ENDMETHOD.

  METHOD structure_include_in_include.
    DATA test_type TYPE lif_test_types=>structure_include_in_include.
    test_type-include_element_1 = 'element1_value'.
    test_type-include_element_2 = 2.
    test_type-element = 'element_value'.

    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring) ).
    exp_json = VALUE #(
        ( `{` )
        ( ` "includeElement1":"element1_value",` )
        ( ` "includeElement2":2,` )
        ( ` "element":"element_value"` )
        ( `}` ) ).

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type ).
  ENDMETHOD.

  METHOD structure_with_incl.
    DATA test_type TYPE lif_test_types=>structure_with_include.
    test_type-include_element_1 = 'value of incl element1'.
    test_type-include_element_2 = 1.
    test_type-element_1 = 2.
    test_type-element_2 = 'value of element2'.

    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring) ).
    exp_json = VALUE #(
        ( `{` )
        ( ` "includeElement1":"value of incl element1",` )
        ( ` "includeElement2":1,` )
        ( ` "element1":2,` )
        ( ` "element2":"value of element2"` )
        ( `}` ) ).

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type ).
  ENDMETHOD.

  METHOD struc_tab_struc_tab.
    DATA(second_table_1) = VALUE lif_test_types=>table_build_in_type( ( `table_1_line_1` ) ( `table_1_line_2` ) ).
    DATA(second_table_2) = VALUE lif_test_types=>table_build_in_type( ( `table_2_line_1` ) ( `table_2_line_2` ) ).
    DATA(first_table) = VALUE lif_test_types=>first_table_type(
      ( second_table = second_table_1 )
      ( second_table = second_table_2 ) ).
    DATA(test_type) = VALUE lif_test_types=>struc_tab_struc_tab(
      first_table = first_table ).

    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring) ).
    exp_json = VALUE #(
        ( `{` )
        ( ` "firstTable":` )
        ( ` [` )
        ( `  {` )
        ( `   "secondTable":` )
        ( `   [` )
        ( `    "table_1_line_1",` )
        ( `    "table_1_line_2"` )
        ( `   ]` )
        ( `  },` )
        ( `  {` )
        ( `   "secondTable":` )
        ( `   [` )
        ( `    "table_2_line_1",` )
        ( `    "table_2_line_2"` )
        ( `   ]` )
        ( `  }` )
        ( ` ]` )
        ( `}` ) ).

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type ).
  ENDMETHOD.


  METHOD table_in_table.
    DATA test_type TYPE lif_test_types=>table_in_table.
    test_type = VALUE #(
        ( VALUE #( ( `table_1_line_1` ) ( `table_1_line_2` ) ) )
        ( VALUE #( ( `table_2_line_1` ) ( `table_2_line_2` ) ) ) ).

    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring) ).
    exp_json = VALUE #(
        ( `[` )
        ( ` [` )
        ( `  "table_1_line_1",` )
        ( `  "table_1_line_2"` )
        ( ` ],` )
        ( ` [` )
        ( `  "table_2_line_1",` )
        ( `  "table_2_line_2"` )
        ( ` ]` )
        ( `]` ) ).

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type ).
  ENDMETHOD.

  METHOD type_timestamp.
    DATA test_type TYPE timestamp.
    test_type = 20200424163000.

    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring) ).

    exp_json = VALUE #(
         ( `"2020-04-24T16:30:00+00:00"` ) ).

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type ).
  ENDMETHOD.

  METHOD type_boolean.
    DATA test_type TYPE abap_bool.
    test_type = 'X'.
    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring) ).

    exp_json = VALUE #(
            ( `true` ) ).

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type ).
  ENDMETHOD.

  METHOD type_numeric.
    DATA test_type TYPE decfloat16.
    test_type = '5.3' ##LITERAL.
    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring) ).

    exp_json = VALUE #(
            ( `5.3` ) ).

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type ).
  ENDMETHOD.

  METHOD type_string.
    DATA test_type TYPE c LENGTH 10.
    test_type = '0123abcdef'.
    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring) ).

    exp_json = VALUE #(
            ( `"0123abcdef"` ) ).

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type ).
  ENDMETHOD.


  METHOD structure_with_language.
    DATA test_type TYPE lif_test_types=>structure_with_language.
    test_type = VALUE #( language = 'E' language2 = 'D').

    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring) ).

    exp_json = VALUE #(
        ( `{` )
        ( `"language":"en",` )
        ( `"language2":"de"` )
        ( `}` ) ).
    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type ).
  ENDMETHOD.


  METHOD assert_json_equals.
    CONCATENATE LINES OF actual_json_stringtab INTO DATA(act_json_as_string).
    CONCATENATE LINES OF expected_json_stringtab INTO DATA(exp_json_as_string).
    CONDENSE act_json_as_string NO-GAPS.
    CONDENSE exp_json_as_string NO-GAPS.
    cl_abap_unit_assert=>assert_equals( msg = 'Expected json and actual json differ' exp = exp_json_as_string act = act_json_as_string ).
  ENDMETHOD.

ENDCLASS.

"!@testing ZCL_AFF_GENERATOR
CLASS ltcl_type_writer_xslt_ad DEFINITION FINAL FOR TESTING
  DURATION MEDIUM
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      exp_transformation TYPE string_table,
      cut                TYPE REF TO zcl_aff_writer_xslt,
      test_generator     TYPE REF TO zcl_aff_generator,
      st_root_name       TYPE string VALUE 'root' ##NO_TEXT,
      log                TYPE REF TO zif_aff_log.

    METHODS: setup,
      validate_output
        IMPORTING
          act          TYPE string_table
          no_log_check TYPE abap_boolean DEFAULT abap_false,
      simple_integer FOR TESTING RAISING cx_static_check,
      simple_string FOR TESTING RAISING cx_static_check,
      simple_data FOR TESTING RAISING cx_static_check,
      simple_structure FOR TESTING RAISING cx_static_check,
      simple_structure_required FOR TESTING RAISING cx_static_check,
      structure_in_structure FOR TESTING RAISING cx_static_check,
      simple_table FOR TESTING RAISING cx_static_check,
      simple_type_with_enum_values FOR TESTING RAISING cx_static_check,
      structure_with_enum_values FOR TESTING RAISING cx_static_check,
      deep_nested_structure FOR TESTING RAISING cx_static_check,
      nested_structure_with_table FOR TESTING RAISING cx_static_check,
      enum_values_with_wrong_link FOR TESTING RAISING cx_static_check,
      struc_with_table_not_req FOR TESTING RAISING cx_static_check,
      structure_different_default FOR TESTING RAISING cx_static_check,
      nested_struc_with_default FOR TESTING RAISING cx_static_check,
      structure_with_callback FOR TESTING RAISING cx_static_check,
      struc_in_struc_with_callback FOR TESTING RAISING cx_static_check,
      table_of_struc_with_callback FOR TESTING RAISING cx_static_check,
      simple_element_with_callack FOR TESTING RAISING cx_static_check,
      table_with_callback FOR TESTING RAISING cx_static_check,
      table_with_call_of_table FOR TESTING RAISING cx_static_check,
      struc_of_table_with_callback FOR TESTING RAISING cx_static_check,
      structure_with_wrong_default FOR TESTING RAISING cx_static_check,
      structure_with_wrong_callback FOR TESTING RAISING cx_static_check,
      type_of_enumtype_and_co_differ FOR TESTING RAISING cx_static_check,
      wrong_default_type_link FOR TESTING RAISING cx_static_check,
      structure_with_enums FOR TESTING RAISING cx_static_check,
      structure_with_default_problem FOR TESTING RAISING cx_static_check,
      struc_with_own_enum_values FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_type_writer_xslt_ad IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_aff_writer_xslt( me->st_root_name ).
    test_generator = NEW zcl_aff_generator( cut ).
  ENDMETHOD.

  METHOD simple_integer.
    DATA test_type TYPE zcl_aff_test_types=>integer.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `    <num>` )
        ( `        <tt:value option="format(alpha)"/>` )
        ( `    </num>` )
        ( `</tt:cond>` ) ).
    validate_output( act_output ).
  ENDMETHOD.

  METHOD simple_string.
    DATA test_type TYPE zcl_aff_test_types=>mystring.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `    <str>` )
        ( `        <tt:value/>` )
        ( `    </str>` )
        ( `</tt:cond>` ) ).
    validate_output( act_output ).
  ENDMETHOD.

  METHOD simple_data.
    DATA test_type TYPE zcl_aff_test_types=>my_date.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `    <str>` )
        ( `    <tt:value option="format(dateTimeOffset)"/>` )
        ( `    </str>` )
        ( `</tt:cond>` ) ).
    validate_output( act_output ).
  ENDMETHOD.

  METHOD simple_structure.
    DATA test_type TYPE zcl_aff_test_types=>my_structure.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
            ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="not-initial(MY_FIRST_ELEMENT)" frq="?">` )
        ( `        <str name="myFirstElement">` )
        ( `          <tt:value ref="MY_FIRST_ELEMENT"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="not-initial(MY_SECOND_ELEMENT)" frq="?">` )
        ( `        <num name="mySecondElement">` )
        ( `          <tt:value ref="MY_SECOND_ELEMENT" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'myFirstElement;mySecondElement;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).
    validate_output( act_output ).
  ENDMETHOD.

  METHOD simple_structure_required.
    DATA test_type TYPE zcl_aff_test_types=>my_structure2.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:group>` )
        ( `      <tt:cond frq="?">` )
        ( `        <str name="myFirstElement">` )
        ( `          <tt:value ref="MY_FIRST_ELEMENT"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="not-initial(MY_SECOND_ELEMENT)" frq="?">` )
        ( `        <num name="mySecondElement">` )
        ( `          <tt:value ref="MY_SECOND_ELEMENT" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'myFirstElement;mySecondElement;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).
    validate_output( act = act_output no_log_check = abap_true ).
    log = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text(
      log                = log
      exp_text           = `Annotation $ructure is unknown`
      exp_type           = zif_aff_log=>c_message_type-warning
      exp_component_name = `MY_STRUCTURE2` ).

  ENDMETHOD.

  METHOD structure_in_structure.
    DATA test_type TYPE zcl_aff_test_types=>my_structure3.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="not-initial(NESTED_STRUC)" frq="?">` )
        ( `        <object name="nestedStruc" tt:ref="NESTED_STRUC">` )
        ( `          <tt:group>` )
        ( `            <tt:cond s-check="not-initial(MY_ELEMENT)" frq="?">` )
        ( `              <str name="myElement">` )
        ( `                <tt:value ref="MY_ELEMENT"/>` )
        ( `              </str>` )
        ( `            </tt:cond>` )
        ( `            <tt:d-cond frq="*">` )
        ( `               <_ tt:lax="on">` )
        ( `                <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `                  <tt:with-parameter name="MEMBERS" val="'myElement;'"/>` )
        ( `                </tt:call-method>` )
        ( `                <tt:skip/>` )
        ( `              </_>` )
        ( `            </tt:d-cond>` )
        ( `            <tt:d-cond frq="?">` )
        ( `              <__/>` )
        ( `            </tt:d-cond>` )
        ( `          </tt:group>` )
        ( `        </object>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond frq="?">` )
        ( `        <str name="myElement">` )
        ( `          <tt:value ref="MY_ELEMENT"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'nestedStruc;myElement;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).

    validate_output( act_output ).
  ENDMETHOD.

  METHOD simple_table.
    DATA test_type TYPE zcl_aff_test_types=>my_standard_table.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `    <array>` )
        ( `        <tt:loop>` )
        ( `            <tt:group>` )
        ( `                <tt:cond>` )
        ( `                    <str>` )
        ( `                        <tt:value/>` )
        ( `                    </str>` )
        ( `                </tt:cond>` )
        ( `            </tt:group>` )
        ( `        </tt:loop>` )
        ( `    </array>` )
        ( `</tt:cond>` ) ).
    validate_output( act_output ).
  ENDMETHOD.

  METHOD simple_type_with_enum_values.
    DATA test_type TYPE zcl_aff_test_types=>category.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
            ( `<tt:cond>` )
            ( `      <str>` )
            ( `        <tt:value map="` )
            ( `          val(N('00'))=xml('general'),` )
            ( `          val(N('01'))=xml('classicBadi')` )
            ( `        "/>` )
            ( `      </str>` )
            ( `    </tt:cond>` ) ).
    validate_output( act = act_output no_log_check = abap_true ).
    log = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text(
      log                = log
      exp_text           = zif_aff_log=>co_msg127
      exp_type           = zif_aff_log=>c_message_type-warning
      exp_component_name = `CATEGORY` ).
  ENDMETHOD.

  METHOD structure_with_enums.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_different_enum.
    test_generator->generate_type( test_type ).
    DATA(log) = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = zif_aff_log=>co_msg127
                                                              exp_component_name = `STRUCTURE_WITH_DIFFERENT_ENUM-ENUM_WITHOUT_ALL`
                                                              exp_type           = zif_aff_log=>c_message_type-warning ).
  ENDMETHOD.

  METHOD enum_values_with_wrong_link.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_wrong_link.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="not-initial(ELEMENT_ONE)" frq="?">` )
        ( `        <str name="elementOne">` )
        ( `          <tt:value ref="ELEMENT_ONE"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="not-initial(ELEMENT_TWO)" frq="?">` )
        ( `        <str name="elementTwo">` )
        ( `          <tt:value ref="ELEMENT_TWO"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'elementOne;elementTwo;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).
    validate_output( act = act_output no_log_check = abap_true ).
    log = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text(
      log                = log
      exp_text           = 'Constant ZCL_AFF_TEST_TYPES=>ENUM_VALUES_WRONG given in ABAP Doc link doesn''t exist'
      exp_type           = zif_aff_log=>c_message_type-warning
      exp_component_name = `STRUCTURE_WITH_WRONG_LINK-ELEMENT_TWO` ).
  ENDMETHOD.

  METHOD structure_with_enum_values.
    DATA test_type TYPE zcl_aff_test_types=>ty_class_properties.
    DATA(act_output) = test_generator->generate_type( test_type ).
    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:assign to-ref="CLASS_CATEGORY" val="N('00')"/>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="not-initial(HEADER)" frq="?">` )
        ( `        <object name="header" tt:ref="HEADER">` )
        ( `         <tt:assign to-ref="ABAP_LANGUAGE_VERSION" val="C('')"/>` )
        ( `          <tt:group>` )
        ( `            <tt:cond s-check="not-initial(DESCRIPTION)" frq="?">` )
        ( `              <str name="description">` )
        ( `                <tt:value ref="DESCRIPTION"/>` )
        ( `              </str>` )
        ( `            </tt:cond>` )
        ( `            <tt:cond s-check="not-initial(ORIGINAL_LANGUAGE)" frq="?">` )
        ( `              <str name="originalLanguage">` )
        ( `                <tt:call-method class="cl_aff_xslt_callback_language" d-name="deserialize" reader="reader" s-name="serialize" writer="writer"> ` )
        ( `                  <tt:with-parameter name="language" ref="ORIGINAL_LANGUAGE"/>` )
        ( `                </tt:call-method>` )
        ( `              </str>` )
        ( `            </tt:cond>` )
        ( `            <tt:cond s-check="ABAP_LANGUAGE_VERSION!=C('')" frq="?">` )
        ( `              <str name="abapLanguageVersion">` )
        ( `                <tt:value ref="ABAP_LANGUAGE_VERSION" map="` )
        ( `                  val('')=xml('standard'),` )
        ( `                  val('5')=xml('cloudDevelopment')` )
        ( `                "/>` )
        ( `              </str>` )
        ( `            </tt:cond>` )
        ( `            <tt:d-cond frq="*">` )
        ( `               <_ tt:lax="on">` )
        ( `                <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `                  <tt:with-parameter name="MEMBERS" val="'description;originalLanguage;abapLanguageVersion;'"/>` )
        ( `                </tt:call-method>` )
        ( `                <tt:skip/>` )
        ( `              </_>` )
        ( `            </tt:d-cond>` )
        ( `            <tt:d-cond frq="?">` )
        ( `              <__/>` )
        ( `            </tt:d-cond>` )
        ( `          </tt:group>` )
        ( `        </object>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="CLASS_CATEGORY!=N('00')" frq="?">` )
        ( `        <str name="classCategory">` )
        ( `          <tt:value ref="CLASS_CATEGORY" map="` )
        ( `            val(N('00'))=xml('general'),` )
        ( `            val(N('01'))=xml('exitClass')` )
        ( `          "/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'header;classCategory;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).
    validate_output( act = act_output ).
  ENDMETHOD.

  METHOD deep_nested_structure.
    DATA test_type TYPE zcl_aff_test_types=>list.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:group>` )
        ( `      <tt:cond frq="?">` )
        ( `        <num name="field1">` )
        ( `          <tt:value ref="FIELD1" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond frq="?">` )
        ( `        <str name="field2">` )
        ( `          <tt:value ref="FIELD2"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond frq="?">` )
        ( `        <object name="list1" tt:ref="LIST1">` )
        ( `          <tt:group>` )
        ( `            <tt:cond frq="?">` )
        ( `              <num name="elementOfList1">` )
        ( `                <tt:value ref="ELEMENT_OF_LIST1" option="format(alpha)"/>` )
        ( `              </num>` )
        ( `            </tt:cond>` )
        ( `            <tt:cond s-check="not-initial(LIST2)" frq="?">` )
        ( `              <object name="list2" tt:ref="LIST2">` )
        ( `                <tt:group>` )
        ( `                  <tt:cond frq="?">` )
        ( `                    <str name="elementOfList2">` )
        ( `                      <tt:value ref="ELEMENT_OF_LIST2"/>` )
        ( `                    </str>` )
        ( `                  </tt:cond>` )
        ( `                  <tt:d-cond frq="*">` )
        ( `                     <_ tt:lax="on">` )
        ( `                      <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `                        <tt:with-parameter name="MEMBERS" val="'elementOfList2;'"/>` )
        ( `                      </tt:call-method>` )
        ( `                      <tt:skip/>` )
        ( `                    </_>` )
        ( `                  </tt:d-cond>` )
        ( `                  <tt:d-cond frq="?">` )
        ( `                    <__/>` )
        ( `                  </tt:d-cond>` )
        ( `                </tt:group>` )
        ( `              </object>` )
        ( `            </tt:cond>` )
        ( `            <tt:d-cond frq="*">` )
        ( `               <_ tt:lax="on">` )
        ( `                <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `                  <tt:with-parameter name="MEMBERS" val="'elementOfList1;list2;'"/>` )
        ( `                </tt:call-method>` )
        ( `                <tt:skip/>` )
        ( `              </_>` )
        ( `            </tt:d-cond>` )
        ( `            <tt:d-cond frq="?">` )
        ( `              <__/>` )
        ( `            </tt:d-cond>` )
        ( `          </tt:group>` )
        ( `        </object>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond frq="?">` )
        ( `        <str name="field3">` )
        ( `          <tt:value ref="FIELD3"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'field1;field2;list1;field3;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).
    validate_output( act_output ).
  ENDMETHOD.

  METHOD nested_structure_with_table.
    DATA test_type TYPE zcl_aff_test_types=>outer_struc.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="not-initial(FIELD1)" frq="?">` )
        ( `        <num name="field1">` )
        ( `          <tt:value ref="FIELD1" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="not-initial(INNER_STRUC)" frq="?">` )
        ( `        <object name="innerStruc" tt:ref="INNER_STRUC">` )
        ( `          <tt:group>` )
        ( `            <tt:cond frq="?">` )
        ( `              <num name="elementOfInnerStruc">` )
        ( `                <tt:value ref="ELEMENT_OF_INNER_STRUC" option="format(alpha)"/>` )
        ( `              </num>` )
        ( `            </tt:cond>` )
        ( `            <tt:cond frq="?">` )
        ( `              <array name="innerTableVar">` )
        ( `                <tt:loop ref="INNER_TABLE_VAR">` )
        ( `                  <tt:group>` )
        ( `                    <tt:cond>` )
        ( `                      <str>` )
        ( `                        <tt:value/>` )
        ( `                      </str>` )
        ( `                    </tt:cond>` )
        ( `                  </tt:group>` )
        ( `                </tt:loop>` )
        ( `              </array>` )
        ( `            </tt:cond>` )
        ( `            <tt:d-cond frq="*">` )
        ( `               <_ tt:lax="on">` )
        ( `                <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `                  <tt:with-parameter name="MEMBERS" val="'elementOfInnerStruc;innerTableVar;'"/>` )
        ( `                </tt:call-method>` )
        ( `                <tt:skip/>` )
        ( `              </_>` )
        ( `            </tt:d-cond>` )
        ( `            <tt:d-cond frq="?">` )
        ( `              <__/>` )
        ( `            </tt:d-cond>` )
        ( `          </tt:group>` )
        ( `        </object>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond frq="?">` )
        ( `        <str name="field2">` )
        ( `          <tt:value ref="FIELD2"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'field1;innerStruc;field2;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).
    validate_output( act_output ).
  ENDMETHOD.

  METHOD struc_with_table_not_req.
    DATA test_type TYPE zcl_aff_test_types=>aff_test_type.
    DATA(act_output) = test_generator->generate_type( test_type ).

    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="not-initial(FIELD1)" frq="?">` )
        ( `        <num name="field1">` )
        ( `          <tt:value ref="FIELD1" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond frq="?">` )
        ( `        <object name="innerStruc" tt:ref="INNER_STRUC">` )
        ( `          <tt:group>` )
        ( `            <tt:cond frq="?">` )
        ( `              <num name="innerElement">` )
        ( `                <tt:value ref="INNER_ELEMENT" option="format(alpha)"/>` )
        ( `              </num>` )
        ( `            </tt:cond>` )
        ( `            <tt:cond s-check="not-initial(INNER_TABLE)" frq="?">` )
        ( `              <array name="innerTable">` )
        ( `                <tt:loop ref="INNER_TABLE">` )
        ( `                  <tt:group>` )
        ( `                    <tt:cond>` )
        ( `                      <str>` )
        ( `                        <tt:value/>` )
        ( `                      </str>` )
        ( `                    </tt:cond>` )
        ( `                  </tt:group>` )
        ( `                </tt:loop>` )
        ( `              </array>` )
        ( `            </tt:cond>` )
        ( `            <tt:d-cond frq="*">` )
        ( `               <_ tt:lax="on">` )
        ( `                <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `                  <tt:with-parameter name="MEMBERS" val="'innerElement;innerTable;'"/>` )
        ( `                </tt:call-method>` )
        ( `                <tt:skip/>` )
        ( `              </_>` )
        ( `            </tt:d-cond>` )
        ( `            <tt:d-cond frq="?">` )
        ( `              <__/>` )
        ( `            </tt:d-cond>` )
        ( `          </tt:group>` )
        ( `        </object>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond frq="?">` )
        ( `        <str name="field2">` )
        ( `          <tt:value ref="FIELD2"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'field1;innerStruc;field2;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).
    validate_output( act_output ).
  ENDMETHOD.


  METHOD structure_different_default.
    DATA test_type TYPE zcl_aff_test_types=>structure_different_default.
    DATA(act_output) = test_generator->generate_type( test_type ).
    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:assign to-ref="FOUR_BYTE_INT" val="I(5)"/>` )
        ( `    <tt:assign to-ref="EIGHT_BYTE_INT" val="INT8(55)"/>` )
        ( `    <tt:assign to-ref="BIN_FLOAT" val="F('4.3')"/>` )
        ( `    <tt:assign to-ref="BYTE_LIKE" val="X('FFFF')"/>` )
        ( `    <tt:assign to-ref="BYTE_LIKE2" val="X('FF00FF')"/>` )
        ( `    <tt:assign to-ref="DECIMAL_FLOAT_16" val="DECFLOAT16('25.26')"/>` )
        ( `    <tt:assign to-ref="DECIMAL_FLOAT_34" val="DECFLOAT34('123.05')"/>` )
        ( `    <tt:assign to-ref="PACKED_NUMBER" val="P(123.45)"/>` )
        ( `    <tt:assign to-ref="NUMERIC_TEXT" val="N('1067')"/>` )
        ( `    <tt:assign to-ref="CHARACTER_TEXT" val="C('abcde')"/>` )
        ( `    <tt:assign to-ref="STRING_TEXT" val="C('Default text')"/>` )
        ( `    <tt:assign to-ref="DATE_FIELD" val="D('19720401')"/>` )
        ( `    <tt:assign to-ref="TIME_FIELD" val="T('201500')"/>` )
        ( `    <tt:assign to-ref="BOOL_TRUE" val="C('X')"/>` )
        ( `    <tt:assign to-ref="BOOL_FALSE" val="C('')"/>` )
        ( `    <tt:assign to-ref="ENUM_TYPE" val="N('01')"/>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="FOUR_BYTE_INT!=I(5)" frq="?">` )
        ( `        <num name="fourByteInt">` )
        ( `          <tt:value ref="FOUR_BYTE_INT" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="EIGHT_BYTE_INT!=INT8(55)" frq="?">` )
        ( `        <num name="eightByteInt">` )
        ( `          <tt:value ref="EIGHT_BYTE_INT" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="BIN_FLOAT!=F('4.3')" frq="?">` )
        ( `        <num name="binFloat">` )
        ( `          <tt:value ref="BIN_FLOAT" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="BYTE_LIKE!=X('FFFF')" frq="?">` )
        ( `        <str name="byteLike">` )
        ( `          <tt:value ref="BYTE_LIKE"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="BYTE_LIKE2!=X('FF00FF')" frq="?">` )
        ( `        <str name="byteLike2">` )
        ( `          <tt:value ref="BYTE_LIKE2"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="DECIMAL_FLOAT_16!=DECFLOAT16('25.26')" frq="?">` )
        ( `        <num name="decimalFloat16">` )
        ( `          <tt:value ref="DECIMAL_FLOAT_16" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="DECIMAL_FLOAT_34!=DECFLOAT34('123.05')" frq="?">` )
        ( `        <num name="decimalFloat34">` )
        ( `          <tt:value ref="DECIMAL_FLOAT_34" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="PACKED_NUMBER!=P(123.45)" frq="?">` )
        ( `        <num name="packedNumber">` )
        ( `          <tt:value ref="PACKED_NUMBER" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="NUMERIC_TEXT!=N('1067')" frq="?">` )
        ( `        <str name="numericText">` )
        ( `          <tt:value ref="NUMERIC_TEXT"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="CHARACTER_TEXT!=C('abcde')" frq="?">` )
        ( `        <str name="characterText">` )
        ( `          <tt:value ref="CHARACTER_TEXT"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="STRING_TEXT!=C('Default text')" frq="?">` )
        ( `        <str name="stringText">` )
        ( `          <tt:value ref="STRING_TEXT"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="DATE_FIELD!=D('19720401')" frq="?">` )
        ( `        <str name="dateField">` )
        ( `          <tt:value ref="DATE_FIELD" option="format(dateTimeOffset)"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="TIME_FIELD!=T('201500')" frq="?">` )
        ( `        <str name="timeField">` )
        ( `          <tt:value ref="TIME_FIELD" option="format(dateTimeOffset)"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="not-initial(DATE_TIME_FIELD)" frq="?">` )
        ( `        <str name="dateTimeField">` )
        ( `          <tt:value ref="DATE_TIME_FIELD" option="format(dateTimeOffset)"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="BOOL_TRUE!=C('X')" frq="?">` )
        ( `        <bool name="boolTrue">` )
        ( `          <tt:value ref="BOOL_TRUE" option="format(boolean)"/>` )
        ( `        </bool>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="BOOL_FALSE!=C('')" frq="?">` )
        ( `        <bool name="boolFalse">` )
        ( `          <tt:value ref="BOOL_FALSE" option="format(boolean)"/>` )
        ( `        </bool>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="ENUM_TYPE!=N('01')" frq="?">` )
        ( `        <str name="enumType">` )
        ( `          <tt:value ref="ENUM_TYPE" map="` )
        ( `            val(N('00'))=xml('general'),` )
        ( `            val(N('01'))=xml('exitClass')` )
        ( `          "/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER"> ` )
        ( `            <tt:with-parameter name="MEMBERS" val="'fourByteInt;eightByteInt;binFloat;byteLike;byteLike2;decimalFloat16;decimalFloat34;` &&
          `packedNumber;numericText;characterText;stringText;dateField;timeField;dateTimeField;boolTrue;boolFalse;enumType;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).
    validate_output( act = act_output no_log_check = abap_true ).
    log = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text(
      log                = log
      exp_text           = `Annotation $default for type UTCLONG is not supported`
      exp_component_name = `STRUCTURE_DIFFERENT_DEFAULT-DATE_TIME_FIELD`
      exp_type           = zif_aff_log=>c_message_type-warning ).
  ENDMETHOD.

  METHOD structure_with_default_problem.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_default_problem.
    DATA(act_output) = test_generator->generate_type( test_type ).
    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:assign to-ref="STRING_ELEMENT" val="C('DefaultString')"/>` )
        ( `    <tt:assign to-ref="ENUM_SHOW_ALWAYS" val="N('01')"/>` )
        ( `    <tt:group>` )
        ( `      <tt:cond frq="?">` )
        ( `        <num name="integer">` )
        ( `          <tt:value ref="INTEGER" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond frq="?">` )
        ( `        <str name="stringElement">` )
        ( `          <tt:value ref="STRING_ELEMENT"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond frq="?">` )
        ( `        <str name="enumRequired">` )
        ( `          <tt:value ref="ENUM_REQUIRED" map="` )
        ( `            val(N('00'))=xml('general'),` )
        ( `            val(N('01'))=xml('exitClass')` )
        ( `          "/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond frq="?">` )
        ( `        <str name="enumShowAlways">` )
        ( `          <tt:value ref="ENUM_SHOW_ALWAYS" map="` )
        ( `            val(N('00'))=xml('general'),` )
        ( `            val(N('01'))=xml('exitClass')` )
        ( `          "/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'integer;stringElement;enumRequired;enumShowAlways;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).
    validate_output( act = act_output no_log_check = abap_true ).
    log = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = zif_aff_log=>co_msg126
                                                              exp_component_name = `STRUCTURE_WITH_DEFAULT_PROBLEM-INTEGER`
                                                              exp_type           = zif_aff_log=>c_message_type-warning ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = zif_aff_log=>co_msg126
                                                              exp_component_name = `STRUCTURE_WITH_DEFAULT_PROBLEM-ENUM_REQUIRED`
                                                              exp_type           = zif_aff_log=>c_message_type-warning ).
  ENDMETHOD.

  METHOD nested_struc_with_default.
    DATA test_type TYPE zcl_aff_test_types=>nested_struc_with_default.
    DATA(act_output) = test_generator->generate_type( test_type ).
    me->exp_transformation = VALUE #(
           ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:assign to-ref="OUTER_COMPONENT" val="I(10)"/>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="OUTER_COMPONENT!=I(10)" frq="?">` )
        ( `        <num name="outerComponent">` )
        ( `          <tt:value ref="OUTER_COMPONENT" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="not-initial(MIDDLE_STRUC)" frq="?">` )
        ( `        <object name="middleStruc" tt:ref="MIDDLE_STRUC">` )
        ( `          <tt:assign to-ref="MIDDLE_COMPONENT" val="C('abcd')"/>` )
        ( `          <tt:group>` )
        ( `            <tt:cond s-check="MIDDLE_COMPONENT!=C('abcd')" frq="?">` )
        ( `              <str name="middleComponent">` )
        ( `                <tt:value ref="MIDDLE_COMPONENT"/>` )
        ( `              </str>` )
        ( `            </tt:cond>` )
        ( `            <tt:cond s-check="not-initial(INNER_STRUC)" frq="?">` )
        ( `              <object name="innerStruc" tt:ref="INNER_STRUC">` )
        ( `                <tt:assign to-ref="INNER_COMPONENT" val="C('Default Value')"/>` )
        ( `                <tt:group>` )
        ( `                  <tt:cond s-check="INNER_COMPONENT!=C('Default Value')" frq="?">` )
        ( `                    <str name="innerComponent">` )
        ( `                      <tt:value ref="INNER_COMPONENT"/>` )
        ( `                    </str>` )
        ( `                  </tt:cond>` )
        ( `                  <tt:d-cond frq="*">` )
        ( `                     <_ tt:lax="on">` )
        ( `                      <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `                        <tt:with-parameter name="MEMBERS" val="'innerComponent;'"/>` )
        ( `                      </tt:call-method>` )
        ( `                      <tt:skip/>` )
        ( `                    </_>` )
        ( `                  </tt:d-cond>` )
        ( `                  <tt:d-cond frq="?">` )
        ( `                    <__/>` )
        ( `                  </tt:d-cond>` )
        ( `                </tt:group>` )
        ( `              </object>` )
        ( `            </tt:cond>` )
        ( `            <tt:d-cond frq="*">` )
        ( `               <_ tt:lax="on">` )
        ( `                <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `                  <tt:with-parameter name="MEMBERS" val="'middleComponent;innerStruc;'"/>` )
        ( `                </tt:call-method>` )
        ( `                <tt:skip/>` )
        ( `              </_>` )
        ( `            </tt:d-cond>` )
        ( `            <tt:d-cond frq="?">` )
        ( `              <__/>` )
        ( `            </tt:d-cond>` )
        ( `          </tt:group>` )
        ( `        </object>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'outerComponent;middleStruc;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).
    validate_output( act_output ).
  ENDMETHOD.

  METHOD type_of_enumtype_and_co_differ.
    TRY.
        DATA test_type TYPE zcl_aff_test_types=>enum.
*    expect error /no output (type of type and constant has to be the same)
        test_generator->generate_type( test_type ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_aff_tools.
    ENDTRY.
  ENDMETHOD.

  METHOD wrong_default_type_link.
    DATA test_type TYPE zcl_aff_test_types=>struc_link_wrong_type.
    DATA(act_output) = test_generator->generate_type( test_type ).
    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="not-initial(DEFAULT_LINK)" frq="?">` )
        ( `        <num name="defaultLink">` )
        ( `          <tt:value ref="DEFAULT_LINK" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'defaultLink;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).

    validate_output( act = act_output no_log_check = abap_true ).
    log = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text(
      log                = log
      exp_text           = |Type of constant CO_TEST does not match type of STRUC_LINK_WRONG_TYPE-DEFAULT_LINK|
      exp_component_name = `STRUC_LINK_WRONG_TYPE-DEFAULT_LINK`
      exp_type           = zif_aff_log=>c_message_type-warning ).
  ENDMETHOD.

  METHOD structure_with_wrong_default.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_wrong_default.
    DATA(act_output) = test_generator->generate_type( test_type ).
    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="not-initial(ELEMENT_ONE)" frq="?">` )
        ( `        <str name="elementOne">` )
        ( `          <tt:value ref="ELEMENT_ONE" map="` )
        ( `            val(N('00'))=xml('general'),` )
        ( `            val(N('01'))=xml('classicBadi')` )
        ( `          "/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="not-initial(ELEMENT_TWO)" frq="?">` )
        ( `        <str name="elementTwo">` )
        ( `          <tt:value ref="ELEMENT_TWO" map="` )
        ( `            val(N('00'))=xml('general'),` )
        ( `            val(N('01'))=xml('classicBadi')` )
        ( `          "/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'elementOne;elementTwo;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).
    validate_output( act = act_output no_log_check = abap_true ).
    log = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = 'Component WRONG_COMPONENT of constant ENUM_VALUES in ABAP Doc link doesn''t exist'
                                                              exp_type           = zif_aff_log=>c_message_type-warning
                                                              exp_component_name = `STRUCTURE_WITH_WRONG_DEFAULT-ELEMENT_ONE` ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = |Link in annotation { zcl_aff_abap_doc_parser=>abap_doc_annotation-default } is incorrect|
                                                              exp_type           = zif_aff_log=>c_message_type-warning
                                                              exp_component_name = `STRUCTURE_WITH_WRONG_DEFAULT-ELEMENT_TWO` ).
  ENDMETHOD.

  METHOD simple_element_with_callack.
    DATA test_type TYPE zcl_aff_test_types=>simple_callback.
    DATA(act_output) = test_generator->generate_type( test_type ).
    DATA(exp_schema) = VALUE string_table(
( `  <str>` )
( `    <tt:call-method class="zcl_aff_test_types" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` )
( |      <tt:with-parameter name="simple_callback" ref=".{ st_root_name }"/>| )
( `    </tt:call-method>` )
( `  </str>` ) ).
    INSERT LINES OF exp_schema INTO TABLE me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD table_with_callback.
    DATA test_type TYPE zcl_aff_test_types=>table_callback.
    DATA(act_output) = test_generator->generate_type( test_type ).
    DATA(exp_schema) = VALUE string_table(
( `  <array>` )
( `    <tt:call-method class="zcl_aff_test_types" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` )
( |      <tt:with-parameter name="table_callback" ref=".{ st_root_name }"/>| )
( `    </tt:call-method>` )
( `  </array>` ) ).
    INSERT LINES OF exp_schema INTO TABLE me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD table_with_call_of_table.
    DATA test_type TYPE zcl_aff_test_types=>table_call_of_table.
    DATA(act_output) = test_generator->generate_type( test_type ).
    DATA(exp_schema) = VALUE string_table(
( `  <array>` )
( `    <tt:call-method class="zcl_aff_test_types" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` )
( |      <tt:with-parameter name="table_call_of_table" ref=".{ st_root_name }"/>| )
( `    </tt:call-method>` )
( `  </array>` ) ).
    INSERT LINES OF exp_schema INTO TABLE me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD structure_with_callback.
    DATA test_type TYPE zcl_aff_test_types=>structure_callback.
    DATA(act_output) = test_generator->generate_type( test_type ).
    DATA(exp_schema) = VALUE string_table(
( `  <object>` )
( `    <tt:call-method class="zcl_aff_test_types" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` )
( |      <tt:with-parameter name="structure_callback" ref=".{ st_root_name }"/>| )
( `    </tt:call-method>` )
( `  </object>` ) ).
    INSERT LINES OF exp_schema INTO TABLE me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD table_of_struc_with_callback.
    DATA test_type TYPE zcl_aff_test_types=>table_of_struc_with_callback.
    DATA(act_output) = test_generator->generate_type( test_type ).
    me->exp_transformation = VALUE #(
        ( `   <tt:cond>` )
        ( `     <array>` )
        ( `       <tt:loop>` )
        ( `         <tt:group>` )
        ( `           <tt:cond>` )
        ( `             <object>` )
        ( `              <tt:call-method class="zcl_aff_test_types" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` )
        ( `                 <tt:with-parameter name="structure_callback" ref="$ref"/>` )
        ( `               </tt:call-method>` )
        ( `             </object>` )
        ( `           </tt:cond>` )
        ( `           </tt:group>` )
        ( `         </tt:loop>` )
        ( `       </array>` )
        ( `     </tt:cond>` ) ).
    validate_output( act = act_output ).
  ENDMETHOD.

  METHOD struc_of_table_with_callback.
    DATA test_type TYPE zcl_aff_test_types=>struc_of_table_with_callback.
    DATA(act_output) = test_generator->generate_type( test_type ).
    me->exp_transformation = VALUE #(
        ( `      <tt:cond>` )
        ( `        <object>` )
        ( `          <tt:group>` )
        ( `            <tt:cond>` )
        ( `              <array>` )
        ( `                <tt:call-method class="zcl_aff_test_types" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` )
        ( `                  <tt:with-parameter name="element_table_callback" ref="ELEMENT_TABLE_CALLBACK"/>` )
        ( `                </tt:call-method>` )
        ( `              </array>` )
        ( `            </tt:cond>` )
        ( `            <tt:cond s-check="not-initial(MY_SECOND_ELEMENT)" frq="?">` )
        ( `             <num name="mySecondElement">` )
        ( `              <tt:value ref="MY_SECOND_ELEMENT" option="format(alpha)"/>` )
        ( `             </num>` )
        ( `            </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'elementTableCallback;mySecondElement;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `          </tt:group>` )
        ( `        </object>` )
        ( `      </tt:cond>` ) ).
    validate_output( act_output ).
  ENDMETHOD.

  METHOD struc_in_struc_with_callback.
    DATA test_type TYPE zcl_aff_test_types=>struc_in_struc_with_callback.
    DATA(act_output) = test_generator->generate_type( test_type ).
    me->exp_transformation = VALUE #(
            ( `<tt:cond>` )
            ( `  <object>` )
            ( `    <tt:group>` )
            ( `      <tt:cond s-check="not-initial(MY_FIRST_ELEMENT)" frq="?">` )
            ( `            <str name="myFirstElement">` )
            ( `              <tt:value ref="MY_FIRST_ELEMENT"/>` )
            ( `            </str>` )
            ( `        </tt:cond>` )
            ( `        <tt:cond>` )
            ( `          <object>` )
            ( `            <tt:call-method class="zcl_aff_test_types" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` )
            ( `              <tt:with-parameter name="element_structure_callback" ref="ELEMENT_STRUCTURE_CALLBACK"/>` )
            ( `            </tt:call-method>` )
            ( `          </object>` )
            ( `        </tt:cond>` )
            ( `        <tt:cond s-check="not-initial(MY_THIRD_ELEMENT)" frq="?">` )
            ( `          <num name="myThirdElement">` )
            ( `            <tt:value ref="MY_THIRD_ELEMENT" option="format(alpha)"/>` )
            ( `          </num>` )
            ( `        </tt:cond>` )
            ( `      <tt:d-cond frq="*">` )
            ( `         <_ tt:lax="on">` )
            ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
            ( `            <tt:with-parameter name="MEMBERS" val="'myFirstElement;elementStructureCallback;myThirdElement;'"/>` )
            ( `         </tt:call-method>` )
            ( `         <tt:skip/>` )
            ( `      </_>` )
            ( `   </tt:d-cond>` )
            ( `   <tt:d-cond frq="?">` )
            ( `       <__/>` )
            ( `   </tt:d-cond>` )
            ( `    </tt:group>` )
            ( `  </object>` )
            ( `</tt:cond>` ) ).
    validate_output( act_output ).
  ENDMETHOD.

  METHOD structure_with_wrong_callback.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_wrong_callback.
    DATA(act_output) = test_generator->generate_type( test_type ).
    me->exp_transformation = VALUE #(

        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:group>` )
        ( `      <tt:cond frq="?">` )
        ( `        <str name="myFirstElement">` )
        ( `          <tt:value ref="MY_FIRST_ELEMENT"/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:cond s-check="not-initial(MY_SECOND_ELEMENT)" frq="?">` )
        ( `        <num name="mySecondElement">` )
        ( `          <tt:value ref="MY_SECOND_ELEMENT" option="format(alpha)"/>` )
        ( `        </num>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'myFirstElement;mySecondElement;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).
    validate_output( act = act_output no_log_check = abap_true ).
    log = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text( log                = log
                                                              exp_text           = zif_aff_log=>co_msg106
                                                              exp_component_name = `STRUCTURE_WITH_WRONG_CALLBACK-MY_FIRST_ELEMENT`
                                                              exp_type           = zif_aff_log=>c_message_type-warning ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_text(
      log                = log
      exp_text           = `Annotation $callbackClass was used incorrectly`
      exp_type           = zif_aff_log=>c_message_type-warning
      exp_component_name = `STRUCTURE_WITH_WRONG_CALLBACK-MY_SECOND_ELEMENT` ).
  ENDMETHOD.

  METHOD struc_with_own_enum_values.
    DATA test_type TYPE zcl_aff_test_types=>struc_with_own_enum_values.
    DATA(act_output) = test_generator->generate_type( test_type ).
    me->exp_transformation = VALUE #(
        ( `<tt:cond>` )
        ( `  <object>` )
        ( `    <tt:assign to-ref="ENUM_COMPONENT" val="C('AA')"/>` )
        ( `    <tt:group>` )
        ( `      <tt:cond s-check="ENUM_COMPONENT!=C('AA')" frq="?">` )
        ( `        <str name="enumComponent">` )
        ( `          <tt:value ref="ENUM_COMPONENT" map="` )
        ( `            val('AA')=xml('AAAA'),` )
        ( `            val('BB')=xml('BBBB')` )
        ( `          "/>` )
        ( `        </str>` )
        ( `      </tt:cond>` )
        ( `      <tt:d-cond frq="*">` )
        ( `         <_ tt:lax="on">` )
        ( `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` )
        ( `            <tt:with-parameter name="MEMBERS" val="'enumComponent;'"/>` )
        ( `          </tt:call-method>` )
        ( `          <tt:skip/>` )
        ( `        </_>` )
        ( `      </tt:d-cond>` )
        ( `      <tt:d-cond frq="?">` )
        ( `        <__/>` )
        ( `      </tt:d-cond>` )
        ( `    </tt:group>` )
        ( `  </object>` )
        ( `</tt:cond>` ) ).
    validate_output( act_output ).
  ENDMETHOD.

  METHOD validate_output.
    DATA exp TYPE string_table.

    APPEND `<?sap.transform simple?>` TO exp.
    APPEND `<tt:transform xmlns:tt="http://www.sap.com/transformation-templates">` TO exp.
    APPEND |<tt:root name="{ st_root_name }"/>| TO exp.
    APPEND `<tt:template>` TO exp.
    APPEND |<tt:ref name="{ st_root_name }">| TO exp.

    APPEND LINES OF me->exp_transformation TO exp.

    APPEND `</tt:ref>` TO exp.
    APPEND `</tt:template>` TO exp.
    APPEND `</tt:transform>` TO exp.
    zcl_aff_tools_unit_test_helper=>assert_equals_ignore_spaces( exp_data = exp act_data = act ).
    IF no_log_check = abap_false.
      log = cut->zif_aff_writer~get_log( ).
      zcl_aff_tools_unit_test_helper=>assert_log_has_no_message( log = log message_severity_threshold = zif_aff_log=>c_message_type-info ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS ltcl_integration_test_ad DEFINITION FINAL FOR TESTING
DURATION MEDIUM
RISK LEVEL DANGEROUS.

  PRIVATE SECTION.
    CONSTANTS:
      c_xslt_prefix     TYPE string VALUE `ZAFF_TRANSFORMATION_GEN_` ##NO_TEXT,
      c_ext_xslt_source TYPE c LENGTH 2 VALUE 'XT'.

    CLASS-DATA st_execution_counter TYPE i.

    DATA exp_json TYPE string_table.
    DATA manually_changed_json TYPE string.

    METHODS: simple_struc_with_extra_field FOR TESTING RAISING cx_static_check,

      structure_in_structure FOR TESTING RAISING cx_static_check,

      simple_table FOR TESTING RAISING cx_static_check,

      simple_type_with_enum_values FOR TESTING RAISING cx_static_check,

      structure_with_enum_values FOR TESTING RAISING cx_static_check,

      deep_nested_structure FOR TESTING RAISING cx_static_check,

      nested_structure_with_table FOR TESTING RAISING cx_static_check,

      structure_with_table_and_enum FOR TESTING RAISING cx_static_check,

      table_in_table FOR TESTING RAISING cx_static_check,

      simple_structure_with_required FOR TESTING RAISING cx_static_check,

      nested_struc_no_default FOR TESTING RAISING cx_static_check,

      nested_struc_with_default FOR TESTING RAISING cx_static_check,

      structure_different_default FOR TESTING RAISING cx_static_check,

      structure_with_callback FOR TESTING RAISING cx_static_check,

      struc_in_struc_with_callback FOR TESTING RAISING cx_static_check,

      simple_element_with_callback FOR TESTING RAISING cx_static_check,

      table_with_callback FOR TESTING RAISING cx_static_check,

      struc_of_table_with_callback FOR TESTING RAISING cx_static_check,

      structure_with_elem_callback FOR TESTING RAISING cx_static_check,

      table_of_struc_with_callback FOR TESTING RAISING cx_static_check,

      structure_with_num_text FOR TESTING RAISING cx_static_check,

      structure_with_include FOR TESTING RAISING cx_static_check,

      structure_with_default_problem FOR TESTING RAISING cx_static_check,

      struc_with_own_enum_values FOR TESTING RAISING cx_static_check,

      from_abap_to_json
        IMPORTING
          test_type     TYPE data
        EXPORTING
          VALUE(result) TYPE string_table
          VALUE(json)   TYPE xstring
        RAISING
          cx_static_check
          cx_sxml_illegal_argument_error,
      from_json_to_abap
        IMPORTING
          json   TYPE xstring
        EXPORTING
          result TYPE data
        RAISING
          zcx_aff_tools
          cx_sxml_illegal_argument_error,
      teardown,
      assert_json_equals
        IMPORTING
          actual_json_stringtab   TYPE string_table
          expected_json_stringtab TYPE string_table,
      do_integration_test
        IMPORTING
          test_type TYPE any
        CHANGING
          act_data  TYPE any
        RAISING
          cx_static_check
          cx_sxml_illegal_argument_error.
ENDCLASS.

CLASS ltcl_integration_test_ad IMPLEMENTATION.

  METHOD teardown.
    CLEAR exp_json.
    CLEAR manually_changed_json.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD simple_struc_with_extra_field.
    DATA test_type TYPE zcl_aff_test_types=>my_structure.
    test_type-my_second_element = 5.
    DATA act_data LIKE test_type.

    exp_json = VALUE #(
            ( `{` )
        ( ` "mySecondElement":5` )
        ( `}` ) ).

    manually_changed_json =
`{` &&
` "mySecondElement":5,` &&
` "additionalField": 10,` &&
` "oneMoreStructure": { ` &&
`    "unknownField": "extra"` &&
` }` &&
`}`.

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data ).
  ENDMETHOD.

  METHOD simple_structure_with_required.
    DATA test_type TYPE zcl_aff_test_types=>my_structure2.
    test_type-my_second_element = 5.
    DATA act_data LIKE test_type.

    exp_json = VALUE #(
        ( `{` )
        ( ` "myFirstElement":"",` )
        ( ` "mySecondElement":5` )
        ( `}` ) ).

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data ).
  ENDMETHOD.

  METHOD structure_in_structure.
    DATA test_type TYPE zcl_aff_test_types=>my_structure3.
    test_type = VALUE #(
      nested_struc = VALUE #( my_element = 'Nested Element')
      my_element = 'Not nested Element' ).
    DATA act_data LIKE test_type.
    exp_json = VALUE #(
        ( `{` )
        ( `  "nestedStruc": {` )
        ( `    "myElement":"Nested Element"` )
        ( `  },` )
        ( ` "myElement": "Not nested Element"` )
        ( `}` ) ).

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data ).
  ENDMETHOD.

  METHOD simple_table.
    DATA test_type TYPE zcl_aff_test_types=>my_standard_table.
    test_type = VALUE #( ( `line_1` ) ( `line_2` ) ).
    DATA act_data LIKE test_type.

    exp_json = VALUE #(
        ( `[` )
        ( `  "line_1",` )
        ( `  "line_2"` )
        ( `]` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data ).
  ENDMETHOD.

  METHOD simple_type_with_enum_values.
    DATA test_type TYPE zcl_aff_test_types=>category.
    DATA act_data LIKE test_type.

    exp_json = VALUE #(

        ( `  "general"` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data ).
  ENDMETHOD.

  METHOD structure_with_enum_values.
    DATA test_type TYPE zcl_aff_test_types=>ty_class_properties.
    test_type =
          VALUE #(
            header = VALUE #( description = 'description of the class' abap_language_version = 5 original_language = 'E' )
            class_category = '01' ).

    DATA act_data LIKE test_type.

    exp_json = VALUE #(
        ( `{` )
        ( `  "header": {` )
        ( `    "description":"description of the class",` )
        ( `    "originalLanguage":"en",` )
        ( `    "abapLanguageVersion":"cloudDevelopment"` )
        ( `  },` )
        ( ` "classCategory": "exitClass"` )
        ( `}` ) ).

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data ).
  ENDMETHOD.

  METHOD deep_nested_structure.
    DATA test_type TYPE zcl_aff_test_types=>list.
    test_type =
      VALUE #(
        field2 = 'AA'
        list1 =
        VALUE #( element_of_list1 = 50
                 list2 =
                 VALUE #(
                 element_of_list2 = 'Deep Nested'
                 )
                )
        field3 = 'ZZ' ).
    DATA act_data LIKE test_type.

    exp_json = VALUE #(
        ( `{` )
        ( `  "field1":0,` )
        ( `  "field2":"AA",` )
        ( `  "list1":{` )
        ( `    "elementOfList1":50,` )
        ( `    "list2":{` )
        ( `      "elementOfList2": "Deep Nested"` )
        ( `    }` )
        ( `  },` )
        ( ` "field3": "ZZ"` )
        ( `}` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data ).
  ENDMETHOD.

  METHOD nested_structure_with_table.
    DATA test_type TYPE zcl_aff_test_types=>outer_struc.
    test_type =
        VALUE #(
            inner_struc = VALUE #(
                element_of_inner_struc = 50
                inner_table_var = VALUE #( ( `line_1` ) ( `line_2` ) ) )
            field2 = 'ZZ' ).
    DATA act_data LIKE test_type.

    exp_json = VALUE #(
        ( `{` )
        ( `  "innerStruc": {` )
        ( `    "elementOfInnerStruc":50,` )
        ( `    "innerTableVar":[` )
        ( `      "line_1",` )
        ( `      "line_2"` )
        ( `    ]` )
        ( `  },` )
        ( ` "field2": "ZZ"` )
        ( `}` ) ).

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data ).
  ENDMETHOD.

  METHOD structure_with_table_and_enum.
    DATA test_type TYPE zcl_aff_test_types=>aff_test_type.
    test_type =
        VALUE #(
            field1 = 25
            inner_struc = VALUE #(
                inner_element = 50 )
            field2 = 'ZZ' ).
    DATA act_data LIKE test_type.

    exp_json = VALUE #(
        ( `{` )
        ( `  "field1":25,` )
        ( `  "innerStruc": {` )
        ( `    "innerElement":50` )
        ( `  },` )
        ( ` "field2": "ZZ"` )
        ( `}` ) ).

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data ).
  ENDMETHOD.

  METHOD table_in_table.
    DATA test_type TYPE zcl_aff_test_types=>first_table.
    test_type = VALUE #(
          ( VALUE #( ( `table_1_line_1` ) ( `table_1_line_2` ) ) )
          ( VALUE #( ( `table_2_line_1` ) ( `table_2_line_2` ) ) ) ).

    DATA act_data LIKE test_type.

    exp_json = VALUE #(
        ( `[` )
        ( ` [` )
        ( `  "table_1_line_1",` )
        ( `  "table_1_line_2"` )

        ( ` ],` )
        ( ` [` )
        ( `  "table_2_line_1",` )
        ( `  "table_2_line_2"` )

        ( ` ]` )
        ( `]` ) ).

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data ).
  ENDMETHOD.


  METHOD nested_struc_no_default.
    DATA test_type TYPE zcl_aff_test_types=>nested_struc_with_default.
    DATA act_data LIKE test_type.
    test_type = VALUE #( outer_component = 12
                         middle_struc = VALUE #( middle_component = 'wxyz'
                                                 inner_struc = VALUE #( inner_component = 'Inner Component' ) ) ).
    exp_json = VALUE #(
        ( `{` )
        ( `  "outerComponent":12,` )
        ( `  "middleStruc": {` )
        ( `    "middleComponent":"wxyz",` )
        ( `    "innerStruc": {` )
        ( `      "innerComponent":"Inner Component"` )
        ( `    }` )
        ( `  }` )
        ( `}` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data ).
  ENDMETHOD.

  METHOD nested_struc_with_default.
    DATA test_type TYPE zcl_aff_test_types=>nested_struc_with_default.
    DATA act_data LIKE test_type.
    test_type = VALUE #( outer_component = 10
                         middle_struc = VALUE #( middle_component = 'abcd'
                                                 inner_struc = VALUE #( inner_component = 'Default Value' ) ) ).
    exp_json = VALUE #(
        ( `{` )
        ( `  "middleStruc": {` )
        ( `    "innerStruc": { }` )
        ( `  }` )
        ( `}` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data ).
  ENDMETHOD.

  METHOD structure_different_default.
    DATA test_type TYPE zcl_aff_test_types=>structure_different_default.
    DATA act_data LIKE test_type.
    test_type = VALUE #( four_byte_int = 5
                          eight_byte_int = 55
                          bin_float = '4.3'
                          byte_like = 'FFFF'
                          byte_like2 = 'FF00FF'
                          decimal_float_16 = '25.26'
                          decimal_float_34 = '123.05'
                          packed_number = '123.45'
                          numeric_text = '1067'
                          character_text = 'abcde'
                          string_text = 'Default text'
                          date_field = '19720401'
                          time_field = '201500'
                          date_time_field = '9999-12-31T23:59:59.9999999'
                          bool_true = abap_true
                          bool_false = abap_false
                          enum_type = '01' ) ##LITERAL.
    exp_json = VALUE #(
        ( `{` )
        ( `"dateTimeField": "9999-12-31T23:59:59.9999999+00:00"` )
        ( `}` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data ).
  ENDMETHOD.

  METHOD structure_with_default_problem.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_default_problem.
    DATA act_data LIKE test_type.
    test_type = VALUE #( integer =  5
                          string_element = 'DefaultString'
                          enum_required = '01'
                          enum_show_always = '01' ) ##LITERAL.
    exp_json = VALUE #(
        ( `{` )
        ( `"integer" : 5,` )
        ( `"stringElement" : "DefaultString",` )
        ( `"enumRequired" : "exitClass",` )
        ( `"enumShowAlways" : "exitClass"` )
        ( `}` ) ).

    manually_changed_json =
  `{` &&
  `"integer" : 5,` &&
  `"enumRequired" : "exitClass"` &&
  `}`.

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data ).

  ENDMETHOD.


  METHOD simple_element_with_callback.
    DATA test_type TYPE zcl_aff_test_types=>simple_callback.
    test_type = 'String Element'.
    zcl_aff_test_types=>set_expected( test_type ).
    exp_json = VALUE #(
        ( `"callbackClass was called"` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type ).
  ENDMETHOD.

  METHOD table_with_callback.
    DATA test_type TYPE zcl_aff_test_types=>table_callback.
    test_type = VALUE #( ( `First` ) ( `Second` ) ).
    zcl_aff_test_types=>set_expected( test_type ).
    exp_json = VALUE #(
        ( `[` )
        ( `"callbackClass was called"` )
        ( `]` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type ).
  ENDMETHOD.

  METHOD structure_with_callback.
    DATA test_type TYPE zcl_aff_test_types=>structure_callback.
    test_type = VALUE #( element_name = 5 ).
    zcl_aff_test_types=>set_expected( test_type ).
    exp_json = VALUE #(
        ( `{` )
        ( `"elementName": "callbackClass was called"` )
        ( `}` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type ).
  ENDMETHOD.


  METHOD struc_of_table_with_callback.
    DATA test_type TYPE zcl_aff_test_types=>struc_of_table_with_callback.
    DATA table_with_callback LIKE test_type-element_table_callback.
    table_with_callback = VALUE #( ( `first` ) ( `second` ) ).
    zcl_aff_test_types=>set_expected( table_with_callback ).
    test_type = VALUE #( element_table_callback = table_with_callback my_second_element = 5 ).
    exp_json = VALUE #(
        ( `{` )
        ( `"elementTableCallback": [` )
        ( `  "callbackClass was called"` )
        ( ` ],` )
        ( `"mySecondElement": 5` )
        ( `}` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type ).
  ENDMETHOD.

  METHOD struc_in_struc_with_callback.
    DATA test_type TYPE zcl_aff_test_types=>struc_in_struc_with_callback.
    DATA element_structure_callback LIKE test_type-element_structure_callback.
    element_structure_callback = VALUE #( element_name = 5 ).
    zcl_aff_test_types=>set_expected( element_structure_callback ).
    test_type = VALUE #( my_first_element = 'firstElement'
                         element_structure_callback = element_structure_callback
                         my_third_element = 6 ).
    exp_json = VALUE #(
        ( `{` )
        ( `  "myFirstElement": "firstElement",` )
        ( `  "elementStructureCallback": {` )
        ( `     "elementName":"callbackClass was called"` )
        ( `  },` )
        ( `  "myThirdElement": 6` )
        ( `}` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type ).
  ENDMETHOD.

  METHOD structure_with_elem_callback.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_elem_callback.
    test_type = VALUE #( element_callback = 'My First Element'
                         my_second_element = 4 ).
    zcl_aff_test_types=>set_expected( 'callbackClass was called' ).
    exp_json = VALUE #(
        ( `{` )
        ( `"elementCallback": "callbackClass was called",` )
        ( `"mySecondElement": 4` )
        ( `}` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type ).
  ENDMETHOD.

  METHOD table_of_struc_with_callback.
    DATA test_type TYPE zcl_aff_test_types=>table_of_struc_with_callback.
    test_type = VALUE #( ( element_name = 5 ) ( element_name = 10 ) ).
    zcl_aff_test_types=>set_expected( VALUE zcl_aff_test_types=>structure_callback( element_name = 5 ) ).
    exp_json = VALUE #(
        ( `[` )
        ( `  {` )
        ( `    "elementName": "callbackClass was called"` )
        ( `  },` )
        ( `  {` )
        ( `    "elementName": "callbackClass was called"` )
        ( `  }` )
        ( `]` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type ).

  ENDMETHOD.

  METHOD structure_with_num_text.
    DATA test_type TYPE zcl_aff_test_types=>struc_with_num_text.
    test_type = VALUE #( numerical_text1 = '4' numerical_text2 = '1234' ).

    exp_json = VALUE #(
        ( `{` )
        ( `    "numericalText1": "0004",` )
        ( `    "numericalText2": "1234",` )
        ( `    "numericalText3": "0000"` )
        ( `}` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type ).
  ENDMETHOD.

  METHOD structure_with_include.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_include.
    test_type = VALUE #( first_element  = 'first'
                         second_element = VALUE #( my_first_element  = 'inner first'
                                                   my_second_element = 9 )
                         third_element  = 10 ).
    exp_json = VALUE #(
        ( `{` )
        ( `    "firstElement": "first",` )
        ( `    "secondElement": { ` )
        ( `      "myFirstElement": "inner first", ` )
        ( `      "mySecondElement": 9 ` )
        ( `    },` )
        ( `    "otherElement": 0` )
        ( `}` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type ).
  ENDMETHOD.

  METHOD struc_with_own_enum_values.
    DATA test_type TYPE zcl_aff_test_types=>struc_with_own_enum_values.
    test_type = VALUE #( enum_component = 'BB' ).
    exp_json = VALUE #(
        ( `{` )
        ( `    "enumComponent": "BBBB"` )
        ( `}` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type ).
    test_type = VALUE #( enum_component = 'AA' ).
    exp_json = VALUE #(
        ( `{` )
        ( `}` ) ).
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type ).
  ENDMETHOD.

  METHOD do_integration_test.
    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring) ).

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    IF manually_changed_json IS NOT INITIAL.
      json_xstring = cl_abap_codepage=>convert_to( manually_changed_json ).
    ENDIF.

    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type ).
  ENDMETHOD.

  METHOD from_abap_to_json.
    DATA(cut) = NEW zcl_aff_writer_xslt( 'root' ).
    DATA(test_generator) = NEW zcl_aff_generator( cut ).
    DATA(st_content) = test_generator->generate_type( test_type ).

    DATA(st_name) = CONV progname( c_xslt_prefix && st_execution_counter ).
    st_name = |{ st_name WIDTH = 30 PAD = '=' }XT|.

    INSERT REPORT st_name FROM st_content EXTENSION TYPE c_ext_xslt_source.

    DATA(json_writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    json_writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_linebreaks ).
    json_writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_indent value = '2' ).
    CALL TRANSFORMATION (st_name) SOURCE root = test_type RESULT XML json_writer.

    DATA(string) = cl_abap_codepage=>convert_from( json_writer->get_output( ) ).
    string = string && cl_abap_char_utilities=>newline.
    SPLIT string AT cl_abap_char_utilities=>newline INTO TABLE result.

    json = json_writer->get_output( ).

    st_execution_counter += 1.
  ENDMETHOD.


  METHOD from_json_to_abap.
    DATA(counter) = st_execution_counter - 1.
    DATA(st_name) = CONV progname( c_xslt_prefix && counter ).
    st_name = |{ st_name WIDTH = 30 PAD = '=' }XT|.


    DATA st_result TYPE abap_trans_resbind_tab.
    FIELD-SYMBOLS <st_result> LIKE LINE OF st_result.

    CLEAR result.
    APPEND INITIAL LINE TO st_result ASSIGNING <st_result>.
    <st_result>-name = 'ROOT'.
    GET REFERENCE OF result  INTO <st_result>-value.

    DATA(json_reader) = cl_sxml_string_reader=>create( json ).
    TRY.
        CALL TRANSFORMATION (st_name)
          SOURCE XML json_reader
          RESULT (st_result).
      CATCH cx_root INTO DATA(exception).
        DELETE REPORT st_name.
        cl_abap_unit_assert=>fail( exception->get_text( ) ).
    ENDTRY.
    DELETE REPORT st_name.
  ENDMETHOD.

  METHOD assert_json_equals.
    CONCATENATE LINES OF actual_json_stringtab INTO DATA(act_json_as_string).
    CONCATENATE LINES OF expected_json_stringtab INTO DATA(exp_json_as_string).
    CONDENSE act_json_as_string NO-GAPS.
    CONDENSE exp_json_as_string NO-GAPS.
    cl_abap_unit_assert=>assert_equals( msg = 'Expected json and actual json differ' exp = exp_json_as_string act = act_json_as_string ).
  ENDMETHOD.


ENDCLASS.

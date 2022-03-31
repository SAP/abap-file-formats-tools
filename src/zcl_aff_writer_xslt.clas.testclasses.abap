INTERFACE lif_test_types.
  TYPES:
    element      TYPE string,
    element_numc TYPE n LENGTH 2,
    element_i    TYPE int8,
    element_char TYPE c LENGTH 2.

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
    BEGIN OF structure_in_structure,
      structure TYPE structure,
      element   TYPE element,
    END OF structure_in_structure.

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

  TYPES:
    BEGIN OF ty_component,
      name        TYPE seocmpname,
      description TYPE seodescr,
    END OF ty_component,
    ty_components    TYPE SORTED TABLE OF ty_component WITH UNIQUE KEY name,
    ty_subcomponents TYPE SORTED TABLE OF ty_component WITH UNIQUE KEY name,
    BEGIN OF ty_method,
      name        TYPE seocmpname,
      description TYPE seodescr,
      parameters  TYPE ty_subcomponents,
      exceptions  TYPE ty_subcomponents,
    END OF ty_method,
    ty_methods TYPE SORTED TABLE OF ty_method WITH UNIQUE KEY name,
    BEGIN OF ty_event,
      name        TYPE seocmpname,
      description TYPE seodescr,
      parameters  TYPE ty_subcomponents,
    END OF ty_event,
    ty_events TYPE SORTED TABLE OF ty_event WITH UNIQUE KEY name,
    BEGIN OF ty_clif_properties,
      attributes TYPE ty_components,
      methods    TYPE ty_methods,
      events     TYPE ty_events,
      types      TYPE ty_components,
    END OF ty_clif_properties.

  TYPES: langu TYPE sy-langu.
  TYPES:
    BEGIN OF structure_with_language,
      language  TYPE sy-langu,
      language2 TYPE langu,
    END OF structure_with_language.

ENDINTERFACE.

"!@testing ZCL_AFF_GENERATOR
CLASS ltcl_type_writer_xslt DEFINITION FINAL FOR TESTING
  DURATION SHORT
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
      value_mappings FOR TESTING RAISING cx_static_check,
      name_mappings_structure FOR TESTING RAISING cx_static_check,
      name_mappings_table FOR TESTING RAISING cx_static_check,
      type_boolean FOR TESTING RAISING cx_static_check,
      type_numeric FOR TESTING RAISING cx_static_check,
      type_string FOR TESTING RAISING cx_static_check,
      open_unsupported_node FOR TESTING RAISING cx_static_check,
      close_unsupported_node FOR TESTING RAISING cx_static_check,
      numc_to_string FOR TESTING RAISING cx_static_check,
      int_to_bool FOR TESTING RAISING cx_static_check,
      char_to_string FOR TESTING RAISING cx_static_check,
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
    cut->formatting_option = zif_aff_writer=>formatting_option-no_formatting.
    test_generator = NEW zcl_aff_generator( cut ).
  ENDMETHOD.


  METHOD date_time_element.
    DATA(act_output) = test_generator->generate_type( VALUE d( ) ).
    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `    <str>` TO me->exp_transformation.
    APPEND `        <tt:value option="format(dateTimeOffset)"/>` TO me->exp_transformation.
    APPEND `    </str >` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD structure_with_incl.
    DATA test_type TYPE lif_test_types=>structure_with_include.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(INCLUDE_ELEMENT_1)" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="INCLUDE_ELEMENT_1">` TO me->exp_transformation.
    APPEND `          <tt:value ref="INCLUDE_ELEMENT_1"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(INCLUDE_ELEMENT_2)" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="INCLUDE_ELEMENT_2">` TO me->exp_transformation.
    APPEND `          <tt:value ref="INCLUDE_ELEMENT_2" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(ELEMENT_1)" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="ELEMENT_1">` TO me->exp_transformation.
    APPEND `          <tt:value ref="ELEMENT_1" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(ELEMENT_2)" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="ELEMENT_2">` TO me->exp_transformation.
    APPEND `          <tt:value ref="ELEMENT_2"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'ELEMENT_1;ELEMENT_2;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.

    validate_output( act_output ).
  ENDMETHOD.

  METHOD structure_include_in_include.
    DATA test_type TYPE lif_test_types=>structure_include_in_include.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(INCLUDE_ELEMENT_1)" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="INCLUDE_ELEMENT_1">` TO me->exp_transformation.
    APPEND `          <tt:value ref="INCLUDE_ELEMENT_1"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(INCLUDE_ELEMENT_2)" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="INCLUDE_ELEMENT_2">` TO me->exp_transformation.
    APPEND `          <tt:value ref="INCLUDE_ELEMENT_2" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(ELEMENT)" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="ELEMENT">` TO me->exp_transformation.
    APPEND `          <tt:value ref="ELEMENT"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'ELEMENT;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.

    validate_output( act_output ).
  ENDMETHOD.

  METHOD include_table.
    DATA test_type TYPE lif_test_types=>include_table.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(TABLE)" frq="?">` TO me->exp_transformation.
    APPEND `        <array name="TABLE">` TO me->exp_transformation.
    APPEND `          <tt:loop ref="TABLE">` TO me->exp_transformation.
    APPEND `            <tt:group>` TO me->exp_transformation.
    APPEND `              <tt:cond>` TO me->exp_transformation.
    APPEND `                <object>` TO me->exp_transformation.
    APPEND `                  <tt:group>` TO me->exp_transformation.
    APPEND `                    <tt:cond s-check="not-initial(ELEMENT_1)" frq="?">` TO me->exp_transformation.
    APPEND `                      <num name="ELEMENT_1">` TO me->exp_transformation.
    APPEND `                        <tt:value ref="ELEMENT_1" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `                      </num>` TO me->exp_transformation.
    APPEND `                    </tt:cond>` TO me->exp_transformation.
    APPEND `                    <tt:cond s-check="not-initial(ELEMENT_2)" frq="?">` TO me->exp_transformation.
    APPEND `                      <str name="ELEMENT_2">` TO me->exp_transformation.
    APPEND `                        <tt:value ref="ELEMENT_2"/>` TO me->exp_transformation.
    APPEND `                      </str>` TO me->exp_transformation.
    APPEND `                    </tt:cond>` TO me->exp_transformation.
    APPEND `                    <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `                       <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `                        <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `                          <tt:with-parameter name="MEMBERS" val="'ELEMENT_1;ELEMENT_2;'"/>` TO me->exp_transformation.
    APPEND `                        </tt:call-method>` TO me->exp_transformation.
    APPEND `                        <tt:skip/>` TO me->exp_transformation.
    APPEND `                      </_>` TO me->exp_transformation.
    APPEND `                    </tt:d-cond>` TO me->exp_transformation.
    APPEND `                    <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `                      <__/>` TO me->exp_transformation.
    APPEND `                    </tt:d-cond>` TO me->exp_transformation.
    APPEND `                  </tt:group>` TO me->exp_transformation.
    APPEND `                </object>` TO me->exp_transformation.
    APPEND `              </tt:cond>` TO me->exp_transformation.
    APPEND `            </tt:group>` TO me->exp_transformation.
    APPEND `          </tt:loop>` TO me->exp_transformation.
    APPEND `        </array>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(INCLUDE_ELEMENT_1)" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="INCLUDE_ELEMENT_1">` TO me->exp_transformation.
    APPEND `          <tt:value ref="INCLUDE_ELEMENT_1" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'INCLUDE_ELEMENT_1;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.

    validate_output( act_output ).
  ENDMETHOD.

  METHOD struc_tab_struc_tab.
    DATA test_type TYPE lif_test_types=>struc_tab_struc_tab.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(FIRST_TABLE)" frq="?">` TO me->exp_transformation.
    APPEND `        <array name="FIRST_TABLE">` TO me->exp_transformation.
    APPEND `          <tt:loop ref="FIRST_TABLE">` TO me->exp_transformation.
    APPEND `            <tt:group>` TO me->exp_transformation.
    APPEND `              <tt:cond>` TO me->exp_transformation.
    APPEND `                <object>` TO me->exp_transformation.
    APPEND `                  <tt:group>` TO me->exp_transformation.
    APPEND `                    <tt:cond s-check="not-initial(SECOND_TABLE)" frq="?">` TO me->exp_transformation.
    APPEND `                      <array name="SECOND_TABLE">` TO me->exp_transformation.
    APPEND `                        <tt:loop ref="SECOND_TABLE"> ` TO me->exp_transformation.
    APPEND `                          <tt:group>` TO me->exp_transformation.
    APPEND `                            <tt:cond>` TO me->exp_transformation.
    APPEND `                              <str>` TO me->exp_transformation.
    APPEND `                                <tt:value/>` TO me->exp_transformation.
    APPEND `                              </str>` TO me->exp_transformation.
    APPEND `                            </tt:cond>` TO me->exp_transformation.
    APPEND `                          </tt:group>` TO me->exp_transformation.
    APPEND `                        </tt:loop>` TO me->exp_transformation.
    APPEND `                      </array>` TO me->exp_transformation.
    APPEND `                    </tt:cond>` TO me->exp_transformation.
    APPEND `                    <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `                       <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `                        <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `                          <tt:with-parameter name="MEMBERS" val="'SECOND_TABLE;'"/>` TO me->exp_transformation.
    APPEND `                        </tt:call-method>` TO me->exp_transformation.
    APPEND `                        <tt:skip/>` TO me->exp_transformation.
    APPEND `                      </_>` TO me->exp_transformation.
    APPEND `                    </tt:d-cond>` TO me->exp_transformation.
    APPEND `                    <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `                      <__/>` TO me->exp_transformation.
    APPEND `                    </tt:d-cond>` TO me->exp_transformation.
    APPEND `                  </tt:group>` TO me->exp_transformation.
    APPEND `                </object>` TO me->exp_transformation.
    APPEND `              </tt:cond>` TO me->exp_transformation.
    APPEND `            </tt:group>` TO me->exp_transformation.
    APPEND `          </tt:loop>` TO me->exp_transformation.
    APPEND `        </array>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'FIRST_TABLE;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.

    validate_output( act_output ).
  ENDMETHOD.

  METHOD table_in_table.
    DATA test_type TYPE lif_test_types=>table_in_table.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `    <array>` TO me->exp_transformation.
    APPEND `        <tt:loop>` TO me->exp_transformation.
    APPEND `            <tt:group>` TO me->exp_transformation.
    APPEND `                <tt:cond>` TO me->exp_transformation.
    APPEND `                    <array>` TO me->exp_transformation.
    APPEND `                        <tt:loop>` TO me->exp_transformation.
    APPEND `                            <tt:group>` TO me->exp_transformation.
    APPEND `                                <tt:cond>` TO me->exp_transformation.
    APPEND `                                    <str>` TO me->exp_transformation.
    APPEND `                                        <tt:value/>` TO me->exp_transformation.
    APPEND `                                    </str>` TO me->exp_transformation.
    APPEND `                                </tt:cond>` TO me->exp_transformation.
    APPEND `                            </tt:group>` TO me->exp_transformation.
    APPEND `                        </tt:loop>` TO me->exp_transformation.
    APPEND `                    </array>` TO me->exp_transformation.
    APPEND `                </tt:cond>` TO me->exp_transformation.
    APPEND `            </tt:group>` TO me->exp_transformation.
    APPEND `        </tt:loop>` TO me->exp_transformation.
    APPEND `    </array>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.

    validate_output( act_output ).
  ENDMETHOD.

  METHOD table_structure.
    DATA test_type TYPE lif_test_types=>table_structure.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `    <array>` TO me->exp_transformation.
    APPEND `        <tt:loop>` TO me->exp_transformation.
    APPEND `            <tt:group>` TO me->exp_transformation.
    APPEND `                <tt:cond>` TO me->exp_transformation.
    APPEND `                    <object>` TO me->exp_transformation.
    APPEND `                        <tt:group>` TO me->exp_transformation.
    APPEND `                            <tt:cond s-check="not-initial(ELEMENT_1)" frq="?">` TO me->exp_transformation.
    APPEND `                                <num name="ELEMENT_1">` TO me->exp_transformation.
    APPEND `                                    <tt:value ref="ELEMENT_1" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `                                </num>` TO me->exp_transformation.
    APPEND `                            </tt:cond>` TO me->exp_transformation.
    APPEND `                            <tt:cond s-check="not-initial(ELEMENT_2)" frq="?">` TO me->exp_transformation.
    APPEND `                                <str name="ELEMENT_2">` TO me->exp_transformation.
    APPEND `                                    <tt:value ref="ELEMENT_2"/>` TO me->exp_transformation.
    APPEND `                                </str>` TO me->exp_transformation.
    APPEND `                            </tt:cond>` TO me->exp_transformation.
    APPEND `              <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `                 <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `                  <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `                    <tt:with-parameter name="MEMBERS" val="'ELEMENT_1;ELEMENT_2;'"/>` TO me->exp_transformation.
    APPEND `                  </tt:call-method>` TO me->exp_transformation.
    APPEND `                  <tt:skip/>` TO me->exp_transformation.
    APPEND `                </_>` TO me->exp_transformation.
    APPEND `              </tt:d-cond>` TO me->exp_transformation.
    APPEND `              <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `                <__/>` TO me->exp_transformation.
    APPEND `              </tt:d-cond>` TO me->exp_transformation.
    APPEND `            </tt:group>` TO me->exp_transformation.
    APPEND `          </object>` TO me->exp_transformation.
    APPEND `        </tt:cond>` TO me->exp_transformation.
    APPEND `            </tt:group>` TO me->exp_transformation.
    APPEND `        </tt:loop>` TO me->exp_transformation.
    APPEND `    </array>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.

    validate_output( act_output ).
  ENDMETHOD.

  METHOD type_timestamp.
    DATA(act_output) = test_generator->generate_type( VALUE timestamp( ) ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `    <str>` TO me->exp_transformation.
    APPEND `        <tt:value option="format(dateTimeOffset)"/>` TO me->exp_transformation.
    APPEND `    </str>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.

    validate_output( act_output ).
  ENDMETHOD.

  METHOD type_boolean.
    DATA(act_output) = test_generator->generate_type( VALUE abap_bool( ) ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `    <bool>` TO me->exp_transformation.
    APPEND `        <tt:value option="format(boolean)"/>` TO me->exp_transformation.
    APPEND `    </bool>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.

    validate_output( act_output ).
  ENDMETHOD.

  METHOD type_numeric.
    DATA(act_output) = test_generator->generate_type( VALUE float( ) ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `    <num>` TO me->exp_transformation.
    APPEND `        <tt:value option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `    </num>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.

    validate_output( act_output ).
  ENDMETHOD.

  METHOD type_string.
    DATA char_10 TYPE c LENGTH 10.
    DATA(act_output) = test_generator->generate_type( char_10 ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `    <str>` TO me->exp_transformation.
    APPEND `        <tt:value/>` TO me->exp_transformation.
    APPEND `    </str>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.

    validate_output( act_output ).
  ENDMETHOD.

  METHOD value_mappings.
    DATA test_type TYPE lif_test_types=>element.
    cut->zif_aff_writer~set_abap_value_mappings( abap_value_mappings = VALUE #( (
                                                 abap_element = 'ELEMENT'
                                                 value_mappings = VALUE #(
                                                     ( abap = 'X' json = 'true' )
                                                     ( abap = '' json = 'false' ) ) ) ) ).

    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `    <str>` TO me->exp_transformation.
    APPEND `        <tt:value map="` TO me->exp_transformation.
    APPEND `          val('X')=xml('true'),` TO me->exp_transformation.
    APPEND `          val('')=xml('false')` TO me->exp_transformation.
    APPEND `        "/>` TO me->exp_transformation.
    APPEND `    </str>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD numc_to_string.
    DATA test_type TYPE lif_test_types=>element_numc.
    cut->zif_aff_writer~set_abap_value_mappings( abap_value_mappings = VALUE #( (
                                                 abap_element   = 'ELEMENT_NUMC'
                                                 target_type    = zif_aff_writer=>type_info-string
                                                 value_mappings = VALUE #(
                                                     ( abap = '01' json = 'first' )
                                                     ( abap = '02' json = 'second' ) ) ) ) ).

    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `    <str>` TO me->exp_transformation.
    APPEND `        <tt:value map="` TO me->exp_transformation.
    APPEND `          val(N('01'))=xml('first'),` TO me->exp_transformation.
    APPEND `          val(N('02'))=xml('second')` TO me->exp_transformation.
    APPEND `        "/>` TO me->exp_transformation.
    APPEND `    </str>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD char_to_string.
    DATA test_type TYPE lif_test_types=>element_char.
    cut->zif_aff_writer~set_abap_value_mappings( abap_value_mappings = VALUE #( (
                                                 abap_element   = 'element_char'
                                                 target_type    = zif_aff_writer=>type_info-string
                                                 value_mappings = VALUE #(
                                                     ( abap = 'aA' json = 'oneTest' )
                                                     ( abap = 'Bb' json = 'twoTest' ) ) ) ) ).

    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `    <str>` TO me->exp_transformation.
    APPEND `        <tt:value map="` TO me->exp_transformation.
    APPEND `          val('aA')=xml('oneTest'),` TO me->exp_transformation.
    APPEND `          val('Bb')=xml('twoTest')` TO me->exp_transformation.
    APPEND `        "/>` TO me->exp_transformation.
    APPEND `    </str>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD int_to_bool.
    DATA test_type TYPE lif_test_types=>element_i.
    cut->zif_aff_writer~set_abap_value_mappings( abap_value_mappings = VALUE #( (
                                                 abap_element   = 'ELEMENT_I'
                                                 target_type    = zif_aff_writer=>type_info-boolean
                                                 value_mappings = VALUE #(
                                                     ( abap = '1' json = 'true' )
                                                     ( abap = '0' json = 'false' ) ) ) ) ).

    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `    <bool>` TO me->exp_transformation.
    APPEND `        <tt:value option="format(boolean)" map="` TO me->exp_transformation.
    APPEND `          val(I(1))=xml('true'),` TO me->exp_transformation.
    APPEND `          val(I(0))=xml('false')` TO me->exp_transformation.
    APPEND `        "/>` TO me->exp_transformation.
    APPEND `    </bool>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD name_mappings_structure.
    DATA test_type TYPE lif_test_types=>structure_in_structure.
    cut->zif_aff_writer~set_name_mappings( name_mappings = VALUE #(
                                           ( abap = 'ELEMENT_1' json = 'MAPPED_ELEMENT1' ) ( abap = 'STRUCTURE' json = 'MAPPED_STRUCTURE' ) ) ).

    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(STRUCTURE)" frq="?">` TO me->exp_transformation.
    APPEND `        <object name="MAPPED_STRUCTURE" tt:ref="STRUCTURE">` TO me->exp_transformation.
    APPEND `          <tt:group>` TO me->exp_transformation.
    APPEND `            <tt:cond s-check="not-initial(ELEMENT_1)" frq="?">` TO me->exp_transformation.
    APPEND `              <num name="MAPPED_ELEMENT1">` TO me->exp_transformation.
    APPEND `                <tt:value ref="ELEMENT_1" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `              </num>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `            <tt:cond s-check="not-initial(ELEMENT_2)" frq="?">` TO me->exp_transformation.
    APPEND `              <str name="ELEMENT_2">` TO me->exp_transformation.
    APPEND `                <tt:value ref="ELEMENT_2"/>` TO me->exp_transformation.
    APPEND `              </str>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `            <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `               <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `                <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `                  <tt:with-parameter name="MEMBERS" val="'MAPPED_ELEMENT1;ELEMENT_2;'"/>` TO me->exp_transformation.
    APPEND `                </tt:call-method>` TO me->exp_transformation.
    APPEND `                <tt:skip/>` TO me->exp_transformation.
    APPEND `              </_>` TO me->exp_transformation.
    APPEND `            </tt:d-cond>` TO me->exp_transformation.
    APPEND `            <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `              <__/>` TO me->exp_transformation.
    APPEND `            </tt:d-cond>` TO me->exp_transformation.
    APPEND `          </tt:group>` TO me->exp_transformation.
    APPEND `        </object>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(ELEMENT)" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="ELEMENT">` TO me->exp_transformation.
    APPEND `          <tt:value ref="ELEMENT"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'MAPPED_STRUCTURE;ELEMENT;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD name_mappings_table.
    DATA test_type TYPE lif_test_types=>structure_with_table.
    cut->zif_aff_writer~set_name_mappings( name_mappings = VALUE #(
                                           ( abap = 'ELEMENT_1' json = 'MAPPED_ELEMENT1' ) ( abap = 'TABLE' json = 'MAPPED_TABLE' ) ) ).

    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(TABLE)" frq="?">` TO me->exp_transformation.
    APPEND `        <array name="MAPPED_TABLE">` TO me->exp_transformation.
    APPEND `          <tt:loop ref="TABLE">` TO me->exp_transformation.
    APPEND `            <tt:group>` TO me->exp_transformation.
    APPEND `              <tt:cond>` TO me->exp_transformation.
    APPEND `                <object>` TO me->exp_transformation.
    APPEND `                  <tt:group>` TO me->exp_transformation.
    APPEND `                    <tt:cond s-check="not-initial(ELEMENT_1)" frq="?">` TO me->exp_transformation.
    APPEND `                      <num name="MAPPED_ELEMENT1">` TO me->exp_transformation.
    APPEND `                        <tt:value ref="ELEMENT_1" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `                      </num>` TO me->exp_transformation.
    APPEND `                    </tt:cond>` TO me->exp_transformation.
    APPEND `                    <tt:cond s-check="not-initial(ELEMENT_2)" frq="?">` TO me->exp_transformation.
    APPEND `                      <str name="ELEMENT_2">` TO me->exp_transformation.
    APPEND `                        <tt:value ref="ELEMENT_2"/>` TO me->exp_transformation.
    APPEND `                      </str>` TO me->exp_transformation.
    APPEND `                    </tt:cond>` TO me->exp_transformation.
    APPEND `                    <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `                       <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `                        <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `                          <tt:with-parameter name="MEMBERS" val="'MAPPED_ELEMENT1;ELEMENT_2;'"/>` TO me->exp_transformation.
    APPEND `                        </tt:call-method>` TO me->exp_transformation.
    APPEND `                        <tt:skip/>` TO me->exp_transformation.
    APPEND `                      </_>` TO me->exp_transformation.
    APPEND `                    </tt:d-cond>` TO me->exp_transformation.
    APPEND `                    <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `                      <__/>` TO me->exp_transformation.
    APPEND `                    </tt:d-cond>` TO me->exp_transformation.
    APPEND `                  </tt:group>` TO me->exp_transformation.
    APPEND `                </object>` TO me->exp_transformation.
    APPEND `              </tt:cond>` TO me->exp_transformation.
    APPEND `            </tt:group>` TO me->exp_transformation.
    APPEND `          </tt:loop>` TO me->exp_transformation.
    APPEND `        </array>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'MAPPED_TABLE;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD structure_with_language.
    DATA test_type TYPE lif_test_types=>structure_with_language.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(LANGUAGE)" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="LANGUAGE">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="cl_aff_xslt_callback_language" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="language" ref="LANGUAGE"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(LANGUAGE2)" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="LANGUAGE2">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="cl_aff_xslt_callback_language" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="language" ref="LANGUAGE2"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'LANGUAGE;LANGUAGE2;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.

    validate_output( act_output ).
  ENDMETHOD.

  METHOD validate_valid_xslt.
    DATA transformation TYPE string_table.
    DATA(log) = NEW zcl_aff_log( ).

    APPEND `<?sap.transform simple?>` TO me->exp_transformation.
    APPEND `<tt:transform xmlns:tt="http://www.sap.com/transformation-templates">` TO me->exp_transformation.
    APPEND `<tt:root name="root"/>` TO me->exp_transformation.
    APPEND `<tt:template>` TO me->exp_transformation.
    APPEND `<tt:ref name="root">` TO me->exp_transformation.
    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(LANGUAGE)">` TO me->exp_transformation.
    APPEND `        <str name="LANGUAGE">` TO me->exp_transformation.
    APPEND `          <tt:value ref="LANGUAGE" option="format(language)"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(LANGUAGE2)">` TO me->exp_transformation.
    APPEND `        <str name="LANGUAGE2">` TO me->exp_transformation.
    APPEND `          <tt:value ref="LANGUAGE2" option="format(language)"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    APPEND `</tt:ref>` TO me->exp_transformation.
    APPEND `</tt:template>` TO me->exp_transformation.
    APPEND `</tt:transform>` TO me->exp_transformation.

    DATA(is_valid) = cut->zif_aff_writer~validate( source = transformation log = log ).

    cl_abap_unit_assert=>assert_true( is_valid ).
    cl_abap_unit_assert=>assert_false( log->zif_aff_log~has_messages( ) ).
  ENDMETHOD.

  METHOD validate_invalid_xslt.
    DATA transformation TYPE string_table.
    DATA(log) = NEW zcl_aff_log( ).

    APPEND `<?sap.transform simple?>` TO me->exp_transformation.
    APPEND `<tt:transform xmlns:tt="http://www.sap.com/transformation-templates">` TO me->exp_transformation.
    APPEND `<tt:root name="root"/>` TO me->exp_transformation.
    APPEND `<tt:template>` TO me->exp_transformation.
    APPEND `<tt:ref name="root">` TO me->exp_transformation.
    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(LANGUAGE)">` TO me->exp_transformation.
    APPEND `        <str name="LANGUAGE">` TO me->exp_transformation.
    APPEND `          <tt:value ref="LANGUAGE" option="format(language)"` TO me->exp_transformation. "<-  missing closing tag /
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(LANGUAGE2)">` TO me->exp_transformation.
    APPEND `        <str name="LANGUAGE2">` TO me->exp_transformation.
    APPEND `          <tt:value ref="LANGUAGE2" option="format(language)"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    APPEND `</tt:ref>` TO me->exp_transformation.
    APPEND `</tt:template>` TO me->exp_transformation.
    APPEND `</tt:transform>` TO me->exp_transformation.

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
      CATCH zcx_aff_tools INTO DATA(exception) ##NO_HANDLER.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = '101' act = exception->if_t100_message~t100key-msgno ).
    cl_abap_unit_assert=>assert_equals( exp = 'ZAFF_TOOLS' act = exception->if_t100_message~t100key-msgid ).
  ENDMETHOD.

  METHOD close_unsupported_node.
    TRY.
        cut->zif_aff_writer~close_node(
          node_description = cl_abap_typedescr=>describe_by_data( VALUE i( ) )
          node_name        = 'Unssuprted Type' ).
        cl_abap_unit_assert=>fail( msg = 'Exception expected' ).
      CATCH zcx_aff_tools INTO DATA(exception) ##NO_HANDLER.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = '101' act = exception->if_t100_message~t100key-msgno ).
    cl_abap_unit_assert=>assert_equals( exp = 'ZAFF_TOOLS' act = exception->if_t100_message~t100key-msgid ).
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
  DURATION SHORT
  RISK LEVEL DANGEROUS.

  PRIVATE SECTION.
    CONSTANTS c_xslt_prefix TYPE string VALUE `ZAFF_TRANSFORMATION_GEN_` ##NO_TEXT.

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
      value_mappings FOR TESTING RAISING cx_static_check,
      name_mappings_structure FOR TESTING RAISING cx_static_check,
      name_mappings_table FOR TESTING RAISING cx_static_check,
      numc_to_string FOR TESTING RAISING cx_static_check,
      char_to_string FOR TESTING RAISING cx_static_check,
      int_to_bool FOR TESTING RAISING cx_static_check,
      structure_with_language FOR TESTING RAISING cx_static_check,
      from_json_to_abap
        IMPORTING
          json   TYPE xstring
        EXPORTING
          result TYPE data
        RAISING
          zcx_aff_tools
          cx_sxml_illegal_argument_error
          cx_aff_root,

      from_abap_to_json
        IMPORTING
          test_type      TYPE data
          name_mappings  TYPE zif_aff_writer=>ty_name_mappings OPTIONAL
          value_mappings TYPE zif_aff_writer=>ty_abap_value_mappings OPTIONAL
          formatting     TYPE zif_aff_writer=>enum_formatting_option DEFAULT zif_aff_writer=>formatting_option-no_formatting
        EXPORTING
          VALUE(result)  TYPE string_table
          VALUE(json)    TYPE xstring
        RAISING
          zcx_aff_tools
          cx_aff_root
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

    cut->zif_aff_writer~set_formatting_option( formatting  ).
    cut->zif_aff_writer~set_name_mappings( name_mappings ).
    cut->zif_aff_writer~set_abap_value_mappings( value_mappings ).

    DATA(test_generator) = NEW zcl_aff_generator( cut ).
    DATA(st_content) = test_generator->generate_type( test_type ).

    DATA(st_name) = CONV progname( c_xslt_prefix && st_execution_counter ).
    st_name = |{ st_name WIDTH = 30 PAD = '=' }XT|.

    INSERT REPORT st_name FROM st_content EXTENSION TYPE srext_ext_xslt_source.

    DATA(json_writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    json_writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_linebreaks ).
    json_writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_indent value = '2' ).
    CALL TRANSFORMATION (st_name) SOURCE root = test_type RESULT XML json_writer.


    cl_aff_content_handler_factory=>get_handler_for_plain_text( )->deserialize(
      EXPORTING content = json_writer->get_output( )
      IMPORTING data = result  ).

    json = json_writer->get_output( ).

    st_execution_counter += 1.
  ENDMETHOD.

  METHOD from_json_to_abap.

    DATA(counter) = st_execution_counter - 1.
    DATA(st_name) = CONV progname( c_xslt_prefix && counter ).
    st_name = |{ st_name WIDTH = 30 PAD = '=' }XT|.

    DATA(handler) = NEW cl_aff_content_handler_json(
      simple_transformation = st_name
      st_root_name          = 'ROOT'
    ).

    TRY.
        handler->if_aff_content_handler~deserialize(
          EXPORTING
            content = json
          IMPORTING
            data    = result
        ).
      CATCH zcx_aff_tools INTO DATA(exception).
        DELETE REPORT st_name.
        cl_abap_unit_assert=>fail( ).
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
        json      = DATA(json_xstring)
    ).

    APPEND `[` TO exp_json.
    APPEND ` {` TO exp_json.
    APPEND `  "ELEMENT_1":1,` TO exp_json.
    APPEND `  "ELEMENT_2":"first_element"` TO exp_json.
    APPEND ` },` TO exp_json.
    APPEND ` {` TO exp_json.
    APPEND `  "ELEMENT_1":2,` TO exp_json.
    APPEND `  "ELEMENT_2":"second_element"` TO exp_json.
    APPEND ` }` TO exp_json.
    APPEND `]` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).

  ENDMETHOD.

  METHOD include_table.
    DATA test_type TYPE lif_test_types=>include_table.
    test_type = VALUE #(
        table = VALUE #(
            ( element_1 = 1 element_2 = 'obj1_element_2_value' )
            ( element_1 = 2 element_2 = 'obj2_element_2_value' )
        )
        include_element_1 = 1
        ).

    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring)
    ).

    APPEND `{` TO exp_json.
    APPEND ` "TABLE":` TO exp_json.
    APPEND ` [` TO exp_json.
    APPEND `  {` TO exp_json.
    APPEND `   "ELEMENT_1":1,` TO exp_json.
    APPEND `   "ELEMENT_2":"obj1_element_2_value"` TO exp_json.
    APPEND `  },` TO exp_json.
    APPEND `  {` TO exp_json.
    APPEND `   "ELEMENT_1":2,` TO exp_json.
    APPEND `   "ELEMENT_2":"obj2_element_2_value"` TO exp_json.
    APPEND `  }` TO exp_json.
    APPEND ` ],` TO exp_json.
    APPEND ` "INCLUDE_ELEMENT_1":1` TO exp_json.
    APPEND `}` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
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
        json      = DATA(json_xstring)
    ).

    APPEND `{` TO exp_json.
    APPEND ` "INCLUDE_ELEMENT_1":"element1_value",` TO exp_json.
    APPEND ` "INCLUDE_ELEMENT_2":2,` TO exp_json.
    APPEND ` "ELEMENT":"element_value"` TO exp_json.
    APPEND `}` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
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
        json      = DATA(json_xstring)
    ).

    APPEND `{` TO exp_json.
    APPEND ` "INCLUDE_ELEMENT_1":"value of incl element1",` TO exp_json.
    APPEND ` "INCLUDE_ELEMENT_2":1,` TO exp_json.
    APPEND ` "ELEMENT_1":2,` TO exp_json.
    APPEND ` "ELEMENT_2":"value of element2"` TO exp_json.
    APPEND `}` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
  ENDMETHOD.

  METHOD struc_tab_struc_tab.
    DATA(second_table_1)  = VALUE lif_test_types=>table_build_in_type( ( `table_1_line_1` ) ( `table_1_line_2` ) ).
    DATA(second_table_2)  = VALUE lif_test_types=>table_build_in_type( ( `table_2_line_1` ) ( `table_2_line_2` ) ).
    DATA(first_table) = VALUE lif_test_types=>first_table_type(
      ( second_table = second_table_1 )
      ( second_table = second_table_2 )
    ).
    DATA(test_type) = VALUE lif_test_types=>struc_tab_struc_tab(
      first_table = first_table
    ).

    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring)
    ).

    APPEND `{` TO exp_json.
    APPEND ` "FIRST_TABLE":` TO exp_json.
    APPEND ` [` TO exp_json.
    APPEND `  {` TO exp_json.
    APPEND `   "SECOND_TABLE":` TO exp_json.
    APPEND `   [` TO exp_json.
    APPEND `    "table_1_line_1",` TO exp_json.
    APPEND `    "table_1_line_2"` TO exp_json.
    APPEND `   ]` TO exp_json.
    APPEND `  },` TO exp_json.
    APPEND `  {` TO exp_json.
    APPEND `   "SECOND_TABLE":` TO exp_json.
    APPEND `   [` TO exp_json.
    APPEND `    "table_2_line_1",` TO exp_json.
    APPEND `    "table_2_line_2"` TO exp_json.
    APPEND `   ]` TO exp_json.
    APPEND `  }` TO exp_json.
    APPEND ` ]` TO exp_json.
    APPEND `}` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
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
        json      = DATA(json_xstring)
    ).

    APPEND `[` TO exp_json.
    APPEND ` [` TO exp_json.
    APPEND `  "table_1_line_1",` TO exp_json.
    APPEND `  "table_1_line_2"` TO exp_json.
    APPEND ` ],` TO exp_json.
    APPEND ` [` TO exp_json.
    APPEND `  "table_2_line_1",` TO exp_json.
    APPEND `  "table_2_line_2"` TO exp_json.
    APPEND ` ]` TO exp_json.
    APPEND `]` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
  ENDMETHOD.

  METHOD type_timestamp.
    DATA test_type TYPE timestamp.
    test_type = 20200424163000.

    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring)
    ).

    APPEND `"2020-04-24T16:30:00+00:00"` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
  ENDMETHOD.

  METHOD type_boolean.
    DATA test_type TYPE abap_bool.
    test_type = 'X'.
    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring)
    ).

    APPEND `true` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
  ENDMETHOD.

  METHOD type_numeric.
    DATA test_type TYPE decfloat16.
    test_type = '5.3' ##LITERAL.
    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring)
    ).

    APPEND `5.3` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
  ENDMETHOD.

  METHOD type_string.
    DATA test_type TYPE c LENGTH 10.
    test_type = '0123abcdef'.
    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring)
    ).

    APPEND `"0123abcdef"` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
  ENDMETHOD.

  METHOD value_mappings.
    DATA test_type TYPE lif_test_types=>element.
    test_type = 'X'.


    from_abap_to_json(
      EXPORTING
        test_type      = test_type
        value_mappings = VALUE #(
                            ( abap_element = 'ELEMENT'
                                value_mappings = VALUE #(
                                    ( abap = 'X' json = 'true' )
                                    ( abap = '' json = 'false' ) ) ) )
      IMPORTING
        result         = DATA(act_json)
        json           = DATA(json_xstring)
    ).

    APPEND `"true"` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
  ENDMETHOD.

  METHOD name_mappings_structure.
    DATA test_type TYPE lif_test_types=>structure.
    test_type-element_1 = 1.
    test_type-element_2 = 'element2_value'.

    from_abap_to_json(
      EXPORTING
        test_type     = test_type
        name_mappings = VALUE #(
                                ( abap = 'ELEMENT_1' json = 'MAPPED_ELEMENT1' )
                                ( abap = 'STRUCTURE' json = 'MAPPED_STRUCTURE' ) )
      IMPORTING
        result        = DATA(act_json)
        json          = DATA(json_xstring)
    ).

    APPEND `{` TO exp_json.
    APPEND ` "MAPPED_ELEMENT1":1,` TO exp_json.
    APPEND ` "ELEMENT_2":"element2_value"` TO exp_json.
    APPEND `}` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
  ENDMETHOD.

  METHOD name_mappings_table.
    DATA test_type TYPE lif_test_types=>structure_with_table.

    test_type-table = VALUE #(
        ( element_1 = 1 element_2 = 'obj1_element2' )
        ( element_1 = 2 element_2 = 'obj2_element2' ) ).

    from_abap_to_json(
      EXPORTING
        test_type     = test_type
        name_mappings = VALUE #(
                                ( abap = 'ELEMENT_1' json = 'MAPPED_ELEMENT1' )
                                ( abap = 'TABLE' json = 'MAPPED_TABLE' ) )
      IMPORTING
        result        = DATA(act_json)
        json          = DATA(json_xstring)
    ).


    APPEND `{` TO exp_json.
    APPEND ` "MAPPED_TABLE":` TO exp_json.
    APPEND ` [` TO exp_json.
    APPEND `  {` TO exp_json.
    APPEND `   "MAPPED_ELEMENT1":1,` TO exp_json.
    APPEND `   "ELEMENT_2":"obj1_element2"` TO exp_json.
    APPEND `  },` TO exp_json.
    APPEND `  {` TO exp_json.
    APPEND `   "MAPPED_ELEMENT1":2,` TO exp_json.
    APPEND `   "ELEMENT_2":"obj2_element2"` TO exp_json.
    APPEND `  }` TO exp_json.
    APPEND ` ]` TO exp_json.
    APPEND `}` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
  ENDMETHOD.

  METHOD numc_to_string.
    DATA test_type TYPE lif_test_types=>element_numc.
    test_type = '01'.


    from_abap_to_json(
      EXPORTING
        test_type      = test_type
        value_mappings = VALUE #(
                            ( abap_element = 'ELEMENT_NUMC'
                              target_type = zif_aff_writer=>type_info-string
                              value_mappings = VALUE #(
                                    ( abap = '01' json = 'first' )
                                    ( abap = '02' json = 'second' ) ) ) )
      IMPORTING
        result         = DATA(act_json)
        json           = DATA(json_xstring)
    ).

    APPEND `"first"` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
  ENDMETHOD.

  METHOD char_to_string.
    DATA test_type TYPE lif_test_types=>element_char.
    test_type = 'Bb'.

    from_abap_to_json(
      EXPORTING
        test_type      = test_type
        value_mappings = VALUE #(
                            ( abap_element = 'eLemEnt_CHAR'
        target_type = zif_aff_writer=>type_info-string
        value_mappings = VALUE #(
                ( abap = 'aA' json = 'oneTest' )
                ( abap = 'Bb' json = 'twoTest' ) ) ) )
      IMPORTING
        result         = DATA(act_json)
        json           = DATA(json_xstring)
    ).

    APPEND `"twoTest"` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
  ENDMETHOD.

  METHOD int_to_bool.
    DATA test_type TYPE lif_test_types=>element_i.
    test_type = '1'.

    from_abap_to_json(
      EXPORTING
        test_type      = test_type
        value_mappings = VALUE #(
                            ( abap_element = 'ELEMENT_I'
        target_type = zif_aff_writer=>type_info-boolean
        value_mappings = VALUE #(
                ( abap = '1' json = 'true' )
                ( abap = '0' json = 'false' ) ) ) )
      IMPORTING
        result         = DATA(act_json)
        json           = DATA(json_xstring)
    ).

    APPEND `true` TO exp_json.

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
  ENDMETHOD.

  METHOD structure_with_language.
    DATA test_type TYPE lif_test_types=>structure_with_language.
    test_type = VALUE #( language = 'E' language2 = 'D').

    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring)
    ).

    APPEND `{` TO exp_json.
    APPEND `"LANGUAGE":"en",` TO exp_json.
    APPEND `"LANGUAGE2":"de"` TO exp_json.
    APPEND `}` TO exp_json.
    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    DATA act_data LIKE test_type.
    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
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
  DURATION SHORT
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
      table_with_call_of_struc FOR TESTING RAISING cx_static_check,
      table_with_call_of_table FOR TESTING RAISING cx_static_check,
      struc_of_table_with_callback FOR TESTING RAISING cx_static_check,
      structure_with_wrong_default FOR TESTING RAISING cx_static_check,
      structure_with_wrong_callback FOR TESTING RAISING cx_static_check,
      type_of_enumtype_and_co_differ FOR TESTING RAISING cx_static_check,
      wrong_default_type_link FOR TESTING RAISING cx_static_check,
      structure_with_enums FOR TESTING RAISING cx_static_check,
      structure_with_default_problem FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_type_writer_xslt_ad IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_aff_writer_xslt( me->st_root_name ).
    test_generator = NEW zcl_aff_generator( cut ).
  ENDMETHOD.

  METHOD simple_integer.
    DATA test_type TYPE zcl_aff_test_types=>integer.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `    <num>` TO me->exp_transformation.
    APPEND `        <tt:value option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `    </num>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD simple_string.
    DATA test_type TYPE zcl_aff_test_types=>mystring.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `    <str>` TO me->exp_transformation.
    APPEND `        <tt:value/>` TO me->exp_transformation.
    APPEND `    </str>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD simple_data.
    DATA test_type TYPE zcl_aff_test_types=>my_date.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `    <str>` TO me->exp_transformation.
    APPEND `    <tt:value option="format(dateTimeOffset)"/>` TO me->exp_transformation.
    APPEND `    </str>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD simple_structure.
    DATA test_type TYPE zcl_aff_test_types=>my_structure.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(MY_FIRST_ELEMENT)" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="myFirstElement">` TO me->exp_transformation.
    APPEND `          <tt:value ref="MY_FIRST_ELEMENT"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(MY_SECOND_ELEMENT)" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="mySecondElement">` TO me->exp_transformation.
    APPEND `          <tt:value ref="MY_SECOND_ELEMENT" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'myFirstElement;mySecondElement;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD simple_structure_required.
    DATA test_type TYPE zcl_aff_test_types=>my_structure2.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `        <str name="myFirstElement">` TO me->exp_transformation.
    APPEND `          <tt:value ref="MY_FIRST_ELEMENT"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(MY_SECOND_ELEMENT)" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="mySecondElement">` TO me->exp_transformation.
    APPEND `          <tt:value ref="MY_SECOND_ELEMENT" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'myFirstElement;mySecondElement;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act = act_output no_log_check = abap_true ).
    log = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log                = log
                                                             exp_message        = VALUE #( msgid = 'ZAFF_TOOLS'
                                                                                           msgno = 108
                                                                                           attr1 = `$ructure` )
                                                             exp_component_name = `MY_STRUCTURE2`
                                                             exp_type           = zif_aff_log=>c_message_type-warning ).
  ENDMETHOD.

  METHOD structure_in_structure.
    DATA test_type TYPE zcl_aff_test_types=>my_structure3.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(NESTED_STRUC)" frq="?">` TO me->exp_transformation.
    APPEND `        <object name="nestedStruc" tt:ref="NESTED_STRUC">` TO me->exp_transformation.
    APPEND `          <tt:group>` TO me->exp_transformation.
    APPEND `            <tt:cond s-check="not-initial(MY_ELEMENT)" frq="?">` TO me->exp_transformation.
    APPEND `              <str name="myElement">` TO me->exp_transformation.
    APPEND `                <tt:value ref="MY_ELEMENT"/>` TO me->exp_transformation.
    APPEND `              </str>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `            <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `               <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `                <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `                  <tt:with-parameter name="MEMBERS" val="'myElement;'"/>` TO me->exp_transformation.
    APPEND `                </tt:call-method>` TO me->exp_transformation.
    APPEND `                <tt:skip/>` TO me->exp_transformation.
    APPEND `              </_>` TO me->exp_transformation.
    APPEND `            </tt:d-cond>` TO me->exp_transformation.
    APPEND `            <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `              <__/>` TO me->exp_transformation.
    APPEND `            </tt:d-cond>` TO me->exp_transformation.
    APPEND `          </tt:group>` TO me->exp_transformation.
    APPEND `        </object>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `        <str name="myElement">` TO me->exp_transformation.
    APPEND `          <tt:value ref="MY_ELEMENT"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'nestedStruc;myElement;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.

    validate_output( act_output ).
  ENDMETHOD.

  METHOD simple_table.
    DATA test_type TYPE zcl_aff_test_types=>my_standard_table.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `    <array>` TO me->exp_transformation.
    APPEND `        <tt:loop>` TO me->exp_transformation.
    APPEND `            <tt:group>` TO me->exp_transformation.
    APPEND `                <tt:cond>` TO me->exp_transformation.
    APPEND `                    <str>` TO me->exp_transformation.
    APPEND `                        <tt:value/>` TO me->exp_transformation.
    APPEND `                    </str>` TO me->exp_transformation.
    APPEND `                </tt:cond>` TO me->exp_transformation.
    APPEND `            </tt:group>` TO me->exp_transformation.
    APPEND `        </tt:loop>` TO me->exp_transformation.
    APPEND `    </array>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD simple_type_with_enum_values.
    DATA test_type TYPE zcl_aff_test_types=>category.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `      <str>` TO me->exp_transformation.
    APPEND `        <tt:value map="` TO me->exp_transformation.
    APPEND `          val(N('00'))=xml('general'),` TO me->exp_transformation.
    APPEND `          val(N('01'))=xml('classicBadi')` TO me->exp_transformation.
    APPEND `        "/>` TO me->exp_transformation.
    APPEND `      </str>` TO me->exp_transformation.
    APPEND `    </tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD structure_with_enums.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_different_enum.
    test_generator->generate_type( test_type ).
    DATA(log) = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log                = log
                                                             exp_message        = VALUE #( msgid = 'ZAFF_TOOLS'
                                                                                           msgno = 127 )
                                                             exp_component_name = `STRUCTURE_WITH_DIFFERENT_ENUM-ENUM_WITHOUT_ALL`
                                                             exp_type           = zif_aff_log=>c_message_type-warning ).


  ENDMETHOD.

  METHOD enum_values_with_wrong_link.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_wrong_link.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(ELEMENT_ONE)" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="elementOne">` TO me->exp_transformation.
    APPEND `          <tt:value ref="ELEMENT_ONE"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(ELEMENT_TWO)" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="elementTwo">` TO me->exp_transformation.
    APPEND `          <tt:value ref="ELEMENT_TWO"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'elementOne;elementTwo;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act = act_output no_log_check = abap_true ).
    log = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log                = log
                                                             exp_message        = VALUE #( msgid = 'ZAFF_TOOLS'
                                                                                           msgno = 104
                                                                                           attr1 = `ZCL_AFF_TEST_TYPES=>ENUM_VALUES_WRONG` )
                                                             exp_component_name = `STRUCTURE_WITH_WRONG_LINK-ELEMENT_TWO`
                                                             exp_type           = zif_aff_log=>c_message_type-warning ).
  ENDMETHOD.

  METHOD structure_with_enum_values.
    DATA test_type TYPE zcl_aff_test_types=>ty_class_properties.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(HEADER)" frq="?">` TO me->exp_transformation.
    APPEND `        <object name="header" tt:ref="HEADER">` TO me->exp_transformation.
    APPEND `          <tt:group>` TO me->exp_transformation.
    APPEND `            <tt:cond s-check="not-initial(DESCRIPTION)" frq="?">` TO me->exp_transformation.
    APPEND `              <str name="description">` TO me->exp_transformation.
    APPEND `                <tt:value ref="DESCRIPTION"/>` TO me->exp_transformation.
    APPEND `              </str>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `            <tt:cond s-check="not-initial(ORIGINAL_LANGUAGE)" frq="?">` TO me->exp_transformation.
    APPEND `              <str name="originalLanguage">` TO me->exp_transformation.
    APPEND `                <tt:call-method class="cl_aff_xslt_callback_language" d-name="deserialize" reader="reader" s-name="serialize" writer="writer"> ` TO me->exp_transformation.
    APPEND `                  <tt:with-parameter name="language" ref="ORIGINAL_LANGUAGE"/>` TO me->exp_transformation.
    APPEND `                </tt:call-method>` TO me->exp_transformation.
    APPEND `              </str>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `            <tt:cond s-check="not-initial(ABAP_LANGUAGE_VERSION)" frq="?">` TO me->exp_transformation.
    APPEND `              <str name="abapLanguageVersion">` TO me->exp_transformation.
    APPEND `                <tt:value ref="ABAP_LANGUAGE_VERSION" map="` TO me->exp_transformation.
    APPEND `                  val('')=xml('standard'),` TO me->exp_transformation.
    APPEND `                  val('5')=xml('cloudDevelopment')` TO me->exp_transformation.
    APPEND `                "/>` TO me->exp_transformation.
    APPEND `              </str>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `            <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `               <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `                <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `                  <tt:with-parameter name="MEMBERS" val="'description;originalLanguage;abapLanguageVersion;'"/>` TO me->exp_transformation.
    APPEND `                </tt:call-method>` TO me->exp_transformation.
    APPEND `                <tt:skip/>` TO me->exp_transformation.
    APPEND `              </_>` TO me->exp_transformation.
    APPEND `            </tt:d-cond>` TO me->exp_transformation.
    APPEND `            <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `              <__/>` TO me->exp_transformation.
    APPEND `            </tt:d-cond>` TO me->exp_transformation.
    APPEND `          </tt:group>` TO me->exp_transformation.
    APPEND `        </object>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(CLASS_CATEGORY)" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="classCategory">` TO me->exp_transformation.
    APPEND `          <tt:value ref="CLASS_CATEGORY" map="` TO me->exp_transformation.
    APPEND `            val(N('00'))=xml('general'),` TO me->exp_transformation.
    APPEND `            val(N('01'))=xml('exitClass')` TO me->exp_transformation.
    APPEND `          "/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'header;classCategory;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act = act_output ).
  ENDMETHOD.

  METHOD deep_nested_structure.
    DATA test_type TYPE zcl_aff_test_types=>list.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `        <num name="field1">` TO me->exp_transformation.
    APPEND `          <tt:value ref="FIELD1" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `        <str name="field2">` TO me->exp_transformation.
    APPEND `          <tt:value ref="FIELD2"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `        <object name="list1" tt:ref="LIST1">` TO me->exp_transformation.
    APPEND `          <tt:group>` TO me->exp_transformation.
    APPEND `            <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `              <num name="elementOfList1">` TO me->exp_transformation.
    APPEND `                <tt:value ref="ELEMENT_OF_LIST1" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `              </num>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `            <tt:cond s-check="not-initial(LIST2)" frq="?">` TO me->exp_transformation.
    APPEND `              <object name="list2" tt:ref="LIST2">` TO me->exp_transformation.
    APPEND `                <tt:group>` TO me->exp_transformation.
    APPEND `                  <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `                    <str name="elementOfList2">` TO me->exp_transformation.
    APPEND `                      <tt:value ref="ELEMENT_OF_LIST2"/>` TO me->exp_transformation.
    APPEND `                    </str>` TO me->exp_transformation.
    APPEND `                  </tt:cond>` TO me->exp_transformation.
    APPEND `                  <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `                     <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `                      <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `                        <tt:with-parameter name="MEMBERS" val="'elementOfList2;'"/>` TO me->exp_transformation.
    APPEND `                      </tt:call-method>` TO me->exp_transformation.
    APPEND `                      <tt:skip/>` TO me->exp_transformation.
    APPEND `                    </_>` TO me->exp_transformation.
    APPEND `                  </tt:d-cond>` TO me->exp_transformation.
    APPEND `                  <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `                    <__/>` TO me->exp_transformation.
    APPEND `                  </tt:d-cond>` TO me->exp_transformation.
    APPEND `                </tt:group>` TO me->exp_transformation.
    APPEND `              </object>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `            <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `               <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `                <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `                  <tt:with-parameter name="MEMBERS" val="'elementOfList1;list2;'"/>` TO me->exp_transformation.
    APPEND `                </tt:call-method>` TO me->exp_transformation.
    APPEND `                <tt:skip/>` TO me->exp_transformation.
    APPEND `              </_>` TO me->exp_transformation.
    APPEND `            </tt:d-cond>` TO me->exp_transformation.
    APPEND `            <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `              <__/>` TO me->exp_transformation.
    APPEND `            </tt:d-cond>` TO me->exp_transformation.
    APPEND `          </tt:group>` TO me->exp_transformation.
    APPEND `        </object>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `        <str name="field3">` TO me->exp_transformation.
    APPEND `          <tt:value ref="FIELD3"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'field1;field2;list1;field3;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD nested_structure_with_table.
    DATA test_type TYPE zcl_aff_test_types=>outer_struc.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(FIELD1)" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="field1">` TO me->exp_transformation.
    APPEND `          <tt:value ref="FIELD1" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(INNER_STRUC)" frq="?">` TO me->exp_transformation.
    APPEND `        <object name="innerStruc" tt:ref="INNER_STRUC">` TO me->exp_transformation.
    APPEND `          <tt:group>` TO me->exp_transformation.
    APPEND `            <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `              <num name="elementOfInnerStruc">` TO me->exp_transformation.
    APPEND `                <tt:value ref="ELEMENT_OF_INNER_STRUC" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `              </num>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `            <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `              <array name="innerTableVar">` TO me->exp_transformation.
    APPEND `                <tt:loop ref="INNER_TABLE_VAR">` TO me->exp_transformation.
    APPEND `                  <tt:group>` TO me->exp_transformation.
    APPEND `                    <tt:cond>` TO me->exp_transformation.
    APPEND `                      <str>` TO me->exp_transformation.
    APPEND `                        <tt:value/>` TO me->exp_transformation.
    APPEND `                      </str>` TO me->exp_transformation.
    APPEND `                    </tt:cond>` TO me->exp_transformation.
    APPEND `                  </tt:group>` TO me->exp_transformation.
    APPEND `                </tt:loop>` TO me->exp_transformation.
    APPEND `              </array>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `            <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `               <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `                <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `                  <tt:with-parameter name="MEMBERS" val="'elementOfInnerStruc;innerTableVar;'"/>` TO me->exp_transformation.
    APPEND `                </tt:call-method>` TO me->exp_transformation.
    APPEND `                <tt:skip/>` TO me->exp_transformation.
    APPEND `              </_>` TO me->exp_transformation.
    APPEND `            </tt:d-cond>` TO me->exp_transformation.
    APPEND `            <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `              <__/>` TO me->exp_transformation.
    APPEND `            </tt:d-cond>` TO me->exp_transformation.
    APPEND `          </tt:group>` TO me->exp_transformation.
    APPEND `        </object>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `        <str name="field2">` TO me->exp_transformation.
    APPEND `          <tt:value ref="FIELD2"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'field1;innerStruc;field2;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD struc_with_table_not_req.
    DATA test_type TYPE zcl_aff_test_types=>aff_test_type.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(FIELD1)" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="field1">` TO me->exp_transformation.
    APPEND `          <tt:value ref="FIELD1" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `        <object name="innerStruc" tt:ref="INNER_STRUC">` TO me->exp_transformation.
    APPEND `          <tt:group>` TO me->exp_transformation.
    APPEND `            <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `              <num name="innerElement">` TO me->exp_transformation.
    APPEND `                <tt:value ref="INNER_ELEMENT" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `              </num>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `            <tt:cond s-check="not-initial(INNER_TABLE)" frq="?">` TO me->exp_transformation.
    APPEND `              <array name="innerTable">` TO me->exp_transformation.
    APPEND `                <tt:loop ref="INNER_TABLE">` TO me->exp_transformation.
    APPEND `                  <tt:group>` TO me->exp_transformation.
    APPEND `                    <tt:cond>` TO me->exp_transformation.
    APPEND `                      <str>` TO me->exp_transformation.
    APPEND `                        <tt:value/>` TO me->exp_transformation.
    APPEND `                      </str>` TO me->exp_transformation.
    APPEND `                    </tt:cond>` TO me->exp_transformation.
    APPEND `                  </tt:group>` TO me->exp_transformation.
    APPEND `                </tt:loop>` TO me->exp_transformation.
    APPEND `              </array>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `            <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `               <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `                <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `                  <tt:with-parameter name="MEMBERS" val="'innerElement;innerTable;'"/>` TO me->exp_transformation.
    APPEND `                </tt:call-method>` TO me->exp_transformation.
    APPEND `                <tt:skip/>` TO me->exp_transformation.
    APPEND `              </_>` TO me->exp_transformation.
    APPEND `            </tt:d-cond>` TO me->exp_transformation.
    APPEND `            <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `              <__/>` TO me->exp_transformation.
    APPEND `            </tt:d-cond>` TO me->exp_transformation.
    APPEND `          </tt:group>` TO me->exp_transformation.
    APPEND `        </object>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `        <str name="field2">` TO me->exp_transformation.
    APPEND `          <tt:value ref="FIELD2"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(FIELD_WITH_VALUES)" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="fieldWithValues">` TO me->exp_transformation.
    APPEND `          <tt:value ref="FIELD_WITH_VALUES" map="` TO me->exp_transformation.
    APPEND `            val(N('00'))=xml('general'),` TO me->exp_transformation.
    APPEND `            val(N('01'))=xml('exitClass')` TO me->exp_transformation.
    APPEND `          "/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'field1;innerStruc;field2;fieldWithValues;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.


  METHOD structure_different_default.
    DATA test_type TYPE zcl_aff_test_types=>structure_different_default.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="FOUR_BYTE_INT" val="I(5)"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="EIGHT_BYTE_INT" val="INT8(55)"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="BIN_FLOAT" val="F('4.3')"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="BYTE_LIKE" val="X('FFFF')"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="BYTE_LIKE2" val="X('FF00FF')"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="DECIMAL_FLOAT_16" val="DECFLOAT16('25.26')"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="DECIMAL_FLOAT_34" val="DECFLOAT34('123.05')"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="PACKED_NUMBER" val="P(123.45)"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="NUMERIC_TEXT" val="N('1067')"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="CHARACTER_TEXT" val="C('abcde')"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="STRING_TEXT" val="C('Default text')"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="DATE_FIELD" val="D('19720401')"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="TIME_FIELD" val="T('201500')"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="BOOL_TRUE" val="C('X')"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="BOOL_FALSE" val="C('')"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="ENUM_TYPE" val="N('01')"/>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="FOUR_BYTE_INT!=I(5)" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="fourByteInt">` TO me->exp_transformation.
    APPEND `          <tt:value ref="FOUR_BYTE_INT" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="EIGHT_BYTE_INT!=INT8(55)" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="eightByteInt">` TO me->exp_transformation.
    APPEND `          <tt:value ref="EIGHT_BYTE_INT" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="BIN_FLOAT!=F('4.3')" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="binFloat">` TO me->exp_transformation.
    APPEND `          <tt:value ref="BIN_FLOAT" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="BYTE_LIKE!=X('FFFF')" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="byteLike">` TO me->exp_transformation.
    APPEND `          <tt:value ref="BYTE_LIKE"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="BYTE_LIKE2!=X('FF00FF')" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="byteLike2">` TO me->exp_transformation.
    APPEND `          <tt:value ref="BYTE_LIKE2"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="DECIMAL_FLOAT_16!=DECFLOAT16('25.26')" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="decimalFloat16">` TO me->exp_transformation.
    APPEND `          <tt:value ref="DECIMAL_FLOAT_16" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="DECIMAL_FLOAT_34!=DECFLOAT34('123.05')" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="decimalFloat34">` TO me->exp_transformation.
    APPEND `          <tt:value ref="DECIMAL_FLOAT_34" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="PACKED_NUMBER!=P(123.45)" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="packedNumber">` TO me->exp_transformation.
    APPEND `          <tt:value ref="PACKED_NUMBER" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="NUMERIC_TEXT!=N('1067')" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="numericText">` TO me->exp_transformation.
    APPEND `          <tt:value ref="NUMERIC_TEXT"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="CHARACTER_TEXT!=C('abcde')" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="characterText">` TO me->exp_transformation.
    APPEND `          <tt:value ref="CHARACTER_TEXT"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="STRING_TEXT!=C('Default text')" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="stringText">` TO me->exp_transformation.
    APPEND `          <tt:value ref="STRING_TEXT"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="DATE_FIELD!=D('19720401')" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="dateField">` TO me->exp_transformation.
    APPEND `          <tt:value ref="DATE_FIELD" option="format(dateTimeOffset)"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="TIME_FIELD!=T('201500')" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="timeField">` TO me->exp_transformation.
    APPEND `          <tt:value ref="TIME_FIELD" option="format(dateTimeOffset)"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(DATE_TIME_FIELD)" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="dateTimeField">` TO me->exp_transformation.
    APPEND `          <tt:value ref="DATE_TIME_FIELD" option="format(dateTimeOffset)"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="BOOL_TRUE!=C('X')" frq="?">` TO me->exp_transformation.
    APPEND `        <bool name="boolTrue">` TO me->exp_transformation.
    APPEND `          <tt:value ref="BOOL_TRUE" option="format(boolean)"/>` TO me->exp_transformation.
    APPEND `        </bool>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="BOOL_FALSE!=C('')" frq="?">` TO me->exp_transformation.
    APPEND `        <bool name="boolFalse">` TO me->exp_transformation.
    APPEND `          <tt:value ref="BOOL_FALSE" option="format(boolean)"/>` TO me->exp_transformation.
    APPEND `        </bool>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="ENUM_TYPE!=N('01')" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="enumType">` TO me->exp_transformation.
    APPEND `          <tt:value ref="ENUM_TYPE" map="` TO me->exp_transformation.
    APPEND `            val(N('00'))=xml('general'),` TO me->exp_transformation.
    APPEND `            val(N('01'))=xml('exitClass')` TO me->exp_transformation.
    APPEND `          "/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER"> ` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'fourByteInt;eightByteInt;binFloat;byteLike;byteLike2;decimalFloat16;decimalFloat34;packedNumber;numericText;characterText;stringText;dateField;timeField;dateTimeField;boolTrue;boolFalse;enumType;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act = act_output no_log_check = abap_true ).
    log = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log                = log
                                                             exp_message        = VALUE #( msgid = 'ZAFF_TOOLS'
                                                                                           msgno = 117
                                                                                           attr1 = `UTCLONG` )
                                                             exp_component_name = `STRUCTURE_DIFFERENT_DEFAULT-DATE_TIME_FIELD`
                                                             exp_type           = zif_aff_log=>c_message_type-warning ).
  ENDMETHOD.

  METHOD structure_with_default_problem.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_default_problem.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="STRING_ELEMENT" val="C('DefaultString')"/>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="ENUM_SHOW_ALWAYS" val="N('01')"/>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `        <num name="integer">` TO me->exp_transformation.
    APPEND `          <tt:value ref="INTEGER" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `        <str name="stringElement">` TO me->exp_transformation.
    APPEND `          <tt:value ref="STRING_ELEMENT"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `        <str name="enumRequired">` TO me->exp_transformation.
    APPEND `          <tt:value ref="ENUM_REQUIRED" map="` TO me->exp_transformation.
    APPEND `            val(N('00'))=xml('general'),` TO me->exp_transformation.
    APPEND `            val(N('01'))=xml('exitClass')` TO me->exp_transformation.
    APPEND `          "/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `        <str name="enumShowAlways">` TO me->exp_transformation.
    APPEND `          <tt:value ref="ENUM_SHOW_ALWAYS" map="` TO me->exp_transformation.
    APPEND `            val(N('00'))=xml('general'),` TO me->exp_transformation.
    APPEND `            val(N('01'))=xml('exitClass')` TO me->exp_transformation.
    APPEND `          "/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'integer;stringElement;enumRequired;enumShowAlways;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act = act_output no_log_check = abap_true ).
    log = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log                = log
                                                             exp_message        = VALUE #( msgid = 'ZAFF_TOOLS'
                                                                                           msgno = 126 )
                                                             exp_component_name = `STRUCTURE_WITH_DEFAULT_PROBLEM-INTEGER`
                                                             exp_type           = zif_aff_log=>c_message_type-warning ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log                = log
                                                             exp_message        = VALUE #( msgid = 'ZAFF_TOOLS'
                                                                                           msgno = 126 )
                                                             exp_component_name = `STRUCTURE_WITH_DEFAULT_PROBLEM-ENUM_REQUIRED`
                                                             exp_type           = zif_aff_log=>c_message_type-warning ).

  ENDMETHOD.

  METHOD nested_struc_with_default.
    DATA test_type TYPE zcl_aff_test_types=>nested_struc_with_default.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:assign to-ref="OUTER_COMPONENT" val="I(10)"/>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="OUTER_COMPONENT!=I(10)" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="outerComponent">` TO me->exp_transformation.
    APPEND `          <tt:value ref="OUTER_COMPONENT" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(MIDDLE_STRUC)" frq="?">` TO me->exp_transformation.
    APPEND `        <object name="middleStruc" tt:ref="MIDDLE_STRUC">` TO me->exp_transformation.
    APPEND `          <tt:assign to-ref="MIDDLE_COMPONENT" val="C('abcd')"/>` TO me->exp_transformation.
    APPEND `          <tt:group>` TO me->exp_transformation.
    APPEND `            <tt:cond s-check="MIDDLE_COMPONENT!=C('abcd')" frq="?">` TO me->exp_transformation.
    APPEND `              <str name="middleComponent">` TO me->exp_transformation.
    APPEND `                <tt:value ref="MIDDLE_COMPONENT"/>` TO me->exp_transformation.
    APPEND `              </str>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `            <tt:cond s-check="not-initial(INNER_STRUC)" frq="?">` TO me->exp_transformation.
    APPEND `              <object name="innerStruc" tt:ref="INNER_STRUC">` TO me->exp_transformation.
    APPEND `                <tt:assign to-ref="INNER_COMPONENT" val="C('Default Value')"/>` TO me->exp_transformation.
    APPEND `                <tt:group>` TO me->exp_transformation.
    APPEND `                  <tt:cond s-check="INNER_COMPONENT!=C('Default Value')" frq="?">` TO me->exp_transformation.
    APPEND `                    <str name="innerComponent">` TO me->exp_transformation.
    APPEND `                      <tt:value ref="INNER_COMPONENT"/>` TO me->exp_transformation.
    APPEND `                    </str>` TO me->exp_transformation.
    APPEND `                  </tt:cond>` TO me->exp_transformation.
    APPEND `                  <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `                     <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `                      <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `                        <tt:with-parameter name="MEMBERS" val="'innerComponent;'"/>` TO me->exp_transformation.
    APPEND `                      </tt:call-method>` TO me->exp_transformation.
    APPEND `                      <tt:skip/>` TO me->exp_transformation.
    APPEND `                    </_>` TO me->exp_transformation.
    APPEND `                  </tt:d-cond>` TO me->exp_transformation.
    APPEND `                  <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `                    <__/>` TO me->exp_transformation.
    APPEND `                  </tt:d-cond>` TO me->exp_transformation.
    APPEND `                </tt:group>` TO me->exp_transformation.
    APPEND `              </object>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `            <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `               <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `                <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `                  <tt:with-parameter name="MEMBERS" val="'middleComponent;innerStruc;'"/>` TO me->exp_transformation.
    APPEND `                </tt:call-method>` TO me->exp_transformation.
    APPEND `                <tt:skip/>` TO me->exp_transformation.
    APPEND `              </_>` TO me->exp_transformation.
    APPEND `            </tt:d-cond>` TO me->exp_transformation.
    APPEND `            <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `              <__/>` TO me->exp_transformation.
    APPEND `            </tt:d-cond>` TO me->exp_transformation.
    APPEND `          </tt:group>` TO me->exp_transformation.
    APPEND `        </object>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'outerComponent;middleStruc;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD type_of_enumtype_and_co_differ.
    TRY.
        DATA test_type TYPE zcl_aff_test_types=>enum.
*    expect error /no output (type of type and constant has to be the same)
        test_generator->generate_type( test_type ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_aff_tools INTO DATA(ex).
        cl_abap_unit_assert=>assert_equals( exp = 'CO_ENUM' act = ex->if_t100_dyn_msg~msgv1 ).
        cl_abap_unit_assert=>assert_equals( exp = 'ENUM' act = ex->if_t100_dyn_msg~msgv2 ).
        cl_abap_unit_assert=>assert_equals( exp = 'ZAFF_TOOLS' act = ex->if_t100_message~t100key-msgid ).
        cl_abap_unit_assert=>assert_equals( exp = 122 act = ex->if_t100_message~t100key-msgno ).
    ENDTRY.
  ENDMETHOD.

  METHOD wrong_default_type_link.
    DATA test_type TYPE zcl_aff_test_types=>struc_link_wrong_type.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(DEFAULT_LINK)" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="defaultLink">` TO me->exp_transformation.
    APPEND `          <tt:value ref="DEFAULT_LINK" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'defaultLink;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.

    validate_output( act = act_output no_log_check = abap_true ).
    log = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log                = log
                                                             exp_message        = VALUE #( msgid = 'ZAFF_TOOLS'
                                                                                           msgno = 122
                                                                                           attr1 = `CO_TEST`
                                                                                           attr2 = `STRUC_LINK_WRONG_TYPE-DEFAULT_LINK` )
                                                             exp_component_name = `STRUC_LINK_WRONG_TYPE-DEFAULT_LINK`
                                                             exp_type           = zif_aff_log=>c_message_type-warning ).
  ENDMETHOD.

  METHOD structure_with_wrong_default.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_wrong_default.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(ELEMENT_ONE)" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="elementOne">` TO me->exp_transformation.
    APPEND `          <tt:value ref="ELEMENT_ONE" map="` TO me->exp_transformation.
    APPEND `            val(N('00'))=xml('general'),` TO me->exp_transformation.
    APPEND `            val(N('01'))=xml('classicBadi')` TO me->exp_transformation.
    APPEND `          "/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(ELEMENT_TWO)" frq="?">` TO me->exp_transformation.
    APPEND `        <str name="elementTwo">` TO me->exp_transformation.
    APPEND `          <tt:value ref="ELEMENT_TWO" map="` TO me->exp_transformation.
    APPEND `            val(N('00'))=xml('general'),` TO me->exp_transformation.
    APPEND `            val(N('01'))=xml('classicBadi')` TO me->exp_transformation.
    APPEND `          "/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'elementOne;elementTwo;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act = act_output no_log_check = abap_true ).
    log = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log                = log
                                                             exp_message        = VALUE #( msgid = 'ZAFF_TOOLS'
                                                                                           msgno = 105
                                                                                           attr1 = `WRONG_COMPONENT`
                                                                                           attr2 = `ENUM_VALUES` )
                                                             exp_component_name = `STRUCTURE_WITH_WRONG_DEFAULT-ELEMENT_ONE`
                                                             exp_type           = zif_aff_log=>c_message_type-warning ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log                = log
                                                             exp_message        = VALUE #( msgid = 'ZAFF_TOOLS'
                                                                                           msgno = 111
                                                                                           attr1 = `$default` )
                                                             exp_component_name = `STRUCTURE_WITH_WRONG_DEFAULT-ELEMENT_TWO`
                                                             exp_type           = zif_aff_log=>c_message_type-warning ).
  ENDMETHOD.

  METHOD simple_element_with_callack.
    DATA test_type TYPE zcl_aff_test_types=>string_callback.
    DATA(act_output) = test_generator->generate_type( test_type ).
    DATA(exp_schema) = VALUE string_table(
( `  <str>` )
( `    <tt:call-method class="zcl_aff_test_types" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` )
( |      <tt:with-parameter name="string_callback" ref=".{ st_root_name }"/>| )
( `    </tt:call-method>` )
( `  </str>` ) ).
    INSERT LINES OF exp_schema INTO TABLE me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD table_with_callback.
    DATA test_type TYPE zcl_aff_test_types=>table_with_callback.
    DATA(act_output) = test_generator->generate_type( test_type ).
    DATA(exp_schema) = VALUE string_table(
( `  <array>` )
( `    <tt:call-method class="zcl_aff_test_types" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` )
( |      <tt:with-parameter name="table_with_callback" ref=".{ st_root_name }"/>| )
( `    </tt:call-method>` )
( `  </array>` ) ).
    INSERT LINES OF exp_schema INTO TABLE me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD table_with_call_of_struc.
    DATA test_type TYPE zcl_aff_test_types=>table_call_of_struc.
    DATA(act_output) = test_generator->generate_type( test_type ).
    DATA(exp_schema) = VALUE string_table(
( `  <array>` )
( `    <tt:call-method class="zcl_aff_test_types" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` )
( |      <tt:with-parameter name="table_call_of_struc" ref=".{ st_root_name }"/>| )
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
    DATA test_type TYPE zcl_aff_test_types=>structure_with_callback.
    DATA(act_output) = test_generator->generate_type( test_type ).
    DATA(exp_schema) = VALUE string_table(
( `  <object>` )
( `    <tt:call-method class="zcl_aff_test_types" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` )
( |      <tt:with-parameter name="structure_with_callback" ref=".{ st_root_name }"/>| )
( `    </tt:call-method>` )
( `  </object>` ) ).
    INSERT LINES OF exp_schema INTO TABLE me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD table_of_struc_with_callback.
    DATA test_type TYPE zcl_aff_test_types=>table_of_struc_with_callback.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `   <tt:cond>` TO me->exp_transformation.
    APPEND `     <array>` TO me->exp_transformation.
    APPEND `       <tt:loop>` TO me->exp_transformation.
    APPEND `         <tt:group>` TO me->exp_transformation.
    APPEND `           <tt:cond>` TO me->exp_transformation.
    APPEND `             <object>` TO me->exp_transformation.
    APPEND `              <tt:call-method class="zcl_aff_test_types" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` TO me->exp_transformation.
    APPEND `                 <tt:with-parameter name="structure_with_callback" ref="$ref"/>` TO me->exp_transformation.
    APPEND `               </tt:call-method>` TO me->exp_transformation.
    APPEND `             </object>` TO me->exp_transformation.
    APPEND `           </tt:cond>` TO me->exp_transformation.
    APPEND `           </tt:group>` TO me->exp_transformation.
    APPEND `         </tt:loop>` TO me->exp_transformation.
    APPEND `       </array>` TO me->exp_transformation.
    APPEND `     </tt:cond>` TO me->exp_transformation.
    validate_output( act = act_output ).
  ENDMETHOD.

  METHOD struc_of_table_with_callback.
    DATA test_type TYPE zcl_aff_test_types=>struc_of_table_with_callback.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `      <tt:cond>` TO me->exp_transformation.
    APPEND `        <object>` TO me->exp_transformation.
    APPEND `          <tt:group>` TO me->exp_transformation.
    APPEND `            <tt:cond>` TO me->exp_transformation.
    APPEND `              <array>` TO me->exp_transformation.
    APPEND `                <tt:call-method class="zcl_aff_test_types" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` TO me->exp_transformation.
    APPEND `                  <tt:with-parameter name="my_table_with_callback" ref="MY_TABLE_WITH_CALLBACK"/>` TO me->exp_transformation.
    APPEND `                </tt:call-method>` TO me->exp_transformation.
    APPEND `              </array>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `            <tt:cond s-check="not-initial(MY_SECOND_ELEMENT)" frq="?">` TO me->exp_transformation.
    APPEND `             <num name="mySecondElement">` TO me->exp_transformation.
    APPEND `              <tt:value ref="MY_SECOND_ELEMENT" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `             </num>` TO me->exp_transformation.
    APPEND `            </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'myTableWithCallback;mySecondElement;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `          </tt:group>` TO me->exp_transformation.
    APPEND `        </object>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD struc_in_struc_with_callback.
    DATA test_type TYPE zcl_aff_test_types=>struc_in_struc_with_callback.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(MY_FIRST_ELEMENT)" frq="?">` TO me->exp_transformation.
    APPEND `            <str name="myFirstElement">` TO me->exp_transformation.
    APPEND `              <tt:value ref="MY_FIRST_ELEMENT"/>` TO me->exp_transformation.
    APPEND `            </str>` TO me->exp_transformation.
    APPEND `        </tt:cond>` TO me->exp_transformation.
    APPEND `        <tt:cond>` TO me->exp_transformation.
    APPEND `          <object>` TO me->exp_transformation.
    APPEND `            <tt:call-method class="zcl_aff_test_types" d-name="deserialize" reader="reader" s-name="serialize" writer="writer">` TO me->exp_transformation.
    APPEND `              <tt:with-parameter name="my_struc_with_callback" ref="MY_STRUC_WITH_CALLBACK"/>` TO me->exp_transformation.
    APPEND `            </tt:call-method>` TO me->exp_transformation.
    APPEND `          </object>` TO me->exp_transformation.
    APPEND `        </tt:cond>` TO me->exp_transformation.
    APPEND `        <tt:cond s-check="not-initial(MY_THIRD_ELEMENT)" frq="?">` TO me->exp_transformation.
    APPEND `          <num name="myThirdElement">` TO me->exp_transformation.
    APPEND `            <tt:value ref="MY_THIRD_ELEMENT" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `          </num>` TO me->exp_transformation.
    APPEND `        </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'myFirstElement;myStrucWithCallback;myThirdElement;'"/>` TO me->exp_transformation.
    APPEND `         </tt:call-method>` TO me->exp_transformation.
    APPEND `         <tt:skip/>` TO me->exp_transformation.
    APPEND `      </_>` TO me->exp_transformation.
    APPEND `   </tt:d-cond>` TO me->exp_transformation.
    APPEND `   <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `       <__/>` TO me->exp_transformation.
    APPEND `   </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act_output ).
  ENDMETHOD.

  METHOD structure_with_wrong_callback.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_wrong_callback.
    DATA(act_output) = test_generator->generate_type( test_type ).

    APPEND `<tt:cond>` TO me->exp_transformation.
    APPEND `  <object>` TO me->exp_transformation.
    APPEND `    <tt:group>` TO me->exp_transformation.
    APPEND `      <tt:cond frq="?">` TO me->exp_transformation.
    APPEND `        <str name="myFirstElement">` TO me->exp_transformation.
    APPEND `          <tt:value ref="MY_FIRST_ELEMENT"/>` TO me->exp_transformation.
    APPEND `        </str>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:cond s-check="not-initial(MY_SECOND_ELEMENT)" frq="?">` TO me->exp_transformation.
    APPEND `        <num name="mySecondElement">` TO me->exp_transformation.
    APPEND `          <tt:value ref="MY_SECOND_ELEMENT" option="format(alpha)"/>` TO me->exp_transformation.
    APPEND `        </num>` TO me->exp_transformation.
    APPEND `      </tt:cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="*">` TO me->exp_transformation.
    APPEND `         <_ tt:lax="on">` TO me->exp_transformation.
    APPEND `          <tt:call-method class="CL_AFF_XSLT_CALLBACK_TYPE" name="RAISE_DIFFERENT_TYPE_EXCEPTION" reader="IO_READER">` TO me->exp_transformation.
    APPEND `            <tt:with-parameter name="MEMBERS" val="'myFirstElement;mySecondElement;'"/>` TO me->exp_transformation.
    APPEND `          </tt:call-method>` TO me->exp_transformation.
    APPEND `          <tt:skip/>` TO me->exp_transformation.
    APPEND `        </_>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `      <tt:d-cond frq="?">` TO me->exp_transformation.
    APPEND `        <__/>` TO me->exp_transformation.
    APPEND `      </tt:d-cond>` TO me->exp_transformation.
    APPEND `    </tt:group>` TO me->exp_transformation.
    APPEND `  </object>` TO me->exp_transformation.
    APPEND `</tt:cond>` TO me->exp_transformation.
    validate_output( act = act_output no_log_check = abap_true ).
    log = cut->zif_aff_writer~get_log( ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log                = log
                                                             exp_message        = VALUE #( msgid = 'ZAFF_TOOLS'
                                                                                           msgno = 106 )
                                                             exp_component_name = `STRUCTURE_WITH_WRONG_CALLBACK-MY_FIRST_ELEMENT`
                                                             exp_type           = zif_aff_log=>c_message_type-warning ).
    zcl_aff_tools_unit_test_helper=>assert_log_contains_msg( log                = log
                                                             exp_message        = VALUE #( msgid = 'ZAFF_TOOLS'
                                                                                           msgno = 109
                                                                                           attr1 = `$callbackClass` )
                                                             exp_component_name = `STRUCTURE_WITH_WRONG_CALLBACK-MY_SECOND_ELEMENT`
                                                             exp_type           = zif_aff_log=>c_message_type-warning ).
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
DURATION SHORT
RISK LEVEL DANGEROUS.

  PRIVATE SECTION.
    CONSTANTS c_xslt_prefix TYPE string VALUE `ZAFF_TRANSFORMATION_GEN_` ##NO_TEXT.

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

      simple_element_with_callack FOR TESTING RAISING cx_static_check,

      table_with_callback FOR TESTING RAISING cx_static_check,

      table_with_callback_of_struc FOR TESTING RAISING cx_static_check,

      struc_of_table_with_callback FOR TESTING RAISING cx_static_check,

      structure_with_elem_callback FOR TESTING RAISING cx_static_check,

      table_of_struc_with_callback FOR TESTING RAISING cx_static_check,

      structure_with_num_text FOR TESTING RAISING cx_static_check,

      structure_with_include FOR TESTING RAISING cx_static_check,

      structure_with_default_problem FOR TESTING RAISING cx_static_check,

      from_abap_to_json
        IMPORTING
          test_type     TYPE data
        EXPORTING
          VALUE(result) TYPE string_table
          VALUE(json)   TYPE xstring
        RAISING
          zcx_aff_tools
          cx_sxml_illegal_argument_error
          cx_aff_root,
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
          zcx_aff_tools
          cx_sxml_illegal_argument_error
          cx_aff_root,
      setup.
ENDCLASS.

CLASS zcl_aff_writer_xslt DEFINITION LOCAL FRIENDS ltcl_integration_test_ad.

CLASS ltcl_integration_test_ad IMPLEMENTATION.

  METHOD setup.
    TEST-INJECTION set_parameter_name.
      write_callback( name_of_callback_class = abap_doc-callback_class parameter_name = 'element_name' ref_name = ref_name ).
    END-TEST-INJECTION.
  ENDMETHOD.

  METHOD teardown.
    CLEAR exp_json.
    CLEAR manually_changed_json.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD simple_struc_with_extra_field.
    DATA test_type TYPE zcl_aff_test_types=>my_structure.
    test_type-my_second_element = 5.
    DATA act_data LIKE test_type.

    APPEND `{` TO exp_json.
    APPEND ` "mySecondElement":5` TO exp_json.
    APPEND `}` TO exp_json.

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
        act_data  = act_data
    ).
  ENDMETHOD.

  METHOD simple_structure_with_required.
    DATA test_type TYPE zcl_aff_test_types=>my_structure2.
    test_type-my_second_element = 5.
    DATA act_data LIKE test_type.

    APPEND `{` TO exp_json.
    APPEND ` "myFirstElement":"",` TO exp_json.
    APPEND ` "mySecondElement":5` TO exp_json.
    APPEND `}` TO exp_json.

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data
    ).
  ENDMETHOD.

  METHOD structure_in_structure.
    DATA test_type TYPE zcl_aff_test_types=>my_structure3.
    test_type = VALUE #(
      nested_struc = VALUE #( my_element = 'Nested Element')
      my_element = 'Not nested Element'
    ).
    DATA act_data LIKE test_type.

    APPEND `{` TO exp_json.
    APPEND `  "nestedStruc": {` TO exp_json.
    APPEND `    "myElement":"Nested Element"` TO exp_json.
    APPEND `  },` TO exp_json.
    APPEND ` "myElement": "Not nested Element"` TO exp_json.
    APPEND `}` TO exp_json.

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data
    ).
  ENDMETHOD.

  METHOD simple_table.
    DATA test_type TYPE zcl_aff_test_types=>my_standard_table.
    test_type = VALUE #( ( `line_1` ) ( `line_2` ) ).
    DATA act_data LIKE test_type.

    APPEND `[` TO exp_json.
    APPEND `  "line_1",` TO exp_json.
    APPEND `  "line_2"` TO exp_json.
    APPEND `]` TO exp_json.
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data
    ).
  ENDMETHOD.

  METHOD simple_type_with_enum_values.
    DATA test_type TYPE zcl_aff_test_types=>category.
    DATA act_data LIKE test_type.

    APPEND `  "general"` TO exp_json.
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data
    ).
  ENDMETHOD.

  METHOD structure_with_enum_values.
    DATA test_type TYPE zcl_aff_test_types=>ty_class_properties.
    test_type =
          VALUE #(
            header = VALUE #( description = 'description of the class' abap_language_version = 5 original_language = 'E' )
            class_category = '01' ).

    DATA act_data LIKE test_type.

    APPEND `{` TO exp_json.
    APPEND `  "header": {` TO exp_json.
    APPEND `    "description":"description of the class",` TO exp_json.
    APPEND `    "originalLanguage":"en",` TO exp_json.
    APPEND `    "abapLanguageVersion":"cloudDevelopment"` TO exp_json.
    APPEND `  },` TO exp_json.
    APPEND ` "classCategory": "exitClass"` TO exp_json.
    APPEND `}` TO exp_json.

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data
    ).
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
        field3 = 'ZZ'
      ).
    DATA act_data LIKE test_type.

    APPEND `{` TO exp_json.
    APPEND `  "field1":0,` TO exp_json.
    APPEND `  "field2":"AA",` TO exp_json.
    APPEND `  "list1":{` TO exp_json.
    APPEND `    "elementOfList1":50,` TO exp_json.
    APPEND `    "list2":{` TO exp_json.
    APPEND `      "elementOfList2": "Deep Nested"` TO exp_json.
    APPEND `    }` TO exp_json.
    APPEND `  },` TO exp_json.
    APPEND ` "field3": "ZZ"` TO exp_json.
    APPEND `}` TO exp_json.
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data
    ).
  ENDMETHOD.

  METHOD nested_structure_with_table.
    DATA test_type TYPE zcl_aff_test_types=>outer_struc.
    test_type =
        VALUE #(
            inner_struc = VALUE #(
                element_of_inner_struc = 50
                inner_table_var = VALUE #( ( `line_1` ) ( `line_2` ) ) )
            field2 = 'ZZ'
        ).
    DATA act_data LIKE test_type.

    APPEND `{` TO exp_json.
    APPEND `  "innerStruc": {` TO exp_json.
    APPEND `    "elementOfInnerStruc":50,` TO exp_json.
    APPEND `    "innerTableVar":[` TO exp_json.
    APPEND `      "line_1",` TO exp_json.
    APPEND `      "line_2"` TO exp_json.
    APPEND `    ]` TO exp_json.
    APPEND `  },` TO exp_json.
    APPEND ` "field2": "ZZ"` TO exp_json.
    APPEND `}` TO exp_json.

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data
    ).
  ENDMETHOD.

  METHOD structure_with_table_and_enum.
    DATA test_type TYPE zcl_aff_test_types=>aff_test_type.
    test_type =
        VALUE #(
            field1 = 25
            inner_struc = VALUE #(
                inner_element = 50 )
            field2 = 'ZZ'
           field_with_values = 01
        ).
    DATA act_data LIKE test_type.

    APPEND `{` TO exp_json.
    APPEND `  "field1":25,` TO exp_json.
    APPEND `  "innerStruc": {` TO exp_json.
    APPEND `    "innerElement":50` TO exp_json.
    APPEND `  },` TO exp_json.
    APPEND ` "field2": "ZZ",` TO exp_json.
    APPEND ` "fieldWithValues":"exitClass"` TO exp_json.
    APPEND `}` TO exp_json.

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data
    ).
  ENDMETHOD.

  METHOD table_in_table.
    DATA test_type TYPE zcl_aff_test_types=>first_table.
    test_type = VALUE #(
          ( VALUE #( ( `table_1_line_1` ) ( `table_1_line_2` ) ) )
          ( VALUE #( ( `table_2_line_1` ) ( `table_2_line_2` ) ) ) ).

    DATA act_data LIKE test_type.

    APPEND `[` TO exp_json.
    APPEND ` [` TO exp_json.
    APPEND `  "table_1_line_1",` TO exp_json.
    APPEND `  "table_1_line_2"` TO exp_json.
    APPEND ` ],` TO exp_json.
    APPEND ` [` TO exp_json.
    APPEND `  "table_2_line_1",` TO exp_json.
    APPEND `  "table_2_line_2"` TO exp_json.
    APPEND ` ]` TO exp_json.
    APPEND `]` TO exp_json.

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data
    ).
  ENDMETHOD.


  METHOD nested_struc_no_default.
    DATA test_type TYPE zcl_aff_test_types=>nested_struc_with_default.
    DATA act_data LIKE test_type.
    test_type = VALUE #( outer_component = 12
                         middle_struc = VALUE #( middle_component = 'wxyz'
                                                 inner_struc = VALUE #( inner_component = 'Inner Component' ) ) ).

    APPEND `{` TO exp_json.
    APPEND `  "outerComponent":12,` TO exp_json.
    APPEND `  "middleStruc": {` TO exp_json.
    APPEND `    "middleComponent":"wxyz",` TO exp_json.
    APPEND `    "innerStruc": {` TO exp_json.
    APPEND `      "innerComponent":"Inner Component"` TO exp_json.
    APPEND `    }` TO exp_json.
    APPEND `  }` TO exp_json.
    APPEND `}` TO exp_json.
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data
    ).
  ENDMETHOD.

  METHOD nested_struc_with_default.
    DATA test_type TYPE zcl_aff_test_types=>nested_struc_with_default.
    DATA act_data LIKE test_type.
    test_type = VALUE #( outer_component = 10
                         middle_struc = VALUE #( middle_component = 'abcd'
                                                 inner_struc = VALUE #( inner_component = 'Default Value' ) ) ).

    APPEND `{` TO exp_json.
    APPEND `  "middleStruc": {` TO exp_json.
    APPEND `    "innerStruc": { }` TO exp_json.
    APPEND `  }` TO exp_json.
    APPEND `}` TO exp_json.

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data
    ).
  ENDMETHOD.

  METHOD structure_different_default.
    DATA test_type TYPE zcl_aff_test_types=>structure_different_default.
    DATA act_data LIKE test_type.
    test_type = VALUE #(  four_byte_int = 5
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

    APPEND `{` TO exp_json.
    APPEND `"dateTimeField": "9999-12-31T23:59:59.9999999+00:00"` TO exp_json.
    APPEND `}` TO exp_json.
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data
    ).
  ENDMETHOD.

  METHOD structure_with_default_problem.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_default_problem.
    DATA act_data LIKE test_type.
    test_type = VALUE #(  integer =  5
                          string_element = 'DefaultString'
                          enum_required = '01'
                          enum_show_always = '01' ) ##LITERAL.

    APPEND `{` TO exp_json.
    APPEND `"integer" : 5,` TO exp_json.
    APPEND `"stringElement" : "DefaultString",` TO exp_json.
    APPEND `"enumRequired" : "exitClass",` TO exp_json.
    APPEND `"enumShowAlways" : "exitClass"` TO exp_json.
    APPEND `}` TO exp_json.

    manually_changed_json =
  `{` &&
  `"integer" : 5,` &&
  `"enumRequired" : "exitClass"` &&
  `}`.

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = act_data
    ).

  ENDMETHOD.


  METHOD simple_element_with_callack.
    zcl_aff_test_types=>choose_code = zcl_aff_test_types=>choose_codes-outer_element.
    DATA test_type TYPE zcl_aff_test_types=>string_callback.
    test_type = 'String Element'.
    zcl_aff_test_types=>set_expected( test_type ).
    APPEND `"callbackClass was called"` TO exp_json.
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type
    ).
  ENDMETHOD.

  METHOD table_with_callback.
    zcl_aff_test_types=>choose_code = zcl_aff_test_types=>choose_codes-outer_table.
    DATA test_type TYPE zcl_aff_test_types=>table_with_callback.
    test_type = VALUE #( ( `First` ) ( `Second` ) ).
    zcl_aff_test_types=>set_expected( test_type ).

    APPEND `[` TO exp_json.
    APPEND `"callbackClass was called"` TO exp_json.
    APPEND `]` TO exp_json.
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type
    ).
  ENDMETHOD.

  METHOD table_with_callback_of_struc.
    zcl_aff_test_types=>choose_code = zcl_aff_test_types=>choose_codes-outer_table.
    DATA test_type TYPE zcl_aff_test_types=>table_call_of_struc.
    test_type = VALUE #( ( VALUE #( my_first_element = `First` my_second_element = 5 ) ) ).
    zcl_aff_test_types=>set_expected( test_type ).

    APPEND `[` TO exp_json.
    APPEND `"callbackClass was called"` TO exp_json.
    APPEND `]` TO exp_json.
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type
    ).
  ENDMETHOD.

  METHOD structure_with_callback.
    zcl_aff_test_types=>choose_code = zcl_aff_test_types=>choose_codes-outer_structure.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_callback.
    test_type = VALUE #( my_element = 5 ).
    zcl_aff_test_types=>set_expected( test_type ).

    APPEND `{` TO exp_json.
    APPEND `"elementName": "callbackClass was called"` TO exp_json.
    APPEND `}` TO exp_json.
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type
    ).
  ENDMETHOD.


  METHOD struc_of_table_with_callback.
    zcl_aff_test_types=>choose_code = zcl_aff_test_types=>choose_codes-inner_table.
    DATA test_type TYPE zcl_aff_test_types=>struc_of_table_with_callback.
    DATA table_with_callback LIKE test_type-my_table_with_callback.
    table_with_callback = VALUE #(  ( `first` ) ( `second` ) ).
    zcl_aff_test_types=>set_expected( table_with_callback ).
    test_type = VALUE #( my_table_with_callback = table_with_callback my_second_element = 5 ).

    APPEND `{` TO exp_json.
    APPEND `"elementName": [` TO exp_json.
    APPEND `  "callbackClass was called"` TO exp_json.
    APPEND ` ],` TO exp_json.
    APPEND `"mySecondElement": 5` TO exp_json.
    APPEND `}` TO exp_json.
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type
    ).
  ENDMETHOD.

  METHOD struc_in_struc_with_callback.
    zcl_aff_test_types=>choose_code = zcl_aff_test_types=>choose_codes-inner_structure.
    DATA test_type TYPE zcl_aff_test_types=>struc_in_struc_with_callback.
    DATA struc_with_callback LIKE test_type-my_struc_with_callback.
    struc_with_callback = VALUE #( my_element = 5 ).
    zcl_aff_test_types=>set_expected( struc_with_callback ).
    test_type = VALUE #( my_first_element = 'firstElement'
                         my_struc_with_callback = struc_with_callback
                         my_third_element = 6 ).

    APPEND `{` TO exp_json.
    APPEND `  "myFirstElement": "firstElement",` TO exp_json.
    APPEND `  "elementName": {` TO exp_json.
    APPEND `     "component":"callbackClass was called"` TO exp_json.
    APPEND `  },` TO exp_json.
    APPEND `  "myThirdElement": 6` TO exp_json.
    APPEND `}` TO exp_json.
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type
    ).
  ENDMETHOD.

  METHOD structure_with_elem_callback.
    zcl_aff_test_types=>choose_code = zcl_aff_test_types=>choose_codes-inner_element.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_elem_callback.
    test_type = VALUE #( my_first_element = 'firstElement'
                         my_second_element = 4 ).
    zcl_aff_test_types=>set_expected( 'firstElement' ).

    APPEND `{` TO exp_json.
    APPEND `"elementName": "callbackClass was called",` TO exp_json.
    APPEND `"mySecondElement": 4` TO exp_json.
    APPEND `}` TO exp_json.
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type
    ).
  ENDMETHOD.

  METHOD table_of_struc_with_callback.
    zcl_aff_test_types=>choose_code = zcl_aff_test_types=>choose_codes-outer_structure.
    DATA test_type TYPE zcl_aff_test_types=>table_of_struc_with_callback.
    test_type = VALUE #( ( my_element = 5 ) ( my_element = 10 ) ).
    zcl_aff_test_types=>set_expected( VALUE zcl_aff_test_types=>structure_with_callback( my_element = 5 ) ).

    APPEND `[` TO exp_json.
    APPEND `  {` TO exp_json.
    APPEND `    "elementName": "callbackClass was called"` TO exp_json.
    APPEND `  },` TO exp_json.
    APPEND `  {` TO exp_json.
    APPEND `    "elementName": "callbackClass was called"` TO exp_json.
    APPEND `  }` TO exp_json.
    APPEND `]` TO exp_json.

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type
    ).

  ENDMETHOD.

  METHOD structure_with_num_text.
    DATA test_type TYPE zcl_aff_test_types=>struc_with_num_text.
    test_type = VALUE #( numerical_text1 = '4' numerical_text2 = '1234' ).

    APPEND `{` TO exp_json.
    APPEND `    "numericalText1": "0004",` TO exp_json.
    APPEND `    "numericalText2": "1234",` TO exp_json.
    APPEND `    "numericalText3": "0000"` TO exp_json.
    APPEND `}` TO exp_json.
    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type
    ).
  ENDMETHOD.

  METHOD structure_with_include.
    DATA test_type TYPE zcl_aff_test_types=>structure_with_include.
    test_type = VALUE #( first_element  = 'first'
                         second_element = VALUE #( my_first_element  = 'inner first'
                                                   my_second_element = 9 )
                         third_element  = 10 ).

    APPEND `{` TO exp_json.
    APPEND `    "firstElement": "first",` TO exp_json.
    APPEND `    "secondElement": { ` TO exp_json.
    APPEND `      "myFirstElement": "inner first", ` TO exp_json.
    APPEND `      "mySecondElement": 9 ` TO exp_json.
    APPEND `    },` TO exp_json.
    APPEND `    "otherElement": 0`  TO exp_json.
    APPEND `}` TO exp_json.

    do_integration_test(
      EXPORTING
        test_type = test_type
      CHANGING
        act_data  = test_type
    ).
  ENDMETHOD.

  METHOD do_integration_test.
    from_abap_to_json(
      EXPORTING
        test_type = test_type
      IMPORTING
        result    = DATA(act_json)
        json      = DATA(json_xstring)
    ).

    assert_json_equals( actual_json_stringtab = act_json expected_json_stringtab = exp_json ).

    IF manually_changed_json IS NOT INITIAL.
      json_xstring = cl_abap_codepage=>convert_to( manually_changed_json ).
    ENDIF.

    from_json_to_abap(
      EXPORTING
        json   = json_xstring
      IMPORTING
        result = act_data
    ).

    cl_abap_unit_assert=>assert_equals(
      act = act_data
      exp = test_type
    ).
  ENDMETHOD.

  METHOD from_abap_to_json.
    DATA(cut) = NEW zcl_aff_writer_xslt( 'root' ).
    DATA(test_generator) = NEW zcl_aff_generator( cut ).
    DATA(st_content) = test_generator->generate_type( test_type ).

    DATA(st_name) = CONV progname( c_xslt_prefix && st_execution_counter ).
    st_name = |{ st_name WIDTH = 30 PAD = '=' }XT|.

    INSERT REPORT st_name FROM st_content EXTENSION TYPE srext_ext_xslt_source.

    DATA(json_writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    json_writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_linebreaks ).
    json_writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_indent value = '2' ).
    CALL TRANSFORMATION (st_name) SOURCE root = test_type RESULT XML json_writer.


    cl_aff_content_handler_factory=>get_handler_for_plain_text( )->deserialize(
      EXPORTING content = json_writer->get_output( )
      IMPORTING data = result  ).

    json = json_writer->get_output( ).

    st_execution_counter += 1.
  ENDMETHOD.


  METHOD from_json_to_abap.
    DATA(counter) = st_execution_counter - 1.
    DATA(st_name) = CONV progname( c_xslt_prefix && counter ).
    st_name = |{ st_name WIDTH = 30 PAD = '=' }XT|.


    DATA(handler) = NEW cl_aff_content_handler_json(
      simple_transformation = st_name
      st_root_name          = 'ROOT'
    ).

    TRY.
        handler->if_aff_content_handler~deserialize(
          EXPORTING
            content = json
          IMPORTING
            data    = result
        ).
      CATCH cx_aff_root INTO DATA(exception).
        DELETE REPORT st_name.
        cl_abap_unit_assert=>fail( ).
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
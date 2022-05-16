*&---------------------------------------------------------------------*
*& Report z_generate_file_format_folder
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_generate_file_format_folder.
CLASS lcl_generator DEFINITION DEFERRED.
DATA helper TYPE REF TO lcl_generator ##NEEDED.



SELECTION-SCREEN BEGIN OF BLOCK block_1 WITH FRAME TITLE TEXT-020.
  PARAMETERS:
    p_intf  TYPE sobj_name,
    p_examp TYPE sobj_name.
SELECTION-SCREEN END OF BLOCK block_1.

TYPES: BEGIN OF aff_object,
         object_type    TYPE c LENGTH 4,
         interface      TYPE sobj_name,
         example        TYPE sobj_name,
         format_version TYPE i,
       END OF aff_object,
       aff_objects_table TYPE STANDARD TABLE OF aff_object.


CLASS lcl_generator DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    DATA log TYPE REF TO zif_aff_log.
    DATA report_log TYPE stringtab.
    DATA xslt_schema_content TYPE string_table.


    METHODS: constructor,
      start_of_selection,
      on_value_request_for_intfname,
      on_value_request_for_example,
      modify_screen,
      at_selection_screen,
      write_to_zip
        IMPORTING zip_archive TYPE xstring
                  zipname     TYPE string,
      print_logs.

  PRIVATE SECTION.

    DATA:
      zip         TYPE REF TO cl_abap_zip,
      aff_object  TYPE aff_object.

    METHODS: get_sub_type_interfaces
      IMPORTING object        TYPE aff_object
      RETURNING VALUE(result) TYPE string_table,
      add_aff_files_to_zip
        IMPORTING
          files    TYPE if_aff_object_file_handler=>ty_object_files
          filename TYPE string,
      generate_repo_folder,
      create_the_variable_dynamicaly
        IMPORTING absolute_typename TYPE string
        RETURNING VALUE(variable)   TYPE REF TO data
        RAISING   cx_root,
      get_dynpro_value
        IMPORTING fieldname         TYPE string
        RETURNING VALUE(fieldvalue) TYPE string,
      set_value_help_result_to_field
        IMPORTING fieldname               TYPE string
                  value_help_result_table TYPE STANDARD TABLE
        RETURNING VALUE(chosen_value)     TYPE string,
      set_object_infos_in_ui
        IMPORTING i_object TYPE aff_object,
      get_object_infos_by_intfname
        IMPORTING intfname      TYPE string
        RETURNING VALUE(object) TYPE aff_object,
      get_format_version
        IMPORTING intfname              TYPE string
        RETURNING VALUE(format_version) TYPE i,
      get_object_type_path
        IMPORTING interface_name TYPE string
        RETURNING VALUE(path)    TYPE string,
      get_content
        IMPORTING absolute_typename TYPE string
                  interfacename     TYPE string
                  generator         TYPE REF TO zcl_aff_generator
        RETURNING VALUE(result)     TYPE string_table,
      object_as_string
        IMPORTING object        TYPE if_aff_object_file_handler=>ty_object
        RETURNING VALUE(result) TYPE string.

ENDCLASS.

CLASS lcl_generator IMPLEMENTATION.

  METHOD write_to_zip.
    DATA file_name TYPE string.
    DATA path TYPE string.
    DATA fullpath TYPE string.
    DATA user_action TYPE i.
    TRY.
        cl_gui_frontend_services=>file_save_dialog(
          EXPORTING
            default_extension = `zip`
            default_file_name = zipname
          CHANGING
            filename          = file_name
            path              = path
            fullpath          = fullpath
            user_action       = user_action
                                ) ##SUBRC_OK.
      CATCH cx_root.
        INSERT `Either Serialization canceled via UI or file-save-dialog caused errors` INTO TABLE report_log ##NO_TEXT.
        RETURN.
    ENDTRY.

*     On mac computers file_save_dialog( ) does not add ".zip" at the file_name ending.
    IF NOT file_name CP '*.zip'.
      file_name = |{ file_name }.zip| ##NO_TEXT.
      fullpath = |{ fullpath }.zip| ##NO_TEXT.
    ENDIF.

    " split xstring into a table
    CONSTANTS chunk_size TYPE i VALUE 1000.
    DATA line TYPE x LENGTH chunk_size.
    DATA content_as_table LIKE TABLE OF line.
    DATA off TYPE i.
    DATA(xstring_length) = xstrlen( zip_archive ).
    WHILE off < xstring_length.
      line = zip_archive+off.
      APPEND line TO content_as_table.
      off = off + chunk_size.
    ENDWHILE.
    TRY.
        cl_gui_frontend_services=>gui_download(
          EXPORTING
            filename     = file_name
            bin_filesize = xstring_length
            filetype     = 'BIN'
            write_lf     = space
          CHANGING
            data_tab     = content_as_table ).
        INSERT |Success: Zip file created here { fullpath }| INTO TABLE report_log ##NO_TEXT.
      CATCH cx_root.
        INSERT |File { fullpath } not created| INTO TABLE report_log ##NO_TEXT.
    ENDTRY.
  ENDMETHOD.

  METHOD get_sub_type_interfaces.

    APPEND object-interface TO result.
    IF object-interface CP `IF_AFF_FUGR*`.
      APPEND `IF_AFF_FUNC_V1` TO result.
      APPEND `IF_AFF_REPS_V1` TO result.
    ENDIF.
    IF object-interface CP `IF_AFF_TABL*`.
      APPEND `IF_AFF_INDX_V1` TO result.
    ENDIF.

  ENDMETHOD.

  METHOD generate_repo_folder.
    DATA(file_handler) = cl_aff_factory=>get_object_file_handler( ).

    SELECT SINGLE devclass FROM tadir WHERE pgmid = 'R3TR' AND obj_name = @aff_object-example AND object = @aff_object-object_type INTO @DATA(example_obj_devclass).
    DATA(example_main_object) = VALUE if_aff_object_file_handler=>ty_object( devclass  = example_obj_devclass obj_type = aff_object-object_type obj_name = aff_object-example ).

    DATA(example_files) = file_handler->serialize_objects( objects = VALUE #( ( example_main_object ) ) log = NEW cl_aff_log( ) ).

    add_aff_files_to_zip( files     = example_files
                          filename  = |{ aff_object-object_type }/examples/| ).


    DATA intf_objects TYPE if_aff_object_file_handler=>tt_objects.
    DATA(interfaces) = get_sub_type_interfaces( aff_object ).

    LOOP AT interfaces ASSIGNING FIELD-SYMBOL(<interface>).
      SELECT SINGLE devclass FROM tadir WHERE obj_name = @<interface> AND pgmid = 'R3TR' AND object = 'INTF' INTO @DATA(intf_obj_devclass).

      IF intf_obj_devclass IS INITIAL.
        INSERT |{ <interface> } is not found in table tadir. Package of the interface is unknown| INTO TABLE report_log ##NO_TEXT.
      ENDIF.

      APPEND VALUE #( devclass = intf_obj_devclass obj_type = 'INTF' obj_name = <interface> ) TO intf_objects.
    ENDLOOP.

    DATA(intf_files) = file_handler->serialize_objects( objects = intf_objects log = NEW cl_aff_log( ) ).

    add_aff_files_to_zip( files     = intf_files
                          filename  = |{ aff_object-object_type }/type/| ).

    LOOP AT interfaces ASSIGNING <interface>.

      DATA(found) = abap_false.
      SELECT SINGLE @abap_true FROM tadir WHERE obj_name = @<interface> INTO @found. "#EC CI_GENBUFF

      IF found = abap_false.
        INSERT |The schema for interface { <interface> } could not be created.| INTO TABLE report_log ##NO_TEXT.
        CONTINUE.
      ENDIF.

      DATA(object_type_path) = get_object_type_path( <interface> ).
      DATA(schemid) = |https://github.com/SAP/abap-file-formats/blob/main/file-formats/{ object_type_path }-v{ aff_object-format_version }.json| ##NO_TEXT.
      DATA(writer) = NEW zcl_aff_writer_json_schema( schema_id = schemid format_version = aff_object-format_version ).

      DATA(str_table) = get_content( absolute_typename  = |\\INTERFACE={ to_upper( <interface> ) }\\TYPE=TY_MAIN|
                                     interfacename      = <interface>
                                     generator          = NEW zcl_aff_generator( writer ) ).
      me->zip->add( name    = |{ aff_object-object_type }/{ to_lower( aff_object-object_type ) }-v{ aff_object-format_version }.json|
                    content = cl_abap_codepage=>convert_to( concat_lines_of( table = str_table sep = cl_abap_char_utilities=>newline ) ) ).
    ENDLOOP.




    DATA(readme) = VALUE string_table(
( |# { aff_object-object_type } File Format| )
( `` )
( `File | Cardinality | Definition | Schema | Example`)
( `:--- | :---  | :--- | :--- | :---` )
( |`<name>.{ aff_object-object_type }.json` \| 1 \| [`{ aff_object-interface }.intf.abap`](./type/{ aff_object-interface }.intf.abap) \| | &&
 | [`{ to_lower( aff_object-object_type ) }-v{ aff_object-format_version }.json`](./{ to_lower( aff_object-object_type ) }-v{ aff_object-format_version }.json)| &&
| \| [`{ aff_object-example }.{ aff_object-object_type }.json`](./examples/{ aff_object-example }.{ aff_object-object_type }.json)| )
 ( `` )
 ).
    me->zip->add( name    = |{ aff_object-object_type }/README.md|
                  content = cl_abap_codepage=>convert_to( concat_lines_of( table = readme sep = cl_abap_char_utilities=>newline ) ) ).

  ENDMETHOD.



  METHOD add_aff_files_to_zip.
    LOOP AT files-files ASSIGNING FIELD-SYMBOL(<file>).
      zip->add( name    = |{ to_lower( filename ) }{ to_lower( <file>-file_name ) }|
                content = <file>-content ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_object_infos_in_ui.
    DATA dynpfields TYPE STANDARD TABLE OF dynpread.

    DATA(intf_name) = i_object-interface.
    APPEND VALUE #( fieldname = 'P_INTF'  fieldvalue = intf_name ) TO dynpfields.

    DATA(example_name) = i_object-example.
    APPEND VALUE #( fieldname = 'P_EXAMP'  fieldvalue = example_name ) TO dynpfields.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = dynpfields.

  ENDMETHOD.


  METHOD get_object_infos_by_intfname.
    SPLIT intfname AT '_' INTO TABLE DATA(splitted_intfname).
    DATA(objecttype) = splitted_intfname[ 3 ].
    DATA(format_version) = get_format_version( to_upper( intfname ) ).
    object = VALUE #( object_type = to_upper( objecttype )
                      interface = to_upper( intfname )
                      format_version = format_version ).
  ENDMETHOD.


  METHOD get_dynpro_value.
    DATA dynpfields TYPE STANDARD TABLE OF dynpread.
    APPEND VALUE #( fieldname = to_upper( fieldname ) ) TO dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = dynpfields
      EXCEPTIONS
        OTHERS     = 1.

    IF sy-subrc = 0.
      fieldvalue = dynpfields[ 1 ]-fieldvalue.
    ELSE.
      CLEAR fieldvalue.
    ENDIF.

  ENDMETHOD.

  METHOD set_value_help_result_to_field.
    DATA: i_f4_result TYPE STANDARD TABLE OF ddshretval.
    DATA: w_f4_result TYPE ddshretval.
    DATA dynpfields TYPE STANDARD TABLE OF dynpread.
    IF value_help_result_table IS NOT INITIAL.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'P_PROG'
          value_org       = 'S'
        TABLES
          value_tab       = value_help_result_table
          return_tab      = i_f4_result
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.

      IF sy-subrc = 0.
        READ TABLE i_f4_result INTO w_f4_result INDEX 1.

        IF sy-subrc = 0.
          chosen_value = w_f4_result-fieldval.
          APPEND VALUE #( fieldname = fieldname  fieldvalue = w_f4_result-fieldval ) TO dynpfields.
          CALL FUNCTION 'DYNP_VALUES_UPDATE'
            EXPORTING
              dyname     = sy-repid
              dynumb     = sy-dynnr
            TABLES
              dynpfields = dynpfields.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD create_the_variable_dynamicaly.
    DATA r_typedescr TYPE REF TO cl_abap_typedescr.
    DATA r_elemdescr TYPE REF TO cl_abap_structdescr.
    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name = absolute_typename RECEIVING p_descr_ref = r_typedescr EXCEPTIONS type_not_found = 1 ).
    IF sy-subrc = 1.
      DATA(class_typename) = replace( val = absolute_typename sub = '\INTERFACE=' with = '\CLASS=' ).
      cl_abap_typedescr=>describe_by_name( EXPORTING  p_name = class_typename RECEIVING p_descr_ref = r_typedescr EXCEPTIONS type_not_found = 1 ).
      IF sy-subrc = 1.
        INSERT |Type { absolute_typename } was not found. Either interface or type doesnt exist.| INTO TABLE report_log ##NO_TEXT.
        RAISE EXCEPTION NEW zcx_aff_tools( ).
      ENDIF.
    ENDIF.
    r_elemdescr ?= r_typedescr.
    DATA r_field TYPE REF TO data.
    FIELD-SYMBOLS <field> TYPE any.
    CREATE DATA r_field TYPE HANDLE r_elemdescr.
    ASSIGN r_field->* TO <field>.
    GET REFERENCE OF <field> INTO variable.
  ENDMETHOD.


  METHOD get_format_version.
    format_version = 1.

    SPLIT intfname  AT '_' INTO TABLE DATA(splitted_intfname).
    DATA(last) = splitted_intfname[ lines( splitted_intfname ) ].
    REPLACE ALL OCCURRENCES OF 'V' IN last WITH ''.
    TRY.
        DATA(regx) = '[[:alpha:]]+'.
        DATA(contains_chars) = xsdbool( count( val = last regex = regx ) > 0 ) ##REGEX_POSIX.
        IF contains_chars = abap_false.
          format_version = last.
        ELSE.
          INSERT |Formatversion couldnt be derived from interface { intfname }. Format version 1 was assumed.| INTO TABLE report_log ##NO_TEXT.
        ENDIF.
      CATCH cx_sy_conversion_no_number.
        INSERT |Formatversion couldnt be derived from interface { intfname }. Format version 1 was assumed.| INTO TABLE report_log ##NO_TEXT.
    ENDTRY.
  ENDMETHOD.

  METHOD get_object_type_path.
    SPLIT interface_name  AT '_' INTO TABLE DATA(splitted_intfname).
    DATA(object_type) = splitted_intfname[ lines( splitted_intfname ) - 1 ].
    DATA(main_object_type) = object_type.

    IF object_type = 'REPS' OR object_type = 'FUNC'.
      main_object_type = 'FUGR'.
    ELSEIF object_type = 'INDX'.
      main_object_type = 'TABL'.
    ENDIF.

    path = |{ to_lower( main_object_type ) }/{ to_lower( object_type ) }|.
  ENDMETHOD.


  METHOD get_content.
    TRY.
        DATA(type) = create_the_variable_dynamicaly( absolute_typename ).
      CATCH cx_root.
        CLEAR result.
        RETURN.
    ENDTRY.

    FIELD-SYMBOLS <field> TYPE any.
    ASSIGN type->* TO <field>.
    TRY.
        result = generator->generate_type( <field> ).
      CATCH cx_root.
        CLEAR result.
        INSERT |The generator couldn't generate the schema/XSLT for type { absolute_typename }| INTO TABLE report_log ##NO_TEXT.
        RETURN.
    ENDTRY.

    LOOP AT generator->get_log( )->get_messages( ) ASSIGNING FIELD-SYMBOL(<msg>).
      IF <msg>-type = zif_aff_log=>c_message_type-info.
        log->add_info( message = <msg>-message component_name = interfacename ).
      ELSEIF <msg>-type = zif_aff_log=>c_message_type-warning.
        log->add_warning( message = <msg>-message component_name = interfacename ).
      ELSEIF <msg>-type = zif_aff_log=>c_message_type-error.
        log->add_error( message = <msg>-message component_name = interfacename ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.



  METHOD print_logs.
    IF lines( report_log ) > 0.
      WRITE: / `Messages of the report` ##NO_TEXT.
      LOOP AT report_log ASSIGNING FIELD-SYMBOL(<log_msg>).
        WRITE: / <log_msg>.
      ENDLOOP.
    ENDIF.

    IF lines( log->get_messages( ) ) > 0.
      SKIP 1.
      WRITE: / `Messages of the AFF Object Handlers and schema/ST Generator` ##NO_TEXT.
      LOOP AT log->get_messages( ) ASSIGNING FIELD-SYMBOL(<log_message>).
        IF NOT ( <log_message>-message-msgid = 'SAFF_CORE' AND
        ( <log_message>-message-msgno = '026' ) OR
        ( <log_message>-message-msgno = '027' ) ).
          DATA obj TYPE if_aff_object_file_handler=>ty_object.
          MOVE-CORRESPONDING <log_message> TO obj.
          WRITE: / |{ object_as_string( obj ) } { <log_message>-type }|.
          SPLIT <log_message>-text AT space INTO TABLE DATA(splitted).
          LOOP AT splitted ASSIGNING FIELD-SYMBOL(<word>).
            WRITE: <word>.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD object_as_string.
    DATA(objname) = |{ object-obj_name WIDTH = 30 ALIGN = LEFT PAD = ' ' }| ##NUMBER_OK.
    DATA(objtype) = |{ object-obj_type WIDTH = 4 ALIGN = LEFT PAD = ' ' }|.
    result = |{ objname } { objtype }|.

    IF object-sub_name IS NOT INITIAL AND object-sub_type IS NOT INITIAL.
      DATA(subname) = |{ object-sub_name WIDTH = 50 ALIGN = LEFT PAD = ' ' }| ##NUMBER_OK.
      DATA(subtype) = |{ object-sub_type WIDTH = 4 ALIGN = LEFT PAD = ' ' }|.
      result = |{ objname } { objtype } { subname } { subtype }|.
    ENDIF.
  ENDMETHOD.


  METHOD start_of_selection.

    aff_object = get_object_infos_by_intfname( CONV #( p_intf ) ).
    aff_object-example = p_examp.

    generate_repo_folder( ).

    DATA(zip_archive) = zip->save( ).
    DATA(zipname) = to_lower( aff_object-object_type ).
    write_to_zip( zip_archive = zip_archive zipname = zipname ).

  ENDMETHOD.



  METHOD on_value_request_for_example.
    DATA(example_value) = get_dynpro_value( fieldname  = `P_EXAMP` ).
    example_value = to_upper( example_value ).

    DATA(objtype_value) = get_dynpro_value( fieldname  = 'P_OBJTYP' ).
    objtype_value = to_upper( objtype_value ).

    IF example_value IS INITIAL AND objtype_value IS NOT INITIAL.
      SELECT obj_name FROM tadir WHERE object = @objtype_value AND devclass = 'TEST_AFF_EXAMPLES' INTO TABLE @DATA(value_help_result_table) UP TO 50 ROWS BYPASSING BUFFER ##NUMBER_OK. "#EC CI_NOORDER
    ELSEIF example_value IS NOT INITIAL AND objtype_value IS NOT INITIAL.
*  both are filled
* The user does not have to type "*" on beginning and ending of the obj_name pattern, we add it automatically
      DATA(example_with_percent) = |%{ to_upper( example_value ) }%|.
      REPLACE ALL OCCURRENCES OF '*' IN example_with_percent WITH `%`.

      " Retrieve object names from tadir which match the search pattern entered in UI Element obj_name
      SELECT obj_name FROM tadir WHERE object = @objtype_value AND obj_name LIKE @example_with_percent INTO TABLE @value_help_result_table UP TO 50 ROWS BYPASSING BUFFER ##NUMBER_OK. "#EC CI_NOORDER                         "#EC CI_NOORDER
    ELSE.
*  both are initial
      SELECT obj_name FROM tadir WHERE devclass = 'TEST_AFF_EXAMPLES' INTO TABLE @value_help_result_table UP TO 50 ROWS BYPASSING BUFFER ##NUMBER_OK. "#EC CI_NOORDER
    ENDIF.

    DATA(example1) = set_value_help_result_to_field( fieldname = `P_EXAMP` value_help_result_table = value_help_result_table ).
*    IF example1 IS NOT INITIAL.
*      DATA(object_infos) = get_object_infos_by_exmplname( example1 ).
*
*      set_object_infos_in_ui( object_infos ).
*      p_intf = object_infos-interface.
*      p_objtyp = object_infos-object_type.
*      p_examp = object_infos-example.
*    ENDIF.

  ENDMETHOD.

  METHOD modify_screen.
    TYPES: screen_name  TYPE c LENGTH 132,
           screen_names TYPE STANDARD TABLE OF screen_name.
    DATA hidden_elements TYPE screen_names.
    CLEAR hidden_elements.

    LOOP AT SCREEN.
      DATA(element_name) = screen-name.
*        %_UI_INPUT_%_APP_%-TEXT
      REPLACE ALL OCCURRENCES OF '%_' IN element_name WITH ``.
      REPLACE ALL OCCURRENCES OF '_APP_%-TEXT' IN element_name WITH ``.
      REPLACE ALL OCCURRENCES OF '_APP_%-OPTI_PUSH' IN element_name WITH ``.
      REPLACE ALL OCCURRENCES OF '_APP_%-VALU_PUSH' IN element_name WITH ``.
      REPLACE ALL OCCURRENCES OF '-LOW' IN element_name WITH ``.

      FIND FIRST OCCURRENCE OF REGEX element_name IN TABLE hidden_elements ##REGEX_POSIX. "or line exists
      IF sy-subrc = 0.
        screen-active = 0.
        MODIFY SCREEN.
      ELSE.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD on_value_request_for_intfname.
    DATA(intfname_value) = get_dynpro_value( fieldname  = `P_INTF` ).
    intfname_value = to_upper( intfname_value ).

    IF intfname_value IS INITIAL.
* The user does not have to type "*" on beginning and ending of the obj_name pattern, we add it automatically
      DATA(intfname_with_percent) = |%{ to_upper( intfname_value ) }%|.
      REPLACE ALL OCCURRENCES OF '*' IN intfname_with_percent WITH `%`.

      " Retrieve object names from tadir which match the search pattern entered in UI Element obj_name
      SELECT obj_name FROM tadir INTO TABLE @DATA(value_help_result_table) UP TO 30 ROWS BYPASSING BUFFER
      WHERE object = `INTF` AND obj_name LIKE @intfname_with_percent ORDER BY obj_name ##NUMBER_OK. "#EC CI_NOORDER
    ENDIF.

    DATA(intfname1) = set_value_help_result_to_field( fieldname = `P_INTF` value_help_result_table = value_help_result_table ).

    IF intfname1 IS NOT INITIAL.
      DATA(object_infos) = get_object_infos_by_intfname( intfname1 ).

      set_object_infos_in_ui( object_infos ).
    ENDIF.
  ENDMETHOD.

  METHOD at_selection_screen.
  ENDMETHOD.

  METHOD constructor.
    me->zip = NEW cl_abap_zip( ).
    me->log = NEW zcl_aff_log( ).
  ENDMETHOD.


ENDCLASS.



INITIALIZATION.

  helper = NEW lcl_generator( ).
  helper->modify_screen( ).


* when enter or F8 is pressed in the screen
AT SELECTION-SCREEN.
  helper->at_selection_screen( ).


AT SELECTION-SCREEN OUTPUT.


START-OF-SELECTION.

  helper->start_of_selection( ).
  helper->print_logs( ).

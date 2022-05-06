*&---------------------------------------------------------------------*
*& Report saff_generate_repo
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
    DATA  zip TYPE REF TO cl_abap_zip.
    DATA  aff_object  TYPE aff_object.

    METHODS: set_parameters
      IMPORTING
        i_intf  TYPE sobj_name OPTIONAL
        i_examp TYPE sobj_name OPTIONAL,

      start_of_selection,
      at_selection_screen,
      write_to_zip
        IMPORTING zip_archive TYPE xstring
                  zipname     TYPE string,
      print_logs.

  PRIVATE SECTION.

    DATA: "needed for testing
      aff_factory TYPE REF TO  if_aff_factory,
      generator   TYPE REF TO zcl_aff_generator,
      writer      TYPE REF TO zif_aff_writer.

    METHODS: get_sub_type_interfaces
      IMPORTING object        TYPE aff_object
      RETURNING VALUE(result) TYPE string_table,
      add_aff_files_to_zip
        IMPORTING
          files    TYPE any
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

    DATA(lower_obj_type) = to_lower( aff_object-object_type ).
    DATA(lower_interface) = to_lower( aff_object-interface ).
    DATA(lower_example) = to_lower( aff_object-example ).

    TRY.
*       file_handler = cl_aff_factory=>get_object_file_handler( ).
        DATA aff_factory TYPE REF TO object.
        DATA file_handler TYPE REF TO object.
        DATA log TYPE REF TO object.
        CREATE OBJECT aff_factory TYPE ('CL_AFF_FACTORY').
*        CREATE OBJECT file_handler TYPE ('IF_AFF_OBJECT_FILE_HANDLER').
        CREATE OBJECT log TYPE ('CL_AFF_LOG').

        CALL METHOD aff_factory->('GET_OBJECT_FILE_HANDLER')
          RECEIVING
            result = file_handler.

        SELECT SINGLE devclass FROM tadir WHERE pgmid = 'R3TR' AND obj_name = @aff_object-example AND object = @aff_object-object_type INTO @DATA(example_obj_devclass).
*        data(example_main_object) = value if_aff_object_file_handler=>ty_object( devclass  = example_obj_devclass obj_type = <object>-object_type obj_name = <object>-example ).
        DATA: example_main_object TYPE REF TO data.
        FIELD-SYMBOLS: <example_main_object> TYPE any.
        CREATE DATA example_main_object TYPE ('IF_AFF_OBJECT_FILE_HANDLER=>TY_OBJECT').
        ASSIGN example_main_object->* TO <example_main_object>.
        ASSIGN COMPONENT 'DEVCLASS' OF STRUCTURE <example_main_object> TO FIELD-SYMBOL(<devclass>).
        ASSIGN COMPONENT 'OBJ_TYPE' OF STRUCTURE <example_main_object> TO FIELD-SYMBOL(<obj_type>).
        ASSIGN COMPONENT 'OBJ_NAME' OF STRUCTURE <example_main_object> TO FIELD-SYMBOL(<obj_name>).
        <devclass> = example_obj_devclass.
        <obj_type> = aff_object-object_type.
        <obj_name> = aff_object-example.

*    DATA(example_files) = file_handler->serialize_objects( objects = VALUE #( ( example_main_object ) ) log = NEW cl_aff_log( ) ).
        DATA: objects TYPE REF TO data.
        FIELD-SYMBOLS: <objects> TYPE any.
        CREATE DATA objects TYPE ('IF_AFF_OBJECT_FILE_HANDLER=>TT_OBJECTS').
        ASSIGN objects->* TO <objects>.
        INSERT <example_main_object> INTO TABLE <objects>.

        DATA: files TYPE REF TO data.
        CREATE DATA files TYPE ('IF_AFF_OBJECT_FILE_HANDLER=>TY_OBJECT_FILES').
*        data(file_handler1) = cast IF_AFF_OBJECT_FILE_HANDLER( file_handler ).
        CALL METHOD file_handler->('SERIALIZE_OBJECTS')
          EXPORTING
            objects = objects->*
            log     = log
          RECEIVING
            result  = files.

        add_aff_files_to_zip( files = files
                              filename = |{ lower_obj_type }/examples/| ).

      CATCH cx_root INTO DATA(lx_error).
        INSERT |Files for the Example object could not be generated.| INTO TABLE report_log ##NO_TEXT.

    ENDTRY.
    TRY.
        DATA(interfaces) = get_sub_type_interfaces( aff_object ).
        CREATE DATA objects TYPE ('IF_AFF_OBJECT_FILE_HANDLER=>TT_OBJECTS').
        ASSIGN objects->* TO <objects>.
        LOOP AT interfaces ASSIGNING FIELD-SYMBOL(<interface>).
          SELECT SINGLE devclass FROM tadir WHERE obj_name = @<interface> AND pgmid = 'R3TR' AND object = 'INTF' INTO @DATA(intf_obj_devclass).

          IF intf_obj_devclass IS INITIAL.
            INSERT |{ <interface> } is not found in table tadir. Package of the interface is unknown| INTO TABLE report_log ##NO_TEXT.
          ENDIF.
          DATA: interface_object TYPE REF TO data.
          FIELD-SYMBOLS: <interface_object> TYPE any.
          CREATE DATA interface_object TYPE ('IF_AFF_OBJECT_FILE_HANDLER=>TY_OBJECT').
          ASSIGN interface_object->* TO <interface_object>.
          ASSIGN COMPONENT 'DEVCLASS' OF STRUCTURE <interface_object> TO <devclass>.
          ASSIGN COMPONENT 'OBJ_TYPE' OF STRUCTURE <interface_object> TO <obj_type>.
          ASSIGN COMPONENT 'OBJ_NAME' OF STRUCTURE <interface_object> TO <obj_name>.
          <devclass> = intf_obj_devclass.
          <obj_type> = 'INTF'.
          <obj_name> = <interface>.
          INSERT <interface_object> INTO TABLE <objects>.
*      APPEND VALUE #( devclass = intf_obj_devclass obj_type = 'INTF' obj_name = <interface> ) TO intf_objects.
        ENDLOOP.

        CALL METHOD file_handler->('IF_AFF_OBJECT_FILE_HANDLER~SERIALIZE_OBJECTS')
          EXPORTING
            objects = objects->*
            log     = log
          RECEIVING
            result  = files.

*    DATA(intf_files) = file_handler->serialize_objects( objects = intf_objects log = NEW cl_aff_log( ) ).

        add_aff_files_to_zip( files = files
                              filename = |{ lower_obj_type }/type/| ).
      CATCH cx_root INTO DATA(lx_error2).
        INSERT |Files for the interfaces could not be generated.| INTO TABLE report_log ##NO_TEXT.

    ENDTRY.

    LOOP AT interfaces ASSIGNING <interface>.

      DATA(found) = abap_false.
      SELECT SINGLE @abap_true FROM tadir WHERE obj_name = @<interface> INTO @found. "#EC CI_GENBUFF

      IF found = abap_false.
        INSERT |The schema for interface { <interface> } could not be created.| INTO TABLE report_log ##NO_TEXT.
        CONTINUE.
      ENDIF.

      DATA(object_type_path) = get_object_type_path( <interface> ).
      DATA(schemid) = |https://github.com/SAP/abap-file-formats/blob/main/file-formats/{ object_type_path }-v{ aff_object-format_version }.json| ##NO_TEXT.
      writer = NEW zcl_aff_writer_json_schema( schema_id = schemid format_version = aff_object-format_version ).
      generator = NEW zcl_aff_generator( writer ).

      DATA(str_table) = get_content( absolute_typename = |\\INTERFACE={ to_upper( <interface> ) }\\TYPE=TY_MAIN| interfacename = <interface> ).
      me->zip->add( name    = |{ lower_obj_type }/{ lower_obj_type }-v{ aff_object-format_version }.json|
                    content = cl_abap_codepage=>convert_to( concat_lines_of( table = str_table sep = cl_abap_char_utilities=>newline ) ) ).
    ENDLOOP.



    DATA(readme) = VALUE string_table(
( |# { aff_object-object_type } File Format| )
( `` )
( `File | Cardinality | Definition | Schema | Example`)
( `:--- | :---  | :--- | :--- | :---` )
( |`<name>.{ lower_obj_type }.json` \| 1 \| [`{ lower_interface }.intf.abap`](./type/{ lower_interface }.intf.abap) \| | &&
 | [`{ lower_obj_type }-v{ aff_object-format_version }.json`](./{ lower_obj_type }-v{ aff_object-format_version }.json)| &&
| \| [`{ lower_example }.{ lower_obj_type }.json`](./examples/{ lower_example }.{ lower_obj_type }.json)| )
 ( `` )
 ).
    me->zip->add( name    = |{ lower_obj_type }/README.md|
                  content = cl_abap_codepage=>convert_to( concat_lines_of( table = readme sep = cl_abap_char_utilities=>newline ) ) ).

  ENDMETHOD.



  METHOD add_aff_files_to_zip.
    FIELD-SYMBOLS <files> TYPE any.
    ASSIGN COMPONENT 'FILES' OF STRUCTURE files TO <files>.

    LOOP AT <files> ASSIGNING FIELD-SYMBOL(<file>).
      ASSIGN COMPONENT 'FILE_NAME' OF STRUCTURE <file> TO FIELD-SYMBOL(<file_name>).
      ASSIGN COMPONENT 'CONTENT' OF STRUCTURE <file> TO FIELD-SYMBOL(<content>).
      zip->add( name    = |{ filename }{ to_lower( <file_name> ) }|
                content = <content> ).
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

    me->zip = NEW cl_abap_zip( ).
    me->log = NEW zcL_aff_log( ).

    aff_object = get_object_infos_by_intfname( CONV #( p_intf ) ).
    aff_object-example = p_examp.

    generate_repo_folder( ).

  ENDMETHOD.

  METHOD at_selection_screen.
  ENDMETHOD.


  METHOD set_parameters.
    p_intf  = i_intf.
    p_examp = i_examp.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_generator DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_generator.
    METHODS chkc FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_generator IMPLEMENTATION.

  METHOD chkc.
    cut = NEW lcl_generator( ).

    SELECT SINGLE @abap_true FROM tadir WHERE obj_name = 'IF_AFF_CHKC_V1'  INTO @DATA(intf_found).
    SELECT SINGLE @abap_true FROM tadir WHERE obj_name = 'AFF_EXAMPLE_CHKC' INTO @DATA(example_found).
    IF intf_found = abap_false OR example_found = abap_false.
      cl_abap_unit_assert=>skip( 'CHKC is not available in this system' ).
    ENDIF.

    cut->set_parameters(
      i_intf  = 'IF_AFF_CHKC_V1'
      i_examp = 'AFF_EXAMPLE_CHKC'
    ).

    cut->start_of_selection( ).


    DATA(actual_log) = cut->log.
    DATA(actual_report_log) = cut->report_log.
    DATA(actual_zip) = cut->zip.

    DATA(expected_files_in_zip) = VALUE stringtab(
     ( `chkc/examples/aff_example_chkc.chkc.json` )
     ( `chkc/type/if_aff_chkc_v1.intf.abap` )
     ( `chkc/type/if_aff_chkc_v1.intf.json` )
     ( `chkc/chkc-v1.json` )
     ( `chkc/README.md` )
     ).

    LOOP AT expected_files_in_zip ASSIGNING FIELD-SYMBOL(<filename>).
      actual_zip->get( EXPORTING name  = <filename> IMPORTING content  = DATA(act_content) ).
    ENDLOOP.

    cl_abap_unit_assert=>assert_false( act = actual_log->has_messages( ) ).
    cl_abap_unit_assert=>assert_initial( act = actual_report_log ).

  ENDMETHOD.


ENDCLASS.


INITIALIZATION.

  helper = NEW lcl_generator( ).

* when enter or F8 is pressed in the  screen
AT SELECTION-SCREEN.
  helper->at_selection_screen( ).


AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.

  helper->start_of_selection( ).

  DATA(zip_archive) = helper->zip->save( ).
  DATA(zipname) = to_lower( helper->aff_object-object_type ).
  helper->write_to_zip( zip_archive = zip_archive zipname = zipname ).
  helper->print_logs( ).

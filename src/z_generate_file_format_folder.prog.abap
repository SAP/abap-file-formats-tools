*&---------------------------------------------------------------------*
*& Report saff_generate_repo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_generate_file_format_folder.
CLASS lcl_generator DEFINITION DEFERRED.
DATA obj_types TYPE saff_repo_types.
DATA helper TYPE REF TO lcl_generator ##NEEDED.

INTERFACE lif_gui_frontend_service.
  METHODS display.
  METHODS clear.
  METHODS write
    IMPORTING message TYPE any.
  METHODS write_to_console_and_screen
    IMPORTING message TYPE string.
  METHODS file_save_dialog
    IMPORTING VALUE(window_title)      TYPE string OPTIONAL
              VALUE(default_extension) TYPE string OPTIONAL
              VALUE(default_file_name) TYPE string OPTIONAL
    CHANGING  filename                 TYPE string
              path                     TYPE string
              fullpath                 TYPE string
              user_action              TYPE i OPTIONAL
    RAISING   cx_root.
  METHODS gui_download
    IMPORTING
              !bin_filesize     TYPE i OPTIONAL
              !filename         TYPE string
              !filetype         TYPE char10 DEFAULT 'ASC'
              !write_lf         TYPE char01 DEFAULT 'X'
    EXPORTING
              VALUE(filelength) TYPE i
    CHANGING
              !data_tab         TYPE STANDARD TABLE
    RAISING   cx_root.

ENDINTERFACE.

CLASS lcl_gui_frontend DEFINITION
  FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_gui_frontend_service.
ENDCLASS.

CLASS lcl_gui_frontend IMPLEMENTATION.


  METHOD lif_gui_frontend_service~file_save_dialog.
    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        default_extension   = default_extension
        default_file_name   = default_file_name
      CHANGING
        filename            = filename
        path                = path
        fullpath            = fullpath
        user_action         = user_action
                              ) ##SUBRC_OK.
  ENDMETHOD.

  METHOD lif_gui_frontend_service~gui_download.
    CLEAR filelength.
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                  = filename
        bin_filesize              = bin_filesize
        filetype                  = filetype
        write_lf                  = write_lf
      CHANGING
        data_tab                  = data_tab ).
  ENDMETHOD.

  METHOD lif_gui_frontend_service~write_to_console_and_screen.
    WRITE: / message.
    cl_demo_output=>write( message ).
  ENDMETHOD.

  METHOD lif_gui_frontend_service~clear.
    cl_demo_output=>clear( ).
  ENDMETHOD.

  METHOD lif_gui_frontend_service~display.
    cl_demo_output=>display( ).
  ENDMETHOD.

  METHOD lif_gui_frontend_service~write.
    cl_demo_output=>write( message ).
  ENDMETHOD.

ENDCLASS.




INTERFACE lif_generator.
  METHODS generate_type
    IMPORTING data          TYPE data
    RETURNING VALUE(result) TYPE string_table
    RAISING   cx_root.
  METHODS get_log
    RETURNING
      VALUE(log) TYPE REF TO zif_aff_log.
ENDINTERFACE.

CLASS lcl_generator_helper DEFINITION
  FINAL
  CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_generator.
    METHODS constructor
      IMPORTING
        writer TYPE REF TO zif_aff_writer.
    DATA generator TYPE REF TO zcl_aff_generator.
ENDCLASS.

CLASS lcl_generator_helper IMPLEMENTATION.

  METHOD constructor.
    me->generator = NEW zcl_aff_generator( writer ).
  ENDMETHOD.

  METHOD lif_generator~generate_type.
    result = me->generator->generate_type( data ).
  ENDMETHOD.

  METHOD lif_generator~get_log.
    log = me->generator->get_log( ).
  ENDMETHOD.

ENDCLASS.


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
    DATA schema_test_content TYPE string_table.
    DATA zip TYPE REF TO cl_abap_zip.
    DATA gui_frontend_service TYPE REF TO lif_gui_frontend_service.

    METHODS: set_parameters
      IMPORTING
        i_intf  TYPE sobj_name OPTIONAL
        i_examp TYPE sobj_name OPTIONAL,

      constructor
        IMPORTING
          i_gui_frontend TYPE REF TO lif_gui_frontend_service OPTIONAL
          aff_factory    TYPE REF TO if_aff_factory OPTIONAL
          generator      TYPE REF TO lif_generator OPTIONAL
          writer         TYPE REF TO zif_aff_writer OPTIONAL,
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

    DATA: "needed for testing
      aff_factory TYPE REF TO  if_aff_factory,
      generator   TYPE REF TO lif_generator,
      writer      TYPE REF TO zif_aff_writer.

    METHODS: get_all_interfaces
      IMPORTING object        TYPE aff_object
      RETURNING VALUE(result) TYPE string_table,
      add_aff_files_to_zip
        IMPORTING
          files    TYPE if_aff_object_file_handler=>ty_object_files
          filename TYPE string,
      generate_repo_folder
        IMPORTING object TYPE aff_object,
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
      get_object_infos_by_exmplname
        IMPORTING examplename   TYPE string
        RETURNING VALUE(object) TYPE aff_object,
      get_intfname_highest_version
        IMPORTING objecttype     TYPE string
        EXPORTING intfname       TYPE string
                  format_version TYPE i,
      get_format_version
        IMPORTING intfname              TYPE string
        RETURNING VALUE(format_version) TYPE i,
      get_object_type_path
        IMPORTING interface_name TYPE string
        RETURNING VALUE(path)    TYPE string,
      get_content
        IMPORTING absolute_typename TYPE string
                  interfacename     TYPE string
        RETURNING VALUE(content)    TYPE string_table,
      object_as_string
        IMPORTING object        TYPE if_aff_object_file_handler=>ty_object
        RETURNING VALUE(result) TYPE string,
      add_file_to_zip
        IMPORTING
          i_file_name         TYPE string
          i_stringtab_content TYPE string_table
          i_error_text        TYPE string.

ENDCLASS.

CLASS lcl_generator IMPLEMENTATION.

  METHOD write_to_zip.
    DATA file_name TYPE string.
    DATA path TYPE string.
    DATA fullpath TYPE string.
    DATA user_action TYPE i.
    TRY.
        gui_frontend_service->file_save_dialog(
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
        gui_frontend_service->gui_download(
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

  METHOD get_all_interfaces.

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

    me->zip = NEW cl_abap_zip( ).

    DATA(object_type_folder_name) = to_lower( object-object_type ).

    SELECT SINGLE devclass FROM tadir WHERE pgmid = 'R3TR' AND obj_name = @object-example AND object = @object-object_type INTO @DATA(example_obj_devclass).
    DATA(example_main_object) = VALUE if_aff_object_file_handler=>ty_object( devclass  = example_obj_devclass obj_type = object-object_type obj_name = object-example ).

    IF aff_factory IS NOT INITIAL.
      DATA(file_handler) = aff_factory->get_object_file_handler( ). " for testing purposes
    ELSE.
      file_handler = cl_aff_factory=>get_object_file_handler( ).
    ENDIF.

    DATA(example_files) = file_handler->serialize_objects( objects = VALUE #( ( example_main_object ) ) log = NEW cl_aff_log( ) ).

    add_aff_files_to_zip( files = example_files
                          filename = |{ object_type_folder_name }/examples/| ).


    DATA intf_objects TYPE if_aff_object_file_handler=>tt_objects.
    DATA(interfaces) = get_all_interfaces( object ).

    LOOP AT interfaces ASSIGNING FIELD-SYMBOL(<interface>).
      SELECT SINGLE devclass FROM tadir WHERE obj_name = @<interface> AND pgmid = 'R3TR' AND object = 'INTF' INTO @DATA(intf_obj_devclass).

      IF intf_obj_devclass IS INITIAL.
        INSERT |{ <interface> } is not found in table tadir. Package of the interface is unknown| INTO TABLE report_log ##NO_TEXT.
      ENDIF.

      APPEND VALUE #( devclass = intf_obj_devclass obj_type = 'INTF' obj_name = <interface> ) TO intf_objects.
    ENDLOOP.

    DATA(intf_files) = file_handler->serialize_objects( objects = intf_objects log = NEW cl_aff_log( ) ).

    add_aff_files_to_zip( files = intf_files
                          filename = |{ object_type_folder_name }/type/| ).

    LOOP AT interfaces ASSIGNING <interface>.

      DATA(found) = abap_false.
      SELECT SINGLE @abap_true FROM tadir WHERE obj_name = @<interface> INTO @found. "#EC CI_GENBUFF

      IF found = abap_false.
        INSERT |The schema for interface { <interface> } could not be created.| INTO TABLE report_log ##NO_TEXT.
        CONTINUE.
      ENDIF.

      DATA(aff_object) = get_object_infos_by_intfname( <interface> ).
      DATA(objecttype) = aff_object-object_type.
      DATA(format_version) = aff_object-format_version.

      DATA(object_type_path) = get_object_type_path( <interface> ).
      DATA(schemid) = |https://github.com/SAP/abap-file-formats/blob/main/file-formats/{ object_type_path }-v{ format_version }.json| ##NO_TEXT.

      IF writer IS INITIAL OR writer IS INSTANCE OF zcl_aff_writer_json_schema OR writer IS INSTANCE OF zcl_aff_writer_xslt. "in testcase the writer is of type zif_aff_writer
        writer = NEW zcl_aff_writer_json_schema( schema_id = schemid format_version = format_version ).
      ENDIF.

      IF generator IS INITIAL OR generator IS INSTANCE OF lcl_generator_helper. "in testcase we use ltc_generator
        generator = NEW lcl_generator_helper( writer ).
      ENDIF.

      DATA(schema_content) = get_content( absolute_typename = |\\INTERFACE={ to_upper( <interface> ) }\\TYPE=TY_MAIN| interfacename = <interface> ).

      IF schema_test_content IS NOT INITIAL."in test case sometimes a test schema content is injected
        schema_content = schema_test_content.
      ENDIF.

      IF schema_content IS INITIAL.
        INSERT |The schema for interface { <interface> } could not be created.| INTO TABLE report_log ##NO_TEXT.
      ELSE.
        add_file_to_zip( i_stringtab_content = schema_content
          i_file_name         = |{ object_type_folder_name }/{ to_lower( objecttype ) }-v{ format_version }.json|
          i_error_text        = |The schema for interface { <interface> } could not be created. Error when transforming schema content from string to xstring| ).
      ENDIF.
    ENDLOOP.

    DATA(readme) = VALUE string_table(
( |# { object-object_type } File Format| )
( `` )
( `File | Cardinality | Definition | Schema | Example`)
( `:--- | :---  | :--- | :--- | :---` )
( |`<name>.{ object_type_folder_name }.json` \| 1 \| [`{ object-interface }.intf.abap`](./type/{ object-interface }.intf.abap) \| | &&
 | [`{ to_lower( object-object_type ) }-v{ object-format_version }.json`](./{ to_lower( object-object_type ) }-v{ object-format_version }.json)| &&
| \| [`{ object-example }.{ object_type_folder_name }.json`](./examples/{ object-example }.{ object_type_folder_name }.json)| )
 ( `` )
 ).
    add_file_to_zip( i_stringtab_content  = readme
                     i_file_name          = |{ object_type_folder_name }/README.md|
                     i_error_text         = |The readme for object { object-object_type } could not be created. Error when transforming readme content from string to xstring| ) ##NO_TEXT.

  ENDMETHOD.

  METHOD add_file_to_zip.
    "convert the string_table to xstring
    DATA(text_handler) = NEW cl_aff_content_handler_text( ).
    TRY.
        DATA(xstring_content) = text_handler->if_aff_content_handler~serialize( i_stringtab_content ).
      CATCH cx_root.
        INSERT i_error_text INTO TABLE report_log.
        RETURN.
    ENDTRY.
    me->zip->add( name    = i_file_name
                content = xstring_content ).
  ENDMETHOD.

  METHOD add_aff_files_to_zip.
    LOOP AT files-files ASSIGNING FIELD-SYMBOL(<file>).
      zip->add( name    = |{ filename }{ to_lower( <file>-file_name ) }|
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

  METHOD get_intfname_highest_version.
    CLEAR intfname.
    CLEAR format_version.
    DATA(string_to_search) = |IF_AFF_{ objecttype  }_%|.
    SELECT obj_name FROM tadir WHERE object = 'INTF' AND obj_name LIKE @string_to_search INTO TABLE @DATA(intfs). "#EC CI_GENBUFF
* take the highest number
    DATA version_list TYPE STANDARD TABLE OF i.
    LOOP AT intfs ASSIGNING FIELD-SYMBOL(<intf>).
      DATA(intf_format_version) = get_format_version( CONV #( <intf> ) ).
      APPEND intf_format_version TO version_list.
    ENDLOOP.
    SORT version_list DESCENDING.
    IF lines( version_list ) > 0.
      format_version = version_list[ 1 ].
      string_to_search = |*{ version_list[ 1 ] }|.
      LOOP AT intfs ASSIGNING <intf> WHERE obj_name CP string_to_search.
        intfname = <intf>.
        EXIT.                                           "#EC CI_NOORDER
      ENDLOOP.
    ELSE.
      intfname = |IF_AFF_{ objecttype  }_V1|.
      format_version = 1.
    ENDIF.
  ENDMETHOD.



  METHOD get_object_infos_by_intfname.
    SPLIT intfname AT '_' INTO TABLE DATA(splitted_intfname).
    DATA(objecttype) = splitted_intfname[ 3 ].
    DATA(format_version) = get_format_version( to_upper( intfname ) ).
    object = VALUE #( object_type = to_upper( objecttype )
                      interface = to_upper( intfname )
                      format_version = format_version ).
  ENDMETHOD.

  METHOD get_object_infos_by_exmplname.
    IF examplename IS NOT INITIAL.
      SPLIT examplename AT '_' INTO TABLE DATA(splitted_examplename).
      DATA(objecttype) = to_upper( splitted_examplename[ lines( splitted_examplename ) ] ).
      IF objecttype IS NOT INITIAL.
        IF strlen( objecttype ) < 4.
          RETURN.
        ENDIF.
        get_intfname_highest_version(
          EXPORTING
            objecttype     = objecttype
          IMPORTING
            intfname       = DATA(intfname)
            format_version = DATA(format_version) ).
        IF objecttype = 'NROB'.
          intfname = 'Z_AFF_NR'.
        ENDIF.
      ENDIF.
    ENDIF.
    object = VALUE #( object_type = objecttype interface = intfname example = examplename format_version = format_version ).
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
        CLEAR content.
        RETURN.
    ENDTRY.

    FIELD-SYMBOLS <field> TYPE any.
    ASSIGN type->* TO <field>.
    TRY.
        content = generator->generate_type( <field> ).
      CATCH cx_root.
        CLEAR content.
        INSERT |The generator couldn't generate the schema/XSLT for type { absolute_typename }| INTO TABLE report_log ##NO_TEXT.
        RETURN.
    ENDTRY.

    DATA(generator_log) = NEW zcl_aff_log( ).
    LOOP AT generator->get_log( )->get_messages( ) ASSIGNING FIELD-SYMBOL(<msg>).
      IF <msg>-type = zif_aff_log=>c_message_type-info.
        generator_log->zif_aff_log~add_info( message = <msg>-message component_name = interfacename ).
      ELSEIF <msg>-type = zif_aff_log=>c_message_type-warning.
        generator_log->zif_aff_log~add_warning( message = <msg>-message component_name = interfacename ).
      ELSEIF <msg>-type = zif_aff_log=>c_message_type-error.
        generator_log->zif_aff_log~add_error( message = <msg>-message component_name = interfacename ).
      ENDIF.
    ENDLOOP.
    me->log->join( generator_log ).
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

    DATA(object) = get_object_infos_by_intfname( CONV #( p_intf ) ).
    object-example = p_examp.

    generate_repo_folder( object ).

    DATA(zip_archive) = zip->save( ).
    DATA(zipname) = to_lower( object-object_type ).
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
    IF generator IS SUPPLIED.
      me->generator = generator.
    ENDIF.
    IF writer IS SUPPLIED.
      me->writer = writer.
    ENDIF.
    IF aff_factory IS SUPPLIED.
      me->aff_factory = aff_factory.
    ENDIF.
    log = NEW zcl_aff_log( ).
    IF i_gui_frontend IS SUPPLIED.
      gui_frontend_service = i_gui_frontend.
    ELSE.
      gui_frontend_service = NEW lcl_gui_frontend( ).
    ENDIF.
  ENDMETHOD.

  METHOD set_parameters.
    p_intf  = i_intf.
    p_examp = i_examp.
  ENDMETHOD.

ENDCLASS.



INITIALIZATION.

  helper = NEW lcl_generator( ).
  helper->modify_screen( ).


* when enter or F8 is pressed in the  screen
AT SELECTION-SCREEN.
  helper->at_selection_screen( ).


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_intf.
  helper->on_value_request_for_intfname( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_examp.
  helper->on_value_request_for_example( ).

AT SELECTION-SCREEN OUTPUT.

  helper->modify_screen( ).

START-OF-SELECTION.

  helper->start_of_selection( ).
  helper->print_logs( ).
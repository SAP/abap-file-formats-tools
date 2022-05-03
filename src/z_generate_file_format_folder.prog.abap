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

CLASS ltc_gui_frontend DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif_gui_frontend_service.
ENDCLASS.
CLASS ltc_gui_frontend IMPLEMENTATION.

  METHOD lif_gui_frontend_service~file_save_dialog.
    IF default_extension         = `zip`.

      filename   = 'TESTFILENAME.zip'.
      path       = 'TESTPATH'.
      fullpath   = 'FULLPATH.zip'.
      user_action = 0.

    ENDIF.
  ENDMETHOD.

  METHOD lif_gui_frontend_service~gui_download.
    CLEAR filelength.
    IF filename = 'TESTFILENAME.zip' AND filetype = 'BIN' AND write_lf = space.
      data_tab = VALUE #( ).
      sy-subrc = 0.
    ELSE.
      data_tab = VALUE #( ).
      sy-subrc = 1.
    ENDIF.
  ENDMETHOD.

  METHOD lif_gui_frontend_service~write_to_console_and_screen ##NEEDED.

  ENDMETHOD.

  METHOD lif_gui_frontend_service~clear ##NEEDED.

  ENDMETHOD.

  METHOD lif_gui_frontend_service~display ##NEEDED.

  ENDMETHOD.

  METHOD lif_gui_frontend_service~write ##NEEDED.

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
        i_schema TYPE abap_bool DEFAULT abap_false
        i_xslt   TYPE abap_bool DEFAULT abap_false
        i_repo   TYPE abap_bool DEFAULT abap_false
        i_whole  TYPE abap_bool DEFAULT abap_false
        i_multre TYPE abap_bool DEFAULT abap_false
        i_objtyp TYPE trobjtype OPTIONAL
        i_intf   TYPE sobj_name OPTIONAL
        i_type   TYPE sobj_name OPTIONAL
        i_examp  TYPE sobj_name OPTIONAL
        i_consol TYPE abap_bool DEFAULT abap_false
        i_disk   TYPE abap_bool DEFAULT abap_false
        i_readm  TYPE abap_bool DEFAULT abap_true
        i_multob TYPE stringtab OPTIONAL,

      set_schema_test_content
        IMPORTING schema_test_content TYPE string_table,

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
      get_table_with_all_githubtypes
        RETURNING VALUE(type_table) TYPE stringtab,
      write_to_zip
        IMPORTING zip_archive TYPE xstring
                  zipname     TYPE string,
      print_logs.

  PRIVATE SECTION.

    TYPES: clsname_tab TYPE STANDARD TABLE OF seoclsname,
           BEGIN OF replacing_line,
             to_be_replaced TYPE string,
             replace_with   TYPE string,
           END OF replacing_line.

    TYPES: replacing_tab TYPE STANDARD TABLE OF replacing_line.

    DATA: "needed for testing
      aff_factory TYPE REF TO  if_aff_factory,
      generator   TYPE REF TO lif_generator,
      writer      TYPE REF TO zif_aff_writer.
    DATA replacing_table_string TYPE replacing_tab.

    METHODS: get_all_interfaces
        IMPORTING object        TYPE aff_object
        RETURNING VALUE(result) TYPE string_table,
      add_aff_files_to_zip
        IMPORTING
          files                  TYPE if_aff_object_file_handler=>ty_object_files
          filename               TYPE string,
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
      get_object_infos_by_objtype
        IMPORTING objecttype    TYPE string
        RETURNING VALUE(object) TYPE aff_object,
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
      get_objname_wo_namspace_with_z
        IMPORTING object_name   TYPE string
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

  METHOD get_objname_wo_namspace_with_z.
    SPLIT object_name AT '/' INTO TABLE DATA(splitted_obj_name_parts).
    DATA(object_name_wo_namespace) = splitted_obj_name_parts[ lines( splitted_obj_name_parts ) ].
    DATA(zname_of_obj) = to_lower( object_name_wo_namespace ).
    IF NOT zname_of_obj CP `z*`.
      result = |z{ zname_of_obj }|.
    ELSE.
      result = zname_of_obj.
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

      data(aff_object) = get_object_infos_by_intfname( <interface> ).
      DATA(objecttype) = aff_object-object_type.
      DATA(format_version) = aff_object-format_version.

      DATA(object_type_path) = get_object_type_path( CONV #( <interface> ) ).
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
    DATA(obj_type) = i_object-object_type.
    APPEND VALUE #( fieldname = 'P_OBJTYP'  fieldvalue = obj_type ) TO dynpfields.

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

  METHOD get_object_infos_by_objtype.
    get_intfname_highest_version(
      EXPORTING
        objecttype     = objecttype
      IMPORTING
        intfname       = DATA(intfname)
        format_version = DATA(format_version) ).
    IF objecttype = 'DOMA'.
      intfname = 'ZIF_AFF_DOMA_V1'.
    ENDIF.
    DATA(examplename) = |Z_AFF_EXAMPLE_{ objecttype }|.
    IF objecttype = 'NROB'.
      examplename = 'Z_AFF_NR'.
    ENDIF.
    object = VALUE #( object_type = objecttype interface = intfname example = examplename format_version = format_version ).
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

  METHOD get_table_with_all_githubtypes.
    APPEND 'CHKC' TO type_table.
    APPEND 'CHKO' TO type_table.
    APPEND 'CHKV' TO type_table.
    APPEND 'CLAS' TO type_table.
    APPEND 'DDLS' TO type_table.
    APPEND 'ENHO' TO type_table.
    APPEND 'ENHS' TO type_table.
    APPEND 'FUGR' TO type_table.
    APPEND 'INTF' TO type_table.
    APPEND 'NROB' TO type_table.
    APPEND 'DDLX' TO type_table.
    APPEND 'DOMA' TO type_table.
  ENDMETHOD.

  METHOD get_format_version.
    SPLIT intfname  AT '_' INTO TABLE DATA(splitted_intfname).
    DATA(last) = splitted_intfname[ lines( splitted_intfname ) ].
    REPLACE ALL OCCURRENCES OF 'V' IN last WITH ''.
    TRY.
        DATA(regx) = '[[:alpha:]]+'.
        " check if the token only contains digits
        DATA(contains_chars) = xsdbool( count( val = last regex = regx ) > 0 ) ##REGEX_POSIX.
        IF contains_chars = abap_false.
          format_version = last.
        ELSE.
          INSERT |Formatversion couldnt be derived from interface { intfname }. Format version 1 was assumed.| INTO TABLE report_log ##NO_TEXT.
          format_version = 1.
        ENDIF.
      CATCH cx_sy_conversion_no_number.
        " if the intfname is not correct we use format_version 1
        INSERT |Formatversion couldnt be derived from interface { intfname }. Format version 1 was assumed.| INTO TABLE report_log ##NO_TEXT.
        format_version = 1.
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

    IF NOT writer->validate( source = content log = me->log ).
      INSERT |ATTENTION: The created schema/xslt for type { absolute_typename } is not valid.| INTO TABLE report_log ##NO_TEXT.
    ENDIF.

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

    data(object) = get_object_infos_by_intfname( CONV #( p_intf ) ).
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

    DATA(objtype_value) = get_dynpro_value( fieldname  = `P_OBJTYP` ).
    objtype_value = to_upper( objtype_value ).

    IF intfname_value IS INITIAL.
      DATA search_pattern TYPE string.
      search_pattern = |IF_AFF%{ objtype_value }%|.
*  put all IF_AFF* Interfaces into the value help
      SELECT obj_name  FROM tadir WHERE obj_name LIKE @search_pattern AND devclass <> `SAFF_CORE` AND object = `INTF`
      INTO TABLE @DATA(value_help_result_table) UP TO 50 ROWS BYPASSING BUFFER ##NUMBER_OK. "#EC CI_NOORDER
    ELSE.
* The user does not have to type "*" on beginning and ending of the obj_name pattern, we add it automatically
      DATA(intfname_with_percent) = |%{ to_upper( intfname_value ) }%|.
      REPLACE ALL OCCURRENCES OF '*' IN intfname_with_percent WITH `%`.

      " Retrieve object names from tadir which match the search pattern entered in UI Element obj_name
      SELECT obj_name FROM tadir INTO TABLE @value_help_result_table UP TO 30 ROWS BYPASSING BUFFER
      WHERE object = `INTF` AND obj_name LIKE @intfname_with_percent ORDER BY obj_name ##NUMBER_OK. "#EC CI_NOORDER
    ENDIF.

    DATA(intfname1) = set_value_help_result_to_field( fieldname = `P_INTF` value_help_result_table = value_help_result_table ).

    IF intfname1 IS NOT INITIAL.
      DATA(object_infos) = get_object_infos_by_intfname( intfname1 ).

      set_object_infos_in_ui( object_infos ).
    ENDIF.
  ENDMETHOD.

  METHOD at_selection_screen.
    IF p_objtyp IS NOT INITIAL AND p_intf IS INITIAL AND p_examp IS INITIAL.
      p_objtyp = to_upper( p_objtyp ).
      DATA(objinfos) = get_object_infos_by_objtype( CONV #( p_objtyp ) ).
      set_object_infos_in_ui( objinfos ).
      p_intf = objinfos-interface.
      p_examp = objinfos-example.
    ELSEIF p_objtyp IS INITIAL AND p_intf IS NOT INITIAL AND p_examp IS INITIAL.
      p_intf = to_upper( p_intf ).
      objinfos = get_object_infos_by_intfname( CONV #( p_intf ) ).
      set_object_infos_in_ui( objinfos ).
      p_objtyp = objinfos-object_type.
      p_examp = objinfos-example.
    ELSEIF p_objtyp IS INITIAL AND p_intf IS INITIAL AND p_examp IS NOT INITIAL.
      p_examp = to_upper( p_examp ).
      objinfos = get_object_infos_by_exmplname( CONV #( p_examp ) ).
      set_object_infos_in_ui( objinfos ).
      p_intf = objinfos-interface.
      p_objtyp = objinfos-object_type.
    ENDIF.
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
    p_schema = i_schema.
    p_xslt  = i_xslt.
    p_repo  = i_repo.
    p_whole = i_whole.
    p_multre = i_multre.
    p_objtyp = i_objtyp.
    p_intf  = i_intf.
    p_type  = i_type.
    p_examp = i_examp.
    p_consol = i_consol.
    p_disk  = i_disk.
    p_readm  = i_readm.

    DATA object LIKE LINE OF p_multob.
    LOOP AT i_multob ASSIGNING FIELD-SYMBOL(<type>).
      object-option = 'EQ'.
      object-sign = 'I'.
      object-low = <type>.
      APPEND object TO p_multob.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_schema_test_content.
    me->schema_test_content = schema_test_content.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_generator_double DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif_generator.
    METHODS constructor
      IMPORTING
        log_to_return                TYPE REF TO zif_aff_log
        generate_type_will_raise_err TYPE abap_bool OPTIONAL.
    DATA log_to_return TYPE REF TO zif_aff_log.
    DATA generate_type_will_raise_err TYPE abap_bool.
ENDCLASS.
CLASS ltc_generator_double IMPLEMENTATION.

  METHOD lif_generator~generate_type.
    IF generate_type_will_raise_err = abap_true.
      RAISE EXCEPTION NEW cx_root( ).
    ELSE.
      DATA(type_description) = cl_abap_typedescr=>describe_by_data( data ).
      DATA(absolutename) = type_description->absolute_name.
*   \INTERFACE=IF_AFF_CHKC_V1\TYPE=TY_MAIN
      SPLIT absolutename AT '\' INTO TABLE DATA(splitted).
      DATA(objectpart) = splitted[ 2 ].
      REPLACE 'INTERFACE=' IN objectpart WITH ''.
      SPLIT objectpart AT '_' INTO TABLE DATA(splitted2).
      DATA(objecttype) = splitted2[ lines( splitted2 ) - 1 ].
      result = VALUE #(
          ( |Test ST/Schema for { objecttype }| ) ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_generator~get_log.
    log = log_to_return.
  ENDMETHOD.

  METHOD constructor.
    me->log_to_return = log_to_return.
    me->generate_type_will_raise_err = generate_type_will_raise_err.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_generator DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_generator.
    DATA generator_double TYPE REF TO lif_generator.
    DATA writer_double TYPE REF TO zif_aff_writer.
    DATA writer_log TYPE REF TO zcl_aff_log.
    DATA generator_log TYPE REF TO zcl_aff_log.
    DATA aff_factory_double TYPE REF TO if_aff_factory.
    DATA file_handler_double TYPE REF TO if_aff_object_file_handler.
    DATA expected_log_messages TYPE zif_aff_log=>tt_log_out.
    DATA expected_report_log TYPE stringtab.
    DATA gui_frontend TYPE REF TO ltc_gui_frontend.

    CONSTANTS c_aff_example_intf TYPE string VALUE 'AFF_EXAMPLE_INTF' ##NO_TEXT.
    CONSTANTS c_aff_example_intf_nspace TYPE string VALUE '/NAMESPACE/AFF_EXAMPLE_INTF' ##NO_TEXT.
    CONSTANTS c_example_intf_package TYPE string VALUE 'TEST_AFF_EXAMPLES' ##NO_TEXT.
    CONSTANTS c_intf TYPE string VALUE 'IF_AFF_INTF_V1' ##NO_TEXT.
    CONSTANTS c_intf_w_namespace TYPE string VALUE '/NAMESP/IF_AFF_INTF_V1' ##NO_TEXT.

    METHODS setup.

    METHODS configure_file_handler
      IMPORTING objects TYPE if_aff_object_file_handler=>tt_objects
      RAISING
                cx_root.

    METHODS assert_file_content
      IMPORTING file_name_tab TYPE stringtab
                zip           TYPE REF TO cl_abap_zip
      RAISING
                cx_root.

    METHODS insert_objects_into_tadir IMPORTING objects TYPE if_aff_object_file_handler=>tt_objects.

    METHODS create_new_cut_with_new_params
      IMPORTING
        i_schema TYPE abap_bool OPTIONAL
        i_xslt   TYPE abap_bool OPTIONAL
        i_repo   TYPE abap_bool OPTIONAL
        i_whole  TYPE abap_bool OPTIONAL
        i_multre TYPE abap_bool OPTIONAL
        i_objtyp TYPE trobjtype OPTIONAL
        i_intf   TYPE sobj_name OPTIONAL
        i_type   TYPE sobj_name OPTIONAL
        i_examp  TYPE sobj_name OPTIONAL
        i_consol TYPE abap_bool OPTIONAL
        i_disk   TYPE abap_bool OPTIONAL
        i_multob TYPE stringtab OPTIONAL.

    METHODS assert_logs_and_file_handler.
    METHODS schema_console_intf FOR TESTING RAISING cx_static_check.
    METHODS schema_zip_intf FOR TESTING RAISING cx_static_check.
    METHODS xslt_console_intf FOR TESTING RAISING cx_static_check.
    METHODS xslt_zip_intf FOR TESTING RAISING cx_static_check.
    METHODS repo_zip_intf FOR TESTING RAISING cx_static_check.
    METHODS whole_repo_zip FOR TESTING RAISING cx_static_check.
    METHODS multirepo_zip FOR TESTING RAISING cx_static_check.
    METHODS error_not_all_paramtrs_supplid FOR TESTING RAISING cx_static_check.
    METHODS generate_type_raises_error FOR TESTING RAISING cx_static_check.
    METHODS writer_validate_returns_false FOR TESTING RAISING cx_static_check.
    METHODS schema_console_func FOR TESTING RAISING cx_static_check.
    METHODS schema_zip_func FOR TESTING RAISING cx_static_check.
    METHODS xslt_consl_error_intf_not_exis FOR TESTING RAISING cx_static_check.
    METHODS xslt_consl_error_type_not_exis FOR TESTING RAISING cx_static_check.
    METHODS xsl_cons_intf_vers_not_readabl FOR TESTING RAISING cx_static_check.
    METHODS repo_zip_fugr FOR TESTING RAISING cx_static_check.
    METHODS repo_zip_intf_w_namespace FOR TESTING RAISING cx_static_check.
    METHODS repo_zip_tabl FOR TESTING RAISING cx_static_check.
    CLASS-DATA function_test_environment TYPE REF TO if_function_test_environment.
    CLASS-DATA environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.

ENDCLASS.

CLASS ltc_generator IMPLEMENTATION.

  METHOD class_setup.
    " keep in mind, the test doubles created for the given function modules would be active for the entire test session
    " and any CALL FUNCTION statement on actual function module would get replaced with the corresponding test double in the session.
    function_test_environment = cl_function_test_environment=>create( VALUE #( ( 'DYNP_VALUES_UPDATE' )
                                                                               ( 'DYNP_VALUES_READ' )
                                                                               ( 'F4IF_INT_TABLE_VALUE_REQUEST' )
                                                                               ) ).
    environment = cl_osql_test_environment=>create( VALUE #( ( 'TADIR' ) ) ).

  ENDMETHOD.

  METHOD class_teardown.
    IF environment IS BOUND.
      environment->destroy( ).
    ENDIF.
  ENDMETHOD.

  METHOD setup.
    " clear all configurations on test doubles
    function_test_environment->clear_doubles( ).
    environment->clear_doubles( ).

    writer_double ?= cl_abap_testdouble=>create( 'ZIF_AFF_WRITER' ).
    cl_abap_testdouble=>configure_call( writer_double )->returning( abap_true )->ignore_all_parameters( ).
    writer_double->validate( source = VALUE #( ) log = NEW zcl_aff_log( ) ).

    file_handler_double ?= cl_abap_testdouble=>create( 'IF_AFF_OBJECT_FILE_HANDLER' ).

    DATA objects TYPE if_aff_object_file_handler=>tt_objects.
    objects = VALUE #(
        ( obj_name = 'IF_AFF_TYPES_V1' devclass = 'SAFF_CORE' obj_type = 'INTF' )
        ( obj_name = 'IF_AFF_OO_TYPES_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = c_intf  devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = c_intf_w_namespace  devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = 'IF_AFF_CHKC_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = 'IF_AFF_CHKO_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = 'IF_AFF_CHKV_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = 'IF_AFF_CLAS_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = 'IF_AFF_DDLS_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = 'IF_AFF_DDLX_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = 'ZIF_AFF_DOMA_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = 'IF_AFF_ENHO_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = 'IF_AFF_ENHS_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = 'IF_AFF_FUGR_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = 'IF_AFF_FUNC_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = 'IF_AFF_REPS_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = 'IF_AFF_NROB_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = 'IF_AFF_TABL_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = 'IF_AFF_INDX_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
        ( obj_name = c_aff_example_intf devclass  = c_example_intf_package obj_type = 'INTF' )
        ( obj_name = c_aff_example_intf_nspace devclass  = c_example_intf_package obj_type = 'INTF' )
        ( obj_name = 'Z_AFF_EXAMPLE_INTF' devclass = c_example_intf_package obj_type = 'INTF' )
        ( obj_name = 'Z_AFF_EXAMPLE_CHKC' devclass = c_example_intf_package obj_type = 'CHKC' )
        ( obj_name = 'Z_AFF_EXAMPLE_CHKO' devclass = c_example_intf_package obj_type = 'CHKO' )
        ( obj_name = 'Z_AFF_EXAMPLE_CHKV' devclass = c_example_intf_package obj_type = 'CHKV' )
        ( obj_name = 'Z_AFF_EXAMPLE_CLAS' devclass = c_example_intf_package obj_type = 'CLAS' )
        ( obj_name = 'Z_AFF_EXAMPLE_DDLS' devclass = c_example_intf_package obj_type = 'DDLS' )
        ( obj_name = 'Z_AFF_EXAMPLE_DDLX' devclass = c_example_intf_package obj_type = 'DDLX' )
        ( obj_name = 'Z_AFF_EXAMPLE_DOMA' devclass = c_example_intf_package obj_type = 'DOMA' )
        ( obj_name = 'Z_AFF_EXAMPLE_ENHO' devclass = c_example_intf_package obj_type = 'ENHO' )
        ( obj_name = 'Z_AFF_EXAMPLE_ENHS' devclass = c_example_intf_package obj_type = 'ENHS' )
        ( obj_name = 'Z_AFF_EXAMPLE_FUGR' devclass = c_example_intf_package obj_type = 'FUGR' )
        ( obj_name = 'Z_AFF_NR' devclass = c_example_intf_package obj_type = 'NROB' ) ).
    TRY.
        configure_file_handler( objects ).
      CATCH cx_root.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.
    insert_objects_into_tadir( objects ).

    aff_factory_double ?= cl_abap_testdouble=>create( 'IF_AFF_FACTORY' ).
    cl_abap_testdouble=>configure_call( aff_factory_double )->returning( file_handler_double ).
    aff_factory_double->get_object_file_handler( ).

    writer_log = NEW zcl_aff_log( ).
    writer_log->zif_aff_log~add_info( message = VALUE #( msgty = 'I'  msgv1 = 'Writer Log' ) object = VALUE #( ) ).

    cl_abap_testdouble=>configure_call( writer_double )->returning( writer_log ).
    writer_double->get_log( ).

    generator_log = NEW zcl_aff_log( ).
    generator_log->zif_aff_log~add_info( message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) object = VALUE #( ) ).
    generator_double = NEW ltc_generator_double( generator_log ).

    gui_frontend = NEW ltc_gui_frontend( ).

    cut = NEW lcl_generator(
      aff_factory    = aff_factory_double
      generator      = generator_double
      writer         = writer_double
      i_gui_frontend = gui_frontend ).

    INSERT LINES OF generator_double->get_log( )->get_messages( ) INTO TABLE expected_log_messages.

  ENDMETHOD.

  METHOD assert_logs_and_file_handler.
    cl_abap_testdouble=>verify_expectations( file_handler_double ).
    cl_abap_unit_assert=>assert_equals( act = cut->report_log exp = expected_report_log ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut->log->get_messages( ) ) exp = lines( expected_log_messages ) ).
    LOOP AT expected_log_messages ASSIGNING FIELD-SYMBOL(<exp_msg>).
      READ TABLE cut->log->get_messages( ) WITH KEY text = <exp_msg>-text TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        cl_abap_unit_assert=>fail( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD insert_objects_into_tadir.

    DATA tadir TYPE STANDARD TABLE OF tadir.
    LOOP AT objects ASSIGNING FIELD-SYMBOL(<object>).
      APPEND VALUE #( pgmid = 'R3TR' object = <object>-obj_type obj_name = <object>-obj_name devclass = <object>-devclass ) TO tadir.
    ENDLOOP.
    environment->insert_test_data( tadir ).

  ENDMETHOD.

  METHOD configure_file_handler.
    DATA files TYPE if_aff_object_file_handler=>ty_object_files.
    DATA(text_handler) = NEW cl_aff_content_handler_text( ).

    LOOP AT objects ASSIGNING FIELD-SYMBOL(<object>).
      DATA(file_name) = |{ <object>-obj_name }.{ <object>-obj_type }.json|.
      IF <object>-obj_name = c_aff_example_intf_nspace.
        file_name = |(NAMESPACE)AFF_EXAMPLE_INTF.{ <object>-obj_type }.json|.
      ELSEIF <object>-obj_name = c_intf_w_namespace.
        file_name = |(NAMESP)IF_AFF_INTF_V1.{ <object>-obj_type }.json|.
      ENDIF.
      DATA(file_content) = text_handler->if_aff_content_handler~serialize( |File of { <object>-obj_name }| ).
      files = VALUE #(
          object_to_file_name = VALUE #(
              ( object = <object> file_name = file_name )
          )
        files = VALUE #(
         ( obj_type = <object>-obj_type obj_name = <object>-obj_name file_name = file_name content = file_content )
        ) ).
      cl_abap_testdouble=>configure_call( file_handler_double )->returning( files )->ignore_parameter( name = 'LOG' ).
      file_handler_double->serialize_objects( objects = VALUE #( ( <object> ) ) log = NEW zcl_aff_log( ) ).
    ENDLOOP.

    file_name = `file_of_reps_func_fugr.json`.
    file_content = text_handler->if_aff_content_handler~serialize( `File of REPS, FUNC, FUGR` ).
    files = VALUE #(
        object_to_file_name = VALUE #(
            ( object = <object> file_name = file_name )
        )
      files = VALUE #(
       ( obj_type = <object>-obj_type obj_name = <object>-obj_name file_name = file_name content = file_content )
      ) ).
    cl_abap_testdouble=>configure_call( file_handler_double )->returning( files )->ignore_parameter( name = 'LOG' ).
    file_handler_double->serialize_objects( objects = VALUE #(
                                            ( obj_name = 'IF_AFF_FUGR_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
                                            ( obj_name = 'IF_AFF_FUNC_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
                                            ( obj_name = 'IF_AFF_REPS_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
                                            )
                                            log     = NEW zcl_aff_log( ) ).
    file_name = `file_of_indx_tabl.json`.
    file_content = text_handler->if_aff_content_handler~serialize( `File of INDX, TABL` ).
    files = VALUE #(
        object_to_file_name = VALUE #(
            ( object = <object> file_name = file_name )
        )
      files = VALUE #(
       ( obj_type = <object>-obj_type obj_name = <object>-obj_name file_name = file_name content = file_content )
      ) ).
    cl_abap_testdouble=>configure_call( file_handler_double )->returning( files )->ignore_parameter( name = 'LOG' ).
    file_handler_double->serialize_objects( objects = VALUE #(
                                            ( obj_name = 'IF_AFF_TABL_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
                                            ( obj_name = 'IF_AFF_INDX_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
                                            )
                                            log     = NEW zcl_aff_log( ) ).
  ENDMETHOD.

  METHOD xslt_console_intf.

    cut->set_parameters(
      i_xslt   = abap_true
      i_objtyp = 'INTF'
      i_intf   = CONV #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = VALUE #( ) ).
    "When
    cut->start_of_selection( ).
    "Then
    expected_report_log = VALUE #( ).
    assert_logs_and_file_handler( ).

  ENDMETHOD.

  METHOD xsl_cons_intf_vers_not_readabl.

    cut->set_parameters(
      i_xslt   = abap_true
      i_objtyp = 'INTF'
      i_intf   = 'IF_AFF_INTF_V1X' "version not readable
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = VALUE #( ) ).
    "When
    cut->start_of_selection( ).
    "Then
    expected_report_log = VALUE #(
  ( `Formatversion couldnt be derived from interface IF_AFF_INTF_V1X. Format version 1 was assumed.` )
  ( `Type \INTERFACE=IF_AFF_INTF_V1X\TYPE=TY_MAIN was not found. Either interface or type doesnt exist.` ) ).
    CLEAR expected_log_messages.
    assert_logs_and_file_handler( ).

  ENDMETHOD.

  METHOD xslt_consl_error_intf_not_exis.

    cut->set_parameters(
      i_xslt   = abap_true
      i_objtyp = 'INTF'
      i_intf   = 'IF_AFF_INTF_V99999' "interface does not exist in system
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = VALUE #( ) ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_initial( cut->zip ).

    expected_report_log = VALUE #(
  ( `Type \INTERFACE=IF_AFF_INTF_V99999\TYPE=TY_MAIN was not found. Either interface or type doesnt exist.` ) ).
    CLEAR expected_log_messages.
    assert_logs_and_file_handler( ).

  ENDMETHOD.

  METHOD xslt_consl_error_type_not_exis.

    cut->set_parameters(
      i_xslt   = abap_true
      i_objtyp = 'INTF'
      i_intf   = 'IF_AFF_INTF_V1'
      i_type   = 'TY_BLABLA' "type does not exist in interface IF_AFF_INTF_V1
      i_examp  = CONV #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = VALUE #( ) ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_initial( cut->zip ).
    expected_report_log = VALUE #(
  ( `Type \INTERFACE=IF_AFF_INTF_V1\TYPE=TY_BLABLA was not found. Either interface or type doesnt exist.` ) ).
    CLEAR expected_log_messages.
    assert_logs_and_file_handler( ).
  ENDMETHOD.


  METHOD schema_console_intf.
    cut->set_parameters(
      i_schema = abap_true
      i_objtyp = 'INTF'
      i_intf   = CONV #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = VALUE #( ) ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_equals( act = cut->xslt_schema_content exp = VALUE string_table( ( |Test ST/Schema for INTF| ) ) ).

    expected_report_log = VALUE #( ).
    assert_logs_and_file_handler( ).
  ENDMETHOD.

  METHOD schema_console_func.
    cut->set_parameters(
      i_schema = abap_true
      i_objtyp = 'FUNC'
      i_intf   = 'IF_AFF_FUNC_V1'
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = VALUE #( ) ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_equals( act = cut->xslt_schema_content exp = VALUE string_table( ( |Test ST/Schema for FUNC| ) ) ).
    expected_report_log = VALUE #( ).
    assert_logs_and_file_handler( ).

  ENDMETHOD.

  METHOD schema_zip_func.
    cut->set_parameters(
      i_schema = abap_true
      i_objtyp = 'FUNC'
      i_intf   = 'IF_AFF_FUNC_V1'
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_disk   = abap_true
      i_multob = VALUE #( ) ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 1 ]-name exp = `func_schema.txt` ).
    cut->zip->get( EXPORTING name = `func_schema.txt` IMPORTING content = DATA(act_content) ).
    DATA(text_handler) = NEW cl_aff_content_handler_text( ).
    DATA(expected_content) = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for FUNC` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    expected_report_log = VALUE #(
  ( `Success: Zip file created here FULLPATH.zip` ) ).
    assert_logs_and_file_handler( ).
  ENDMETHOD.

  METHOD schema_zip_intf.
    cut->set_parameters(
      i_schema = abap_true
      i_objtyp = 'INTF'
      i_intf   = CONV #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_disk   = abap_true
      i_multob = VALUE #( ) ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 1 ]-name exp = `intf_schema.txt` ).
    cut->zip->get( EXPORTING name = `intf_schema.txt` IMPORTING content = DATA(act_content) ).
    DATA(text_handler) = NEW cl_aff_content_handler_text( ).
    DATA(expected_content) = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for INTF` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    expected_report_log = VALUE #(
  ( `Success: Zip file created here FULLPATH.zip` ) ).
    assert_logs_and_file_handler( ).
  ENDMETHOD.

  METHOD xslt_zip_intf.
    cut->set_parameters(
      i_xslt   = abap_true
      i_objtyp = 'INTF'
      i_intf   = CONV #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_disk   = abap_true
      i_multob = VALUE #( ) ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 1 ]-name exp = `intf_xslt.txt` ).
    cut->zip->get( EXPORTING name = `intf_xslt.txt` IMPORTING content = DATA(act_content) ).
    DATA(text_handler) = NEW cl_aff_content_handler_text( ).
    DATA(expected_content) = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for INTF` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).
    expected_report_log = VALUE #(
  ( `Success: Zip file created here FULLPATH.zip` ) ).
    assert_logs_and_file_handler( ).
  ENDMETHOD.

  METHOD repo_zip_intf.
    cut->set_parameters(
      i_repo   = abap_true
      i_objtyp = 'INTF'
      i_intf   = CONV #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_disk   = abap_true
      i_multob = VALUE #( ) ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 4 ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 1 ]-name exp = `intf/examples/zaff_example_intf.intf.json` ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 2 ]-name exp = `intf/type/zif_aff_intf_v1.intf.json` ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 3 ]-name exp = `intf/intf-v1.json` ).

    DATA(text_handler) = NEW cl_aff_content_handler_text( ).
    DATA(expected_content) = text_handler->if_aff_content_handler~serialize( `File of zaff_example_intf` ).

    cut->zip->get( EXPORTING name  = `intf/examples/zaff_example_intf.intf.json` IMPORTING content  = DATA(act_content) ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    expected_content = text_handler->if_aff_content_handler~serialize( `File of zif_aff_intf_v1` ).
    cut->zip->get( EXPORTING name  = `intf/type/zif_aff_intf_v1.intf.json` IMPORTING content  = act_content ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    cut->zip->get( EXPORTING name  = `intf/intf-v1.json` IMPORTING content  = act_content ).
    expected_content = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for INTF` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).
    expected_report_log = VALUE #(
  ( `Success: Zip file created here FULLPATH.zip` ) ).
    assert_logs_and_file_handler( ).
  ENDMETHOD.


  METHOD repo_zip_intf_w_namespace.
    cut->set_parameters(
      i_repo   = abap_true
      i_objtyp = 'INTF'
      i_intf   = CONV #( c_intf_w_namespace )
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf_nspace )
      i_disk   = abap_true
      i_multob = VALUE #( ) ).
    cut->set_schema_test_content( VALUE #( ( `TEST ABC` ) ) ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 4 ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 1 ]-name exp = `intf/examples/zaff_example_intf.intf.json` ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 2 ]-name exp = `intf/type/zif_aff_intf_v1.intf.json` ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 3 ]-name exp = `intf/intf-v1.json` ).

    DATA(text_handler) = NEW cl_aff_content_handler_text( ).
    DATA(expected_content) = text_handler->if_aff_content_handler~serialize( `File of zaff_example_intf` ).

    cut->zip->get( EXPORTING name  = `intf/examples/zaff_example_intf.intf.json` IMPORTING content  = DATA(act_content) ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    expected_content = text_handler->if_aff_content_handler~serialize( `File of zif_aff_intf_v1` ).
    cut->zip->get( EXPORTING name  = `intf/type/zif_aff_intf_v1.intf.json` IMPORTING content  = act_content ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    cut->zip->get( EXPORTING name  = `intf/intf-v1.json` IMPORTING content  = act_content ).
    expected_content = text_handler->if_aff_content_handler~serialize( `TEST ABC` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).
    expected_report_log = VALUE #(
  ( `Type \INTERFACE=/NAMESP/IF_AFF_INTF_V1\TYPE=TY_MAIN was not found. Either interface or type doesnt exist.` )
  ( `Success: Zip file created here FULLPATH.zip` ) ).
    CLEAR expected_log_messages.
    assert_logs_and_file_handler( ).
  ENDMETHOD.


  METHOD repo_zip_fugr.
    cut->set_parameters(
      i_repo   = abap_true
      i_objtyp = 'FUGR'
      i_intf   = 'IF_AFF_FUGR_V1'
      i_type   = 'TY_MAIN'
      i_examp  = 'Z_AFF_EXAMPLE_FUGR'
      i_disk   = abap_true
      i_multob = VALUE #( ) ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 6 ).

    DATA(text_handler) = NEW cl_aff_content_handler_text( ).
    DATA(expected_content) = text_handler->if_aff_content_handler~serialize( `File of Z_AFF_EXAMPLE_FUGR` ).

    cut->zip->get( EXPORTING name  = `fugr/examples/z_aff_example_fugr.fugr.json` IMPORTING content  = DATA(act_content) ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    expected_content = text_handler->if_aff_content_handler~serialize( `File of REPS, FUNC, FUGR` ).
    cut->zip->get( EXPORTING name  = `fugr/type/file_of_reps_func_fugr.json` IMPORTING content  = act_content ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    cut->zip->get( EXPORTING name  = `fugr/fugr-v1.json` IMPORTING content  = act_content ).
    expected_content = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for FUGR` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).
    cut->zip->get( EXPORTING name  = `fugr/func-v1.json` IMPORTING content  = act_content ).
    expected_content = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for FUNC` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).
    cut->zip->get( EXPORTING name  = `fugr/reps-v1.json` IMPORTING content  = act_content ).
    expected_content = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for REPS` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    expected_report_log = VALUE #(
  ( `Success: Zip file created here FULLPATH.zip` ) ).
    expected_log_messages = VALUE #(
  ( object = VALUE #( devclass = 'IF_AFF_FUGR_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( object = VALUE #( devclass = 'IF_AFF_FUNC_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( object = VALUE #( devclass = 'IF_AFF_REPS_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) ) ).
    assert_logs_and_file_handler( ).
  ENDMETHOD.


  METHOD repo_zip_tabl.
    cut->set_parameters(
      i_repo   = abap_true
      i_objtyp = 'TABL'
      i_intf   = 'IF_AFF_TABL_V1'
      i_type   = 'TY_MAIN'
      i_examp  = 'Z_AFF_EXAMPLE_TABL'
      i_disk   = abap_true
      i_multob = VALUE #( ) ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 3 ).

    DATA(text_handler) = NEW cl_aff_content_handler_text( ).
    DATA(expected_content) = text_handler->if_aff_content_handler~serialize( `File of INDX, TABL` ).
    cut->zip->get( EXPORTING name  = `tabl/type/file_of_indx_tabl.json` IMPORTING content  = DATA(act_content) ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    cut->zip->get( EXPORTING name  = `tabl/indx-v1.json` IMPORTING content  = act_content ).
    expected_content = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for INDX` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    expected_report_log = VALUE #(
( `Type \INTERFACE=IF_AFF_TABL_V1\TYPE=TY_MAIN was not found. Either interface or type doesnt exist.` )
( `The schema for interface IF_AFF_TABL_V1 could not be created.` )
( `Success: Zip file created here FULLPATH.zip` ) ).
    expected_log_messages = VALUE #(
  ( object = VALUE #( devclass = 'IF_AFF_INDX_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) ) ).
    assert_logs_and_file_handler( ).
  ENDMETHOD.

  METHOD whole_repo_zip.
    cut->set_parameters(
      i_whole  = abap_true
      i_objtyp = 'INTF'
      i_intf   = CONV #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_disk   = abap_true
      i_multob = VALUE #( ) ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_number_between(
      lower  = 48 "DOMA only has a local interface in UIA
      upper  = 53
      number = lines( cut->zip->files ) ).

    cut->zip->get( EXPORTING name  = `fugr/type/file_of_reps_func_fugr.json` IMPORTING content  = DATA(act_content) ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = cl_abap_codepage=>convert_to( `File of REPS, FUNC, FUGR` ) ).

    "these are the expected files. In our unit test (for simplicity reasons) the aff of all object types consist only of 1 json file
    DATA file_name_tab TYPE stringtab.
    file_name_tab = VALUE #(
    ( `chkc/examples/z_aff_example_chkc.chkc.json` )
    ( `chkc/type/zif_aff_chkc_v1.intf.json` )
    ( `chkc/chkc-v1.json` )
    ( `chko/examples/z_aff_example_chko.chko.json` )
    ( `chko/type/zif_aff_chko_v1.intf.json` )
    ( `chko/chko-v1.json` )
    ( `chkv/examples/z_aff_example_chkv.chkv.json` )
    ( `chkv/type/zif_aff_chkv_v1.intf.json` )
    ( `chkv/chkv-v1.json` )
    ( `clas/examples/z_aff_example_clas.clas.json` )
    ( `clas/type/zif_aff_clas_v1.intf.json` )
    ( `clas/clas-v1.json` )
    ( `ddls/examples/z_aff_example_ddls.ddls.json` )
    ( `ddls/type/zif_aff_ddls_v1.intf.json` )
    ( `ddls/ddls-v1.json` )
    ( `ddlx/examples/z_aff_example_ddlx.ddlx.json` )
    ( `ddlx/type/zif_aff_ddlx_v1.intf.json` )
    ( `ddlx/ddlx-v1.json` )
    ( `enho/examples/z_aff_example_enho.enho.json` )
    ( `enho/type/zif_aff_enho_v1.intf.json` )
    ( `enho/enho-v1.json` )
    ( `enhs/examples/z_aff_example_enhs.enhs.json` )
    ( `enhs/type/zif_aff_enhs_v1.intf.json` )
    ( `enhs/enhs-v1.json` )
    ( `fugr/examples/z_aff_example_fugr.fugr.json` )
    ( `fugr/fugr-v1.json` )
    ( `fugr/func-v1.json` )
    ( `fugr/reps-v1.json` )
    ( `intf/examples/z_aff_example_intf.intf.json` )
    ( `intf/type/zif_aff_intf_v1.intf.json` )
    ( `intf/intf-v1.json` )
    ( `nrob/examples/z_aff_nr.nrob.json` )
    ( `nrob/type/zif_aff_nrob_v1.intf.json` )
    ( `nrob/nrob-v1.json` )
    ( `zif_aff_types_v1.intf.json` )
    ( `zif_aff_oo_types_v1.intf.json` ) ).


    assert_file_content(
      file_name_tab = file_name_tab
      zip           = cut->zip ).
    expected_log_messages = VALUE #(
  ( object = VALUE #( obj_name  = 'IF_AFF_CHKC_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( object = VALUE #( obj_name  = 'IF_AFF_CHKO_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( object = VALUE #( obj_name  = 'IF_AFF_CHKV_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( object = VALUE #( obj_name  = 'IF_AFF_CLAS_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( object = VALUE #( obj_name  = 'IF_AFF_DDLS_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( object = VALUE #( obj_name  = 'IF_AFF_ENHO_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( object = VALUE #( obj_name  = 'IF_AFF_ENHS_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( object = VALUE #( obj_name  = 'IF_AFF_FUGR_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( object = VALUE #( obj_name  = 'IF_AFF_FUNC_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( object = VALUE #( obj_name  = 'IF_AFF_REPS_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( object = VALUE #( obj_name  = 'IF_AFF_INTF_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( object = VALUE #( obj_name  = 'IF_AFF_NROB_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( object = VALUE #( obj_name  = 'IF_AFF_DDLX_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( object = VALUE #( obj_name  = 'ZIF_AFF_DOMA_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) ) ).

    cl_abap_testdouble=>verify_expectations( file_handler_double ).

    cl_abap_unit_assert=>assert_number_between(
        lower            = 12 "DOMA only has a local interface in UIA
        upper            = 14
        number           = lines( cut->log->get_messages( ) ) ).
    IF lines( cut->log->get_messages( ) ) = 14.
      cl_abap_unit_assert=>assert_equals( act = cut->log->get_messages( ) exp = expected_log_messages ).
    ENDIF.
  ENDMETHOD.

  METHOD multirepo_zip.
    cut->set_parameters(
      i_multre = abap_true
      i_objtyp = 'INTF'
      i_intf   = CONV #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_disk   = abap_true
      i_multob = VALUE #( ( `CHKC` ) ( `CHKO` ) ( `CHKV` ) ) ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 12 ).

    "these are the expected files. In our unit test (for simplicity reasons) the aff of all object types consist only of 1 json file
    DATA file_name_tab TYPE stringtab.
    file_name_tab = VALUE #(
    ( `chkc/examples/z_aff_example_chkc.chkc.json` )
    ( `chkc/type/zif_aff_chkc_v1.intf.json` )
    ( `chkc/chkc-v1.json` )
    ( `chko/examples/z_aff_example_chko.chko.json` )
    ( `chko/type/zif_aff_chko_v1.intf.json` )
    ( `chko/chko-v1.json` )
    ( `chkv/examples/z_aff_example_chkv.chkv.json` )
    ( `chkv/type/zif_aff_chkv_v1.intf.json` )
    ( `chkv/chkv-v1.json` ) ).

    assert_file_content(
      file_name_tab = file_name_tab
      zip           = cut->zip ).
    expected_report_log = VALUE #(
  ( `Success: Zip file created here FULLPATH.zip` ) ).
    expected_log_messages = VALUE #(
   ( object = VALUE #( devclass = 'IF_AFF_CHKC_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
   ( object = VALUE #( devclass = 'IF_AFF_CHKO_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) )
   ( object = VALUE #( devclass = 'IF_AFF_CHKV_V1' ) type = 'I' text = `I::000 Generator Log` message = VALUE #( msgty = 'I' msgv1 = 'Generator Log' ) ) ).
    assert_logs_and_file_handler( ).
  ENDMETHOD.



  METHOD generate_type_raises_error.
    "generator will raise an error when generate_type is called
    generator_double = NEW ltc_generator_double( log_to_return  = generator_log generate_type_will_raise_err = abap_true ).

    cut = NEW lcl_generator(
      aff_factory = aff_factory_double
      generator   = generator_double
      writer      = writer_double ).

    "schema on console
    cut->set_parameters(
      i_schema = abap_true
      i_objtyp = 'INTF'
      i_intf   = CONV #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = VALUE #( ) ).

    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_initial( cut->zip ).

    expected_report_log = VALUE #(
  ( `The generator couldn't generate the schema/XSLT for type \INTERFACE=IF_AFF_INTF_V1\TYPE=TY_MAIN` ) ).
    CLEAR expected_log_messages.
    assert_logs_and_file_handler( ).
  ENDMETHOD.

  METHOD writer_validate_returns_false.
    "writer returns false when validate is called
    writer_double ?= cl_abap_testdouble=>create( 'ZIF_AFF_WRITER' ).
    cl_abap_testdouble=>configure_call( writer_double )->returning( abap_false )->ignore_all_parameters( ).
    writer_double->validate( source = VALUE #( ) log = NEW zcl_aff_log( ) ).

    cl_abap_testdouble=>configure_call( writer_double )->returning( writer_log ).
    writer_double->get_log( ).

    cut = NEW lcl_generator(
      aff_factory = aff_factory_double
      generator   = generator_double
      writer      = writer_double ).

    "schema on console
    cut->set_parameters(
      i_schema = abap_true
      i_objtyp = 'INTF'
      i_intf   = CONV #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = VALUE #( ) ).

    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_not_initial( cut->xslt_schema_content ).
    expected_report_log = VALUE #(
  ( `ATTENTION: The created schema/xslt for type \INTERFACE=IF_AFF_INTF_V1\TYPE=TY_MAIN is not valid.` ) ).
    assert_logs_and_file_handler( ).
  ENDMETHOD.

  METHOD error_not_all_paramtrs_supplid.
    expected_log_messages = VALUE #( ).
    expected_report_log = VALUE #(
  ( `Please fill out all fields (interfacename, objecttype, abaptypename)` ) ).
    "schema on console
    create_new_cut_with_new_params(
      i_schema = abap_true
*     i_objtyp = 'INTF'          object type not supplied
      i_intf   = CONV #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = VALUE #( ) ).

    create_new_cut_with_new_params(
      i_schema = abap_true
      i_objtyp = 'INTF'
*     i_intf   = conv #( c_intf )    interfacename not supplied
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = VALUE #( ) ).

    create_new_cut_with_new_params(
      i_schema = abap_true
      i_objtyp = 'INTF'
      i_intf   = CONV #( c_intf )
*     i_type   = 'TY_MAIN'               typename not supplied
      i_examp  = CONV #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = VALUE #( ) ).

    expected_report_log = VALUE #(
  ( `Please fill out all fields (objecttype, interfacename, examplename)` ) ).

    " repo folder for one object
    create_new_cut_with_new_params(
      i_repo   = abap_true
*     i_objtyp = 'INTF'          object type not supplied
      i_intf   = CONV #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = VALUE #( ) ).

    create_new_cut_with_new_params(
      i_repo   = abap_true
      i_objtyp = 'INTF'
*     i_intf   = conv #( c_intf )    interfacename not supplied
      i_type   = 'TY_MAIN'
      i_examp  = CONV #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = VALUE #( ) ).

    create_new_cut_with_new_params(
      i_repo   = abap_true
      i_objtyp = 'INTF'
      i_intf   = CONV #( c_intf )
      i_type   = 'TY_MAIN'
*     i_examp  = conv #( c_aff_example_intf )   examplename not supplied
      i_consol = abap_true
      i_multob = VALUE #( ) ).

  ENDMETHOD.

  METHOD create_new_cut_with_new_params.
    cut = NEW lcl_generator(
      aff_factory = aff_factory_double
      generator   = generator_double
      writer      = writer_double ).

    cut->set_parameters(
      i_schema = i_schema
      i_xslt   = i_xslt
      i_repo   = i_repo
      i_whole  = i_whole
      i_multre = i_multre
      i_objtyp = i_objtyp
      i_intf   = i_intf
      i_type   = i_type
      i_examp  = i_examp
      i_consol = i_consol
      i_disk   = i_disk
      i_multob = i_multob ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_initial( cut->zip ).

    assert_logs_and_file_handler( ).
  ENDMETHOD.

  METHOD assert_file_content.

    DATA(text_handler) = NEW cl_aff_content_handler_text( ).

    LOOP AT file_name_tab ASSIGNING FIELD-SYMBOL(<filename>).
      zip->get( EXPORTING name  = <filename> IMPORTING content  = DATA(act_content) ).
      IF <filename> CP '*-v1.json'.
        SPLIT <filename> AT '/' INTO TABLE DATA(splitted).
        DATA(objecttype) = splitted[ 2 ].
        REPLACE '-v1.json' IN objecttype WITH ''.
        DATA(expected_content) = text_handler->if_aff_content_handler~serialize( |Test ST/Schema for { to_upper( objecttype ) }| ).
        cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).
      ELSE.
        SPLIT <filename> AT '/' INTO TABLE splitted.
        DATA(objectname_part) = splitted[ lines( splitted ) ].
        SPLIT objectname_part AT '.' INTO TABLE DATA(splitted2).
        DATA(objectname) = splitted2[ 1 ].
        DATA(expected_content_str) = |file of { to_lower( objectname ) }|.
        DATA(actual) = to_lower( cl_abap_codepage=>convert_from( act_content ) ).

        cl_abap_unit_assert=>assert_equals( act = actual exp = expected_content_str ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


INITIALIZATION.




  helper = NEW lcl_generator( ).
  helper->modify_screen( ).

  LOOP AT helper->get_table_with_all_githubtypes( ) ASSIGNING FIELD-SYMBOL(<type>) ##NEEDED.
    p_multob-option = 'EQ'.
    p_multob-sign = 'I'.
    p_multob-low = <type>.
    APPEND p_multob.
  ENDLOOP.

* when enter or F8 is pressed in the  screen
AT SELECTION-SCREEN.
  helper->at_selection_screen( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_objtyp.
  helper->on_value_request_for_objtype( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_intf.
  helper->on_value_request_for_intfname( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_examp.
  helper->on_value_request_for_example( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_type.
  helper->on_value_request_for_type( ).

AT SELECTION-SCREEN OUTPUT.


  helper->modify_screen( ).

START-OF-SELECTION.

  helper->start_of_selection( ).
  helper->print_logs( ).

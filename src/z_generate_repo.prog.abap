*&---------------------------------------------------------------------*
*& Report z_generate_repo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report z_generate_repo.
class lcl_generator definition deferred.
data obj_types type saff_repo_types.
data helper type ref to lcl_generator ##NEEDED.

interface lif_gui_frontend_service.
  methods display.
  methods clear.
  methods write
    importing message type any.
  methods write_to_console_and_screen
    importing message type string.
  methods file_save_dialog
    importing value(window_title)      type string optional
              value(default_extension) type string optional
              value(default_file_name) type string optional
    changing  filename                 type string
              path                     type string
              fullpath                 type string
              user_action              type i optional
    raising   zcx_aff_tools.
  methods gui_download
    importing
              !bin_filesize     type i optional
              !filename         type string
              !filetype         type char10 default 'ASC'
              !write_lf         type char01 default 'X'
    exporting
              value(filelength) type i
    changing
              !data_tab         type standard table
    raising   zcx_aff_tools.

endinterface.

class lcl_gui_frontend definition
  final create public.
  public section.
    interfaces lif_gui_frontend_service.
endclass.

class lcl_gui_frontend implementation.


  method lif_gui_frontend_service~file_save_dialog.
    cl_gui_frontend_services=>file_save_dialog(
      exporting
        default_extension   = default_extension
        default_file_name   = default_file_name
      changing
        filename            = filename
        path                = path
        fullpath            = fullpath
        user_action         = user_action
                              ) ##SUBRC_OK.
  endmethod.

  method lif_gui_frontend_service~gui_download.
    clear filelength.
    cl_gui_frontend_services=>gui_download(
      exporting
        filename                  = filename
        bin_filesize              = bin_filesize
        filetype                  = filetype
        write_lf                  = write_lf
      changing
        data_tab                  = data_tab
    ).
  endmethod.

  method lif_gui_frontend_service~write_to_console_and_screen.
    write: / message.
    cl_demo_output=>write( message ).
  endmethod.

  method lif_gui_frontend_service~clear.
    cl_demo_output=>clear( ).
  endmethod.

  method lif_gui_frontend_service~display.
    cl_demo_output=>display( ).
  endmethod.

  method lif_gui_frontend_service~write.
    cl_demo_output=>write( message  ).
  endmethod.

endclass.

class ltc_gui_frontend definition final for testing.
  public section .
    interfaces lif_gui_frontend_service.
endclass.
class ltc_gui_frontend implementation.

  method lif_gui_frontend_service~file_save_dialog.
    if default_extension         = `zip`.

      filename   = 'TESTFILENAME.zip'.
      path       = 'TESTPATH'.
      fullpath   = 'FULLPATH.zip'.
      user_action = 0.

    endif.
  endmethod.

  method lif_gui_frontend_service~gui_download.
    clear filelength.
    if filename = 'TESTFILENAME.zip' and filetype = 'BIN' and write_lf = space.
      data_tab = value #( ).
      sy-subrc = 0.
    else.
      data_tab = value #( ).
      sy-subrc = 1.
    endif.
  endmethod.

  method lif_gui_frontend_service~write_to_console_and_screen ##NEEDED.

  endmethod.

  method lif_gui_frontend_service~clear ##NEEDED.

  endmethod.

  method lif_gui_frontend_service~display ##NEEDED.

  endmethod.

  method lif_gui_frontend_service~write ##NEEDED.

  endmethod.

endclass.

interface lif_generator.
  methods generate_type
    importing data          type data
    returning value(result) type rswsourcet
    raising   zcx_aff_tools.
  methods get_log
    returning
      value(log) type ref to zif_aff_log.
endinterface.

class lcl_generator_helper definition
  final
  create public.
  public section.
    interfaces lif_generator.
    methods constructor
      importing
        writer type ref to zif_aff_writer.
    data generator type ref to zcl_aff_generator.
endclass.

class lcl_generator_helper implementation.

  method constructor.
    me->generator = new zcl_aff_generator( writer ).
  endmethod.

  method lif_generator~generate_type.
    result = me->generator->generate_type( data ).
  endmethod.

  method lif_generator~get_log.
    log = me->generator->get_log( ).
  endmethod.

endclass.


selection-screen begin of block block_1 with frame title text-020.
  parameters:
    p_schema type c radiobutton group sel user-command upd default 'X',
    p_xslt   type c radiobutton group sel,
    p_repo   type c radiobutton group sel,
    p_whole  type c radiobutton group sel,
    p_multre type c radiobutton group sel.
selection-screen end of block block_1.

selection-screen begin of block block_2 with frame title text-021 ##TEXT_POOL.
  parameters:
    p_objtyp type trobjtype,
    p_intf   type sobj_name,
    p_type   type sobj_name default 'TY_MAIN',
    p_examp  type sobj_name,
    p_readm  type abap_bool default abap_true as checkbox,
    p_consol type c radiobutton group two user-command two default 'X',
    p_disk   type c radiobutton group two.
  select-options:
 p_multob for obj_types no intervals.
selection-screen end of block block_2.

types: begin of aff_object,
         object_type    type c length 4,
         interface      type sobj_name,
         example        type sobj_name,
         format_version type i,
       end of aff_object,
       aff_objects_table type standard table of aff_object.


class lcl_generator definition final create public .

  public section.
    data generator_log type ref to zif_aff_log.
    data aff_framework_log type ref to if_aff_log.
    data report_log type stringtab.
    data xslt_schema_content type rswsourcet.
    data schema_test_content type rswsourcet.
    data zip type ref to cl_abap_zip.
    data gui_frontend_service type ref to lif_gui_frontend_service.

    methods: set_parameters
      importing
        i_schema type abap_bool default abap_false
        i_xslt   type abap_bool default abap_false
        i_repo   type abap_bool default abap_false
        i_whole  type abap_bool default abap_false
        i_multre type abap_bool default abap_false
        i_objtyp type trobjtype optional
        i_intf   type sobj_name optional
        i_type   type sobj_name optional
        i_examp  type sobj_name optional
        i_consol type abap_bool default abap_false
        i_disk   type abap_bool default abap_false
        i_readm  type abap_bool default abap_true
        i_multob type stringtab optional,

      set_schema_test_content
        importing schema_test_content type rswsourcet,

      constructor
        importing
          i_gui_frontend type ref to lif_gui_frontend_service optional
          aff_factory    type ref to if_aff_factory optional
          generator      type ref to lif_generator optional
          writer         type ref to zif_aff_writer optional,
      start_of_selection,
      on_value_request_for_type,
      on_value_request_for_objtype,
      on_value_request_for_intfname,
      on_value_request_for_example,
      modify_screen,
      at_selection_screen,
      get_table_with_all_githubtypes
        returning value(type_table) type stringtab,
      write_to_zip
        importing zip_archive type xstring
                  zipname     type string,
      create_schema_xslt_zip
        importing content      type rswsourcet
        returning value(r_zip) type ref to cl_abap_zip,
      print_logs.

  private section.

    types: clsname_tab type standard table of seoclsname,
           begin of replacing_line,
             to_be_replaced type string,
             replace_with   type string,
           end of replacing_line.

    types: replacing_tab type standard table of replacing_line.

    data:"needed for testing
      aff_factory type ref to  if_aff_factory,
      generator   type ref to lif_generator,
      writer      type ref to zif_aff_writer.
    data replacing_table_string type replacing_tab.

    methods: get_replacing_table_and_intfs
      importing name_of_intf_of_mainobj type string
                example_files           type if_aff_object_file_handler=>ty_object_files
      exporting interfaces              type clsname_tab,
      replace_names_in_string
        importing
                  content_as_string      type string
                  replacing_table_string type replacing_tab
        returning value(content)         type string,
      add_aff_files_to_zip
        importing
          files                  type if_aff_object_file_handler=>ty_object_files
          filename               type string
          replacing_table_string type replacing_tab,
      generate_repo_folder
        importing objects          type aff_objects_table
                  whole_aff_folder type boolean default abap_false,
      create_the_variable_dynamicaly
        importing absolute_typename type string
        returning value(variable)   type ref to data
        raising
                  zcx_aff_tools,
      get_dynpro_value
        importing fieldname         type string
        returning value(fieldvalue) type string,
      set_value_help_result_to_field
        importing fieldname               type string
                  value_help_result_table type standard table
        returning value(chosen_value)     type string,
      set_object_infos_in_ui
        importing i_object type aff_object,
      get_object_infos_by_objtype
        importing objecttype    type string
        returning value(object) type aff_object,
      get_object_infos_by_intfname
        importing intfname      type string
        returning value(object) type aff_object,
      get_object_infos_by_exmplname
        importing examplename   type string
        returning value(object) type aff_object,
      get_intfname_highest_version
        importing objecttype     type string
        exporting intfname       type string
                  format_version type i,
      get_format_version_of_intfname
        importing intfname              type string
        returning value(format_version) type i,
      get_schema_or_xslt_content
        returning value(content) type rswsourcet,
      get_content
        importing absolute_typename type string
                  interfacename     type string
        returning value(content)    type rswsourcet,
      object_as_string
        importing object        type if_aff_object_file_handler=>ty_object
        returning value(result) type string,
      get_objname_wo_namspace_with_z
        importing object_name   type string
        returning value(result) type string,
      add_file_to_zip
        importing
          i_file_name         type string
          i_stringtab_content type rswsourcet
          i_error_text        type string.

endclass.

class lcl_generator implementation.

  method write_to_zip.
    data file_name type string.
    data path type string.
    data fullpath type string.
    data user_action type i.
    try.
        gui_frontend_service->file_save_dialog(
          exporting
            default_extension = `zip`
            default_file_name = zipname
          changing
            filename          = file_name
            path              = path
            fullpath          = fullpath
            user_action       = user_action
                                ) ##SUBRC_OK.
      catch zcx_aff_tools.
        insert `Either Serialization canceled via UI or file-save-dialog caused errors` into table report_log ##NO_TEXT.
        return.
    endtry.

*     On mac computers file_save_dialog( ) does not add ".zip" at the file_name ending.
    if not file_name cp '*.zip'.
      file_name = |{ file_name }.zip| ##NO_TEXT.
      fullpath = |{ fullpath }.zip| ##NO_TEXT.
    endif.

    " split xstring into a table
    constants chunk_size type i value 1000.
    data line type x length chunk_size.
    data content_as_table like table of line.
    data off type i.
    data(xstring_length) = xstrlen( zip_archive ).
    while off < xstring_length.
      line = zip_archive+off.
      append line to content_as_table.
      off = off + chunk_size.
    endwhile.
    try.
        gui_frontend_service->gui_download(
          exporting
            filename     = file_name
            bin_filesize = xstring_length
            filetype     = 'BIN'
            write_lf     = space
          changing
            data_tab     = content_as_table
        ).
        insert |Success: Zip file created here { fullpath }| into table report_log ##NO_TEXT.
      catch zcx_aff_tools.
        insert |File { fullpath } not created| into table report_log ##NO_TEXT.
    endtry.
  endmethod.

  method get_replacing_table_and_intfs.
*       fill the table with the strings which need to be replaced (interface objects need to begin with z)
    clear interfaces.
    append name_of_intf_of_mainobj to interfaces.
    if name_of_intf_of_mainobj cp `IF_AFF_FUGR*`.
      append `IF_AFF_FUNC_V1` to interfaces.
      append `IF_AFF_REPS_V1` to interfaces.
    endif.
    if name_of_intf_of_mainobj cp `IF_AFF_TABL*`.
      append `IF_AFF_INDX_V1` to interfaces.
    endif.

    " the interface names which need to be replaced
    loop at interfaces assigning field-symbol(<intf>).
      data(intf_name) = to_lower( <intf> ).
      if not intf_name cp `z*`.
        insert value #( to_be_replaced = intf_name replace_with = get_objname_wo_namspace_with_z( to_lower( <intf> ) ) ) into table replacing_table_string.
        if intf_name cp `*/*`.
          replace first occurrence of '/' in intf_name with'('.
          replace first occurrence of '/' in intf_name with')'.
          insert value #( to_be_replaced = intf_name replace_with = get_objname_wo_namspace_with_z( to_lower( <intf> ) ) ) into table replacing_table_string.
        endif.
      endif.
    endloop.

*  the example names which need to be replaced. (example objects need to begin with z)
    loop at example_files-object_to_file_name assigning field-symbol(<object>).
      data(name_of_example_obj) = to_lower( <object>-object-obj_name ).
      if not name_of_example_obj cp `z*`.
        insert value #( to_be_replaced = name_of_example_obj replace_with = get_objname_wo_namspace_with_z( name_of_example_obj ) ) into table replacing_table_string.
        if name_of_example_obj cp `*/*`.
          replace first occurrence of '/' in name_of_example_obj with'('.
          replace first occurrence of '/' in name_of_example_obj with')'.
          insert value #( to_be_replaced = name_of_example_obj replace_with = get_objname_wo_namspace_with_z( to_lower( <object>-object-obj_name  ) ) ) into table replacing_table_string.
        endif.
      endif.

      if <object>-object-sub_name is not initial.
        data(name_of_example_subobj) = to_lower( <object>-object-sub_name ).
        if not name_of_example_subobj cp `z*`.
          if ( <object>-object-obj_type = 'FUGR' and <object>-object-sub_type = 'FUNC' ) or
            ( <object>-object-obj_type = 'TABL' and <object>-object-sub_type = 'INDX' ) .
            insert value #( to_be_replaced = name_of_example_subobj replace_with = get_objname_wo_namspace_with_z( name_of_example_subobj ) ) into table replacing_table_string.
            if <object>-object-sub_name cp `*/*`.
              replace first occurrence of '/' in name_of_example_subobj with'('.
              replace first occurrence of '/' in name_of_example_subobj with')'.
              insert value #( to_be_replaced = name_of_example_subobj replace_with = get_objname_wo_namspace_with_z( conv #( <object>-object-sub_name ) ) ) into table replacing_table_string.
            endif.
          endif.
        endif.
      endif.
    endloop.
    insert value #( to_be_replaced = `if_aff_types_v1` replace_with = get_objname_wo_namspace_with_z( `zif_aff_types_v1` ) ) into table replacing_table_string ##NO_TEXT ##NO_TEXT.
    insert value #( to_be_replaced =  `if_aff_oo_types_v1` replace_with = get_objname_wo_namspace_with_z(  `zif_aff_oo_types_v1` ) ) into table replacing_table_string ##NO_TEXT ##NO_TEXT.

    sort replacing_table_string ascending by to_be_replaced.
    delete adjacent duplicates from replacing_table_string.
  endmethod.

  method get_objname_wo_namspace_with_z.
    split object_name at '/' into table data(splitted_obj_name_parts).
    data(object_name_wo_namespace) = splitted_obj_name_parts[ lines( splitted_obj_name_parts ) ].
    data(zname_of_obj) = to_lower( object_name_wo_namespace ).
    if not zname_of_obj cp `z*`.
      result = |z{ zname_of_obj }|.
    else.
      result = zname_of_obj.
    endif.
  endmethod.


  method generate_repo_folder.
    if p_repo = abap_true.
      "serialize only one repo folder
      if p_objtyp is initial or p_intf is initial or p_examp is initial.
        insert `Please fill out all fields (objecttype, interfacename, examplename)` into table report_log ##NO_TEXT.
        return.
      endif.
    endif.
    " generate zip folder
    me->zip = new cl_abap_zip( ).

    loop at objects assigning field-symbol(<object>).
      data(object_type_folder_name) = to_lower( <object>-object_type ).

      select single devclass from tadir where pgmid = 'R3TR' and obj_name = @<object>-example and object = @<object>-object_type into @data(example_obj_devclass).
      data(example_main_object) = value if_aff_object_file_handler=>ty_object( devclass  = example_obj_devclass obj_type = <object>-object_type obj_name = <object>-example ).
      if aff_factory is not initial.
        data(file_handler) = aff_factory->get_object_file_handler( ). " for testing purposes
      else.
        file_handler = cl_aff_factory=>get_object_file_handler( ).
      endif.
*      DATA(l_log) = NEW cl_aff_log( ).
*      DATA(example_files) = file_handler->serialize_objects( objects = VALUE #( ( example_main_object ) ) log = l_log ).
      data(example_files) = file_handler->serialize_objects( objects = value #( ( example_main_object ) ) log = aff_framework_log ).

      get_replacing_table_and_intfs(
        exporting name_of_intf_of_mainobj = conv #( <object>-interface ) example_files = example_files
        importing interfaces = data(interfaces)
      ).
      "adding the example files
      add_aff_files_to_zip( files = example_files filename = |{ object_type_folder_name }/examples/| replacing_table_string = replacing_table_string ).

      data intf_objects type if_aff_object_file_handler=>tt_objects.
      clear intf_objects.

      "generate type folder with all serialized interfaces (main and subobjects)
      loop at interfaces assigning field-symbol(<interface>).
        data(upper_intf) = to_upper( <interface> ).
        select single devclass from tadir where obj_name = @upper_intf and pgmid = 'R3TR' and object = 'INTF' into @data(intf_obj_devclass) .
        if intf_obj_devclass is initial.
          insert |{ upper_intf } is not found in table tadir. Package of the interface is unknown| into table report_log ##NO_TEXT.
        endif.
        append value #( devclass  = intf_obj_devclass obj_type  = 'INTF' obj_name = upper_intf ) to intf_objects.
      endloop.

*      DATA(intf_files) = file_handler->serialize_objects( objects = intf_objects log = l_log ).
      data(intf_files) = file_handler->serialize_objects( objects = intf_objects log = aff_framework_log ).

      add_aff_files_to_zip( files = intf_files filename = |{ object_type_folder_name }/type/| replacing_table_string = replacing_table_string ).

      "generate the schema(s) of the mainobject and all of its subobjects
      "add it to the zip folder
      loop at interfaces assigning <interface>.
        data(intfname) = <interface>.
        split intfname at `_` into table data(splitted_intfname).
        if lines( splitted_intfname ) < 4.
          insert |The schema for interface { <interface> } could not be created. Objecttype could not be derived from intfname.| into table report_log ##NO_TEXT.
          continue.
        endif.
        data(objecttype) = splitted_intfname[ lines( splitted_intfname ) - 1 ].

        data(found) = abap_false.
        select single @abap_true from tadir where obj_name = @<interface> into @found. "#EC CI_GENBUFF
        if found = abap_false.
          insert |The schema for interface { <interface> } could not be created.| into table report_log ##NO_TEXT.
          continue.
        endif.

        data(mainobjtype) = objecttype.
        if objecttype = `FUNC` or objecttype = `REPS`.
          mainobjtype = `FUGR`.
        elseif objecttype = `INDX`.
          mainobjtype = `TABL`.
        endif.

        data(format_version) = get_format_version_of_intfname( conv #( intfname ) ).
        data(schemid) = |https://github.com/SAP/abap-file-formats/blob/main/file-formats/{ to_lower( mainobjtype ) }/{ to_lower( objecttype ) }-v{ format_version }.json| ##NO_TEXT.
        if writer is initial or writer is instance of zcl_aff_writer_json_schema or writer is instance of zcl_aff_writer_xslt. "in testcase the writer is of type zif_aff_writer
          writer = new zcl_aff_writer_json_schema( schema_id = schemid format_version = format_version ).
        endif.
        if generator is initial or generator is instance of lcl_generator_helper. "in testcase we use ltc_generator
          generator = new lcl_generator_helper( writer ).
        endif.
        data(schema_content) = get_content( absolute_typename = |\\INTERFACE={ to_upper( intfname ) }\\TYPE=TY_MAIN| interfacename = conv #( intfname ) ).
        if schema_test_content is not initial."in test case sometimes a test schema content is injected
          schema_content = schema_test_content.
        endif.
        if schema_content is initial.
          insert |The schema for interface { intfname } could not be created.| into table report_log ##NO_TEXT.
        else.
          add_file_to_zip(  i_stringtab_content = schema_content
            i_file_name         = |{ object_type_folder_name }/{ to_lower( objecttype ) }-v{ format_version }.json| ##NO_TEXT
            i_error_text        = |The schema for interface { intfname } could not be created. Error when transforming schema content from string to xstring| ##NO_TEXT
          ).
        endif.
      endloop.
      if p_readm = abap_true.
        data(interfacename) = replace_names_in_string( content_as_string  = to_lower( intfname ) replacing_table_string = replacing_table_string ).
        data(examplename) = replace_names_in_string( content_as_string  = to_lower( <object>-example ) replacing_table_string = replacing_table_string ).
        data(readme) = value rswsourcet(
  ( |# { <object>-object_type } File Format| ) ##NO_TEXT
  ( `` )
  ( `File | Cardinality | Definition | Schema | Example`) ##NO_TEXT
  ( `:--- | :---  | :--- | :--- | :---` ) ##NO_TEXT
  ( |`<name>.{ object_type_folder_name }.json` \| 1 \| [`{ interfacename }.intf.abap`](./type/{ interfacename }.intf.abap) \| [`{ to_lower( objecttype ) }-v{ format_version }.json`](./{ to_lower( objecttype ) }-v{ format_version }.json)| &&
  | \| [`{ examplename }.{ object_type_folder_name }.json`](./examples/{ examplename }.{ object_type_folder_name }.json)| )
   ( `` ) ##NO_TEXT
   ) ##NO_TEXT.
        add_file_to_zip( i_stringtab_content = readme
                        i_file_name  = |{ object_type_folder_name }/README.md|
 i_error_text = |The readme for object { <object>-object_type } could not be created. Error when transforming readme content from string to xstring| ) ##NO_TEXT.
      endif.
    endloop.

    if whole_aff_folder = abap_true.
      "also serialize the two interfaces: if_aff_types_v1 and if_aff_oo_types_v1
      data two_interfaces type if_aff_object_file_handler=>tt_objects .
      append value #( devclass = 'SAFF_CORE' obj_name = 'IF_AFF_TYPES_V1' obj_type = 'INTF' ) to two_interfaces.
      append value #( devclass = 'SEO_AFF' obj_name = 'IF_AFF_OO_TYPES_V1' obj_type = 'INTF' ) to two_interfaces.

      data replacing_names type replacing_tab.
      replacing_names = value #(
       ( to_be_replaced = `if_aff_types_v1` replace_with = `zif_aff_types_v1` ) ##NO_TEXT
       ( to_be_replaced = `if_aff_oo_types_v1` replace_with = `zif_aff_oo_types_v1` ) ##NO_TEXT
      ) .

      loop at two_interfaces assigning field-symbol(<interf>).
        data(intf_files2) = file_handler->serialize_objects( objects = value #( ( <interf> ) ) log = aff_framework_log ).
        add_aff_files_to_zip( files = intf_files2 filename = `` replacing_table_string = replacing_names ).
      endloop.
    endif.

  endmethod.

  method add_file_to_zip.
    "convert the string_table to xstring
    data(text_handler) = new cl_aff_content_handler_text( ).
    try.
        data(xstring_content) = text_handler->if_aff_content_handler~serialize( i_stringtab_content ).
      catch zcx_aff_tools.
        insert i_error_text into table report_log.
        return.
    endtry.
    me->zip->add( name    = i_file_name
                content = xstring_content ).
  endmethod.

  method add_aff_files_to_zip.
    data(text_handler) = new cl_aff_content_handler_text( ).
    loop at files-files assigning field-symbol(<file>).
*     replace the filenames of the files
      data(file_name) = to_lower( <file>-file_name ).
      file_name = replace_names_in_string( content_as_string = file_name replacing_table_string = replacing_table_string ).

*     replace the content of the files
      data(file_content_xstring) = <file>-content.
      data content_as_string type string.
      try.
          text_handler->if_aff_content_handler~deserialize( exporting content = file_content_xstring importing data = content_as_string ).
          content_as_string = replace_names_in_string( content_as_string = content_as_string replacing_table_string = replacing_table_string ).
          file_content_xstring = text_handler->if_aff_content_handler~serialize( content_as_string ).
        catch zcx_aff_tools into data(exception).
          insert |Object names in file { <file>-file_name } could not be changed to 'z...'. { exception->get_text( ) }| into table report_log ##NO_TEXT.
      endtry.
      zip->add( name    = |{ filename }{ file_name }|
                content = file_content_xstring ).
    endloop.

  endmethod.

  method set_object_infos_in_ui.
    data dynpfields type standard table of dynpread.
    data(obj_type) = i_object-object_type.
    append value #( fieldname = 'P_OBJTYP'  fieldvalue = obj_type ) to dynpfields.

    data(intf_name) = i_object-interface.
    append value #( fieldname = 'P_INTF'  fieldvalue = intf_name ) to dynpfields.

    data(example_name) = i_object-example .
    append value #( fieldname = 'P_EXAMP'  fieldvalue = example_name ) to dynpfields.

    call function 'DYNP_VALUES_UPDATE'
      exporting
        dyname     = sy-repid
        dynumb     = sy-dynnr
      tables
        dynpfields = dynpfields.

  endmethod.

  method get_intfname_highest_version.
    clear intfname.
    clear format_version.
    data(string_to_search) = |IF_AFF_{ objecttype  }_%|.
    select obj_name from tadir where object = 'INTF' and obj_name like @string_to_search into table @data(intfs). "#EC CI_GENBUFF
* take the highest number
    data version_list type standard table of i.
    loop at intfs assigning field-symbol(<intf>).
      data(intf_format_version) = get_format_version_of_intfname( conv #( <intf> ) ).
      append intf_format_version to version_list.
    endloop.
    sort version_list descending.
    if lines( version_list ) > 0.
      format_version = version_list[ 1 ].
      string_to_search = |*{ version_list[ 1 ] }|.
      loop at intfs assigning <intf> where obj_name cp string_to_search.
        intfname = <intf>.
        exit.                                           "#EC CI_NOORDER
      endloop.
    else.
      intfname = |IF_AFF_{ objecttype  }_V1|.
      format_version = 1.
    endif.
  endmethod.

  method get_object_infos_by_objtype.
    get_intfname_highest_version(
      exporting
        objecttype     = objecttype
      importing
        intfname       = data(intfname)
        format_version = data(format_version)
    ).
    if objecttype = 'DOMA'.
      intfname = 'ZIF_AFF_DOMA_V1'.
    endif.
    data(examplename) = |Z_AFF_EXAMPLE_{ objecttype }|.
    if objecttype = 'NROB'.
      examplename = 'Z_AFF_NR'.
    endif.
    object = value #( object_type = objecttype interface = intfname example = examplename format_version = format_version ).
  endmethod.

  method get_object_infos_by_intfname.
    split intfname at '_' into table data(splitted_intfname).
    if lines( splitted_intfname ) >= 3.
      data(objecttype) = splitted_intfname[ 3 ].
      data(format_version) = get_format_version_of_intfname( intfname ).
      data(examplename) = |Z_AFF_EXAMPLE_{ objecttype }|.
      if objecttype = 'NROB'.
        examplename = 'Z_AFF_NR'.
      endif.
      object = value #( object_type = objecttype interface = intfname example = examplename format_version = format_version ).
    endif.
  endmethod.

  method get_object_infos_by_exmplname.
    if examplename is not initial.
      split examplename at '_' into table data(splitted_examplename).
      data(objecttype) = to_upper( splitted_examplename[ lines( splitted_examplename ) ] ).
      if objecttype is not initial.
        if strlen( objecttype ) < 4.
          return.
        endif.
        get_intfname_highest_version(
          exporting
            objecttype     = objecttype
          importing
            intfname       = data(intfname)
            format_version = data(format_version)
        ).
        if objecttype = 'NROB'.
          intfname = 'Z_AFF_NR'.
        endif.
      endif.
    endif.
    object = value #( object_type = objecttype interface = intfname example = examplename format_version = format_version ).
  endmethod.

  method replace_names_in_string.
    data(string_content) = content_as_string.
    loop at replacing_table_string assigning field-symbol(<replace_string>).
      find all occurrences of <replace_string>-to_be_replaced in string_content ignoring case results data(table).
*      replace all occurrences of <replace_string>-to_be_replaced in string_content with <replace_string>-replace_with ignoring case.
      data(counter_replaced) = 0.
      loop at table assigning field-symbol(<finding>).
        if <finding>-offset > 0.
          data(offset) = <finding>-offset + counter_replaced.
          data(before_offset) = offset - 1.
          data(char_before_offset) = to_lower( string_content+before_offset(1) ).
        else.
          char_before_offset = ' '.
          offset = 0.
        endif.
        if not to_lower( char_before_offset ) eq 'z'.
*          string_content = insert( val = string_content sub = 'z' off = offset ).
*         make the object names to lower
          replace section offset offset length <finding>-length of string_content with <replace_string>-replace_with.
          counter_replaced += 1.
        endif.
      endloop.
    endloop.
    content = string_content.
  endmethod.


  method get_dynpro_value.
    data dynpfields type standard table of dynpread.
    append value #( fieldname = to_upper( fieldname ) ) to dynpfields.

    call function 'DYNP_VALUES_READ'
      exporting
        dyname     = sy-repid
        dynumb     = sy-dynnr
      tables
        dynpfields = dynpfields
      exceptions
        others     = 1.

    if sy-subrc = 0.
      fieldvalue = dynpfields[ 1 ]-fieldvalue.
    else.
      clear fieldvalue.
    endif.

  endmethod.

  method set_value_help_result_to_field.
    data: i_f4_result type standard table of ddshretval.
    data: w_f4_result type ddshretval.
    data dynpfields type standard table of dynpread.
    if value_help_result_table is not initial.
      call function 'F4IF_INT_TABLE_VALUE_REQUEST'
        exporting
          retfield        = 'P_PROG'
          value_org       = 'S'
        tables
          value_tab       = value_help_result_table
          return_tab      = i_f4_result
        exceptions
          parameter_error = 1
          no_values_found = 2
          others          = 3.

      if sy-subrc = 0.
        read table i_f4_result into w_f4_result index 1.

        if sy-subrc = 0.
          chosen_value = w_f4_result-fieldval.
          append value #( fieldname = fieldname  fieldvalue = w_f4_result-fieldval ) to dynpfields.
          call function 'DYNP_VALUES_UPDATE'
            exporting
              dyname     = sy-repid
              dynumb     = sy-dynnr
            tables
              dynpfields = dynpfields.
        endif.
      endif.
    endif.
  endmethod.

  method create_the_variable_dynamicaly.
    data r_typedescr type ref to cl_abap_typedescr.
    data r_elemdescr type ref to cl_abap_structdescr.
    cl_abap_typedescr=>describe_by_name( exporting  p_name = absolute_typename receiving p_descr_ref = r_typedescr exceptions type_not_found = 1 ).
    if sy-subrc = 1.
      data(class_typename) = replace( val = absolute_typename sub = '\INTERFACE=' with = '\CLASS=' ).
      cl_abap_typedescr=>describe_by_name( exporting  p_name = class_typename receiving p_descr_ref = r_typedescr exceptions type_not_found = 1 ).
      if sy-subrc = 1.
        insert |Type { absolute_typename } was not found. Either interface or type doesnt exist.| into table report_log ##NO_TEXT.
        raise exception type zcx_aff_tools.
      endif.
    endif.
    r_elemdescr ?= r_typedescr.
    data r_field type ref to data.
    field-symbols <field> type any.
    create data r_field type handle r_elemdescr.
    assign r_field->* to <field>.
    get reference of <field> into variable.
  endmethod.

  method get_table_with_all_githubtypes.
    append 'CHKC' to type_table.
    append 'CHKO' to type_table.
    append 'CHKV' to type_table.
    append 'CLAS' to type_table.
    append 'DDLS' to type_table.
    append 'ENHO' to type_table.
    append 'ENHS' to type_table.
    append 'FUGR' to type_table.
    append 'INTF' to type_table.
    append 'NROB' to type_table.
    append 'DDLX' to type_table.
    append 'DOMA' to type_table.
  endmethod.

  method get_format_version_of_intfname.
    split intfname  at '_' into table data(splitted_intfname).
    data(last) = splitted_intfname[ lines( splitted_intfname ) ].
    replace all occurrences of 'v' in last with ''.
    replace all occurrences of 'V' in last with ''.
    try.
        data(regx) = '[[:alpha:]]+'.
*      check if the token only contains digits
        data(contains_chars) = xsdbool( count( val = last regex = regx ) > 0 ) ##REGEX_POSIX.
        if contains_chars = abap_false.
          format_version = last.
        else.
          insert |Formatversion couldnt be derived from interface { intfname }. Format version 1 was assumed.| into table report_log ##NO_TEXT.
          format_version = 1.
        endif.
      catch cx_sy_conversion_no_number.
        "if the intfname is not correct we use format_version 1
        insert |Formatversion couldnt be derived from interface { intfname }. Format version 1 was assumed.| into table report_log ##NO_TEXT.
        format_version = 1.
    endtry.
  endmethod.

  method get_schema_or_xslt_content.
    if p_intf is initial or p_type is initial or p_objtyp is initial.
      insert `Please fill out all fields (interfacename, objecttype, abaptypename)` into table report_log ##NO_TEXT.
      clear content.
      return.
    endif.

    data(mainobjtype) = p_objtyp.
    if p_objtyp = 'REPS' or p_objtyp = 'FUNC'.
      mainobjtype = 'FUGR'.
    elseif p_objtyp = 'INDX'.
      mainobjtype = 'TABL'.
    endif.

    data(format_version) = get_format_version_of_intfname( conv #( p_intf ) ).
    data(schemid) = |https://github.com/SAP/abap-file-formats/blob/main/file-formats/{ to_lower( mainobjtype ) }/{ to_lower( p_objtyp ) }-v{ format_version }.json| ##NO_TEXT.

    if writer is initial or writer is instance of zcl_aff_writer_json_schema or writer is instance of zcl_aff_writer_xslt. "we are not in test scenario
      if p_schema = abap_true.
        writer = new zcl_aff_writer_json_schema( schema_id  = schemid format_version = format_version ).
      elseif p_xslt = abap_true.
        writer = new zcl_aff_writer_xslt( ).
      endif.
    endif.
    if generator is initial or generator is instance of lcl_generator_helper."we are not in test scenario
      generator = new lcl_generator_helper( writer ).
    endif.
    content = get_content( absolute_typename = |\\INTERFACE={ p_intf }\\TYPE={ p_type }| interfacename = conv #( p_intf ) ).
  endmethod .

  method get_content.
    try.
        data(type) = create_the_variable_dynamicaly( absolute_typename ).
      catch zcx_aff_tools.
        clear content.
        return.
    endtry.
    "type was succesfully created (else an exception in create_the_variable_dynamicaly( ) would be raised)
    field-symbols <field> type any.
    assign type->* to <field>.
    " getting the XSLT/Schema of the type
    try.
        content = generator->generate_type( <field> ).
      catch zcx_aff_tools .
        clear content.
        insert |The generator couldn't generate the schema/XSLT for type { absolute_typename }| into table report_log ##NO_TEXT.
        return.
    endtry.
    "content was succesfully created ( else an exception would be raised)
    "check if the content is valid
    data(is_valid) = writer->validate( source = content log = me->generator_log ).
    if is_valid = abap_false.
      insert |ATTENTION: The created schema/xslt for type { absolute_typename } is not valid.| into table report_log ##NO_TEXT.
    endif.

*    DATA(generator_log) = NEW zcl_aff_log( ).
    generator_log->join( generator->get_log( ) ).
*    LOOP AT generator->get_log( )->get_messages( ) ASSIGNING FIELD-SYMBOL(<msg>).
*      IF <msg>-type = zif_aff_log=>c_message_type-info.
**        generator_log->zif_aff_log~add_info( message = <msg>-message component_name = object->if_aff_obj~get_name( ) ).
*        generator_log->add_info( message = <msg>-message component_name = object->if_aff_obj~get_name( ) ).
*      ELSEIF <msg>-type = zif_aff_log=>c_message_type-warning.
**        generator_log->zif_aff_log~add_warning( message = <msg>-message component_name = object->if_aff_obj~get_name( ) ).
*        generator_log->add_warning( message = <msg>-message component_name = object->if_aff_obj~get_name( ) ).
*      ELSEIF <msg>-type = zif_aff_log=>c_message_type-error.
**        generator_log->zif_aff_log~add_error( message = <msg>-message component_name = object->if_aff_obj~get_name( ) ).
*        generator_log->add_error( message = <msg>-message component_name = object->if_aff_obj~get_name( ) ).
*      ENDIF.
*    ENDLOOP.
*    me->generator_log->join( generator_log ).
  endmethod.

  method create_schema_xslt_zip.
    r_zip = new cl_abap_zip( ).
    data(text_handler) = new cl_aff_content_handler_text( ).
    try.
        data(content_as_xstring) = text_handler->if_aff_content_handler~serialize( content ).
      catch zcx_aff_tools.
        clear r_zip.
        insert `Schema/Xslt could not be created. Error when serializing string to xstring` into table report_log ##NO_TEXT.
        return.
    endtry.
    data(filename) = |{ to_lower( p_objtyp ) }_schema.txt| ##NO_TEXT.
    if p_xslt = abap_true.
      filename = |{ to_lower( p_objtyp ) }_xslt.txt| ##NO_TEXT.
    endif.
    r_zip->add( name    = |{ filename }|
                content = content_as_xstring ).
  endmethod.

  method print_logs.
    if lines( report_log ) > 0.
      write: / `Messages of the report:` ##NO_TEXT.
      loop at report_log assigning field-symbol(<log_msg>).
        write: / <log_msg>.
      endloop.
    endif.

    if lines( generator_log->get_messages( ) ) > 0.
      skip 1.
      write: / `Messages schema/ST Generator:` ##NO_TEXT.
      loop at generator_log->get_messages( ) assigning field-symbol(<WRITER_log_message>).
        write: / |{ <WRITER_log_message>-type } { <WRITER_log_message>-component_name width = 40 align = left pad = ' ' } { <WRITER_log_message>-text }|.
      endloop.
    endif.

    if lines(  aff_framework_log->get_messages( ) ) > 0.
      skip 1.
      write: / `Messages of the AFF Object Handlers:` ##NO_TEXT.
      loop at aff_framework_log->get_messages( ) assigning field-symbol(<framework_log_message>).
        if not ( <framework_log_message>-message-msgid = 'SAFF_CORE' and
               ( <framework_log_message>-message-msgno = '026' ) or
               ( <framework_log_message>-message-msgno = '027' ) ).
          write: / |{ <framework_log_message>-type } { <framework_log_message>-text }|.
        endif.
      endloop.
    endif.
  endmethod.

  method object_as_string.
    if object-devclass is not initial.
      data(package) = |{ object-devclass width = 20 align = left pad = ' ' }| ##NUMBER_OK.
    endif.
    data(objname) = |{ object-obj_name width = 30 align = left pad = ' ' }| ##NUMBER_OK.
    data(objtype) = |{ object-obj_type width = 4 align = left pad = ' ' }|.
    result = |{ package }{ objname } { objtype }|.

    if object-sub_name is not initial and object-sub_type is not initial.
      data(subname) = |{ object-sub_name width = 50 align = left pad = ' ' }| ##NUMBER_OK.
      data(subtype) = |{ object-sub_type width = 4 align = left pad = ' ' }|.
      result = |{ package }{ objname } { objtype } { subname } { subtype }|.
    endif.
  endmethod.


  method start_of_selection.

    p_objtyp = to_upper( p_objtyp ).
    p_intf   = to_upper( p_intf ).
    p_type   = to_upper( p_type ).
    p_examp  = to_upper( p_examp ).

    if p_xslt = abap_true or p_schema = abap_true.
      "serialize ST or schema
      xslt_schema_content  = get_schema_or_xslt_content( ).
      if p_disk = abap_true.
        zip = create_schema_xslt_zip( xslt_schema_content ).
        clear xslt_schema_content.
      endif.
    elseif p_repo = abap_true.
      "serialize one repo folder
      generate_repo_folder( value #( ( object_type  = p_objtyp interface =  p_intf example =  p_examp ) ) ).
    elseif p_multre = abap_true.
      "    serialize multiple repo folders
      data objects type aff_objects_table.
      loop at p_multob assigning field-symbol(<object>).
        data(object1) = get_object_infos_by_objtype( conv #( <object>-low ) ).
        append object1 to objects.
      endloop.

      generate_repo_folder( objects ).
    elseif p_whole = abap_true.
      "write complete file-formats folder
      data(all_aff_objects) = get_table_with_all_githubtypes( ).
      data aff_objects type aff_objects_table.
      loop at all_aff_objects assigning field-symbol(<aff_object>).
        data(aff_object) = get_object_infos_by_objtype( <aff_object> ).
        append aff_object to aff_objects.
      endloop.

      generate_repo_folder( objects  = aff_objects whole_aff_folder = abap_true ).
    endif.

    if xslt_schema_content is not initial. "show it in the console
      gui_frontend_service->write( xslt_schema_content ).
      gui_frontend_service->display( ).
    elseif zip is not initial. "write it to disk
      data(zip_archive) = zip->save( ).
      data(zipname) = to_lower( p_objtyp ).
      if p_multre = abap_true or p_whole = abap_true.
        zipname = `file-formats` ##NO_TEXT.
      endif.
      write_to_zip( zip_archive = zip_archive zipname = zipname ).
    endif.

  endmethod.

  method on_value_request_for_type.
    data(typename_value) = get_dynpro_value( fieldname  = 'P_TYPE' ).
    typename_value = to_upper( typename_value ) .

    data(intfname_value) = get_dynpro_value( fieldname  = 'P_INTF' ).
    intfname_value = to_upper( intfname_value ) .

    if typename_value is initial and intfname_value is initial.
      return.
    elseif typename_value is initial and intfname_value is not initial.
      select cmpname from seocompo  where clsname = @intfname_value and cmptype = 3 into table @data(value_help_result)
      up to 30 rows  bypassing buffer ##NUMBER_OK.      "#EC CI_NOORDER
    else. "both are filled
      data(typename_with_percent) = |%{ to_upper( typename_value ) }%|.
      replace all occurrences of '*' in typename_with_percent with `%`.
      select cmpname from seocompo  where clsname = @intfname_value and cmptype = 3 and cmpname like @typename_with_percent
        into table @value_help_result up to 30 rows bypassing buffer ##NUMBER_OK. "#EC CI_NOORDER
    endif.

    set_value_help_result_to_field( fieldname = 'P_TYPE' value_help_result_table = value_help_result ).
  endmethod.

  method on_value_request_for_example.
    data(example_value) = get_dynpro_value( fieldname  =  `P_EXAMP` ).
    example_value = to_upper( example_value ) .

    data(objtype_value) = get_dynpro_value( fieldname  = 'P_OBJTYP' ).
    objtype_value = to_upper( objtype_value ) .

    if example_value is initial and objtype_value is not initial.
      select obj_name from tadir where object = @objtype_value and devclass = 'TEST_AFF_EXAMPLES' into table @data(value_help_result_table) up to 50 rows bypassing buffer ##NUMBER_OK. "#EC CI_NOORDER
    elseif example_value is not initial and objtype_value is not initial.
*  both are filled
* The user does not have to type "*" on beginning and ending of the obj_name pattern, we add it automatically
      data(example_with_percent) = |%{ to_upper( example_value ) }%|.
      replace all occurrences of '*' in example_with_percent with `%`.

      " Retrieve object names from tadir which match the search pattern entered in UI Element obj_name
      select obj_name from tadir where object = @objtype_value and obj_name like @example_with_percent into table @value_help_result_table up to 50 rows bypassing buffer ##NUMBER_OK. "#EC CI_NOORDER                         "#EC CI_NOORDER
    else.
*  both are initial
      select obj_name from tadir where devclass = 'TEST_AFF_EXAMPLES' into table @value_help_result_table up to 50 rows bypassing buffer ##NUMBER_OK. "#EC CI_NOORDER
    endif.

    data(example1) = set_value_help_result_to_field( fieldname = `P_EXAMP` value_help_result_table = value_help_result_table ).
    if example1 is not initial.
      data(object_infos) = get_object_infos_by_exmplname( example1 ).

      set_object_infos_in_ui( object_infos ).
      p_intf = object_infos-interface.
      p_objtyp = object_infos-object_type.
      p_examp = object_infos-example.
    endif.

  endmethod.

  method modify_screen.
    types: screen_name  type c length 132,
           screen_names type standard table of screen_name.
    data hidden_elements type screen_names.
    clear hidden_elements.

    if p_schema = abap_true or p_xslt = abap_true.
      append 'P_EXAMP' to hidden_elements.
      append 'P_MULTOB' to hidden_elements.
      append 'P_READM' to hidden_elements.
    endif.
    if p_repo = abap_true.
      append 'P_TYPE' to hidden_elements.
      append 'P_MULTOB' to hidden_elements.
      append 'P_CONSOL' to hidden_elements.
      append 'P_DISK' to hidden_elements.
    endif.
    if p_multre = abap_true or p_whole = abap_true.
      append 'P_OBJTYP' to hidden_elements.
      append 'P_EXAMP' to hidden_elements.
      append 'P_TYPE' to hidden_elements.
      append 'P_INTF' to hidden_elements.
      append 'P_CONSOL' to hidden_elements.
      append 'P_DISK' to hidden_elements.
      if p_whole = abap_true.
        append 'P_MULTOB' to hidden_elements.
      endif.
    endif.

    loop at screen.
      data(element_name) = screen-name.
*        %_UI_INPUT_%_APP_%-TEXT
      replace all occurrences of '%_' in element_name with ``.
      replace all occurrences of '_APP_%-TEXT' in element_name with ``.
      replace all occurrences of '_APP_%-OPTI_PUSH' in element_name with ``.
      replace all occurrences of '_APP_%-VALU_PUSH' in element_name with ``.
      replace all occurrences of '-LOW' in element_name with ``.

      find first occurrence of regex element_name in table hidden_elements ##REGEX_POSIX. "or line exists
      if sy-subrc = 0.
        screen-active = 0.
        modify screen.
      else.
        screen-active = 1.
        modify screen.
      endif.
    endloop.
  endmethod.

  method on_value_request_for_objtype.
    data(objtype_value) = get_dynpro_value( fieldname  =  `P_OBJTYP` ).
    objtype_value = to_upper( objtype_value ) .
    if objtype_value is initial.
*  put all Types into the value help
      select distinct object from e071 into table @data(value_help_result_table) up to 50 rows bypassing buffer order by object ##NUMBER_OK. "#EC CI_NOWHERE
      loop at get_table_with_all_githubtypes( ) assigning field-symbol(<type>).
        insert <type> into value_help_result_table index 1.
      endloop.
    else.
* The user does not have to type "*" on beginning and ending of the obj type pattern, we add it automatically
      data(objtype_with_percent) = |%{ to_upper( objtype_value ) }%|.
      replace all occurrences of '*' in objtype_with_percent with `%`.

      " Retrieve object which match the search pattern entered in UI Element objtype
      select distinct object from e071 into table @value_help_result_table up to 30 rows bypassing buffer where object like @objtype_with_percent ##NUMBER_OK. "#EC CI_NOORDER "#EC CI_NOFIELD
    endif.

    data(objtype) = set_value_help_result_to_field(  fieldname = `P_OBJTYP` value_help_result_table = value_help_result_table ).

    if objtype is not initial.
      data(object_infos) = get_object_infos_by_objtype( objtype ).

      set_object_infos_in_ui( object_infos ).
      p_intf = object_infos-interface.
      p_objtyp = object_infos-object_type.
      p_examp = object_infos-example.
    endif.
  endmethod.

  method on_value_request_for_intfname.
    data(intfname_value) = get_dynpro_value( fieldname  =  `P_INTF` ).
    intfname_value = to_upper( intfname_value ) .

    data(objtype_value) = get_dynpro_value( fieldname  =  `P_OBJTYP` ).
    objtype_value = to_upper( objtype_value ) .

    if intfname_value is initial.
      data search_pattern type string.
      search_pattern = |IF_AFF%{ objtype_value }%|.
*  put all IF_AFF* Interfaces into the value help
      select obj_name  from tadir where obj_name like @search_pattern and devclass <> `SAFF_CORE` and object = `INTF`
      into table @data(value_help_result_table) up to 50 rows bypassing buffer ##NUMBER_OK. "#EC CI_NOORDER
    else.
* The user does not have to type "*" on beginning and ending of the obj_name pattern, we add it automatically
      data(intfname_with_percent) = |%{ to_upper( intfname_value ) }%|.
      replace all occurrences of '*' in intfname_with_percent with `%`.

      " Retrieve object names from tadir which match the search pattern entered in UI Element obj_name
      select obj_name from tadir into table @value_help_result_table up to 30 rows bypassing buffer
      where object = `INTF` and obj_name like @intfname_with_percent order by obj_name ##NUMBER_OK. "#EC CI_NOORDER
    endif.

    data(intfname1) = set_value_help_result_to_field( fieldname = `P_INTF` value_help_result_table = value_help_result_table ).

    if intfname1 is not initial.
      data(object_infos) = get_object_infos_by_intfname( intfname1 ).

      set_object_infos_in_ui( object_infos ).
    endif.
  endmethod.

  method at_selection_screen.
    if p_objtyp is not initial and p_intf is initial and p_examp is initial.
      p_objtyp = to_upper( p_objtyp ).
      data(objinfos) = get_object_infos_by_objtype( conv #( p_objtyp ) ).
      set_object_infos_in_ui( objinfos ).
      p_intf = objinfos-interface.
      p_examp = objinfos-example.
    elseif p_objtyp is initial and p_intf is not initial and p_examp is initial.
      p_intf = to_upper( p_intf ).
      objinfos = get_object_infos_by_intfname( conv #( p_intf ) ).
      set_object_infos_in_ui( objinfos ).
      p_objtyp = objinfos-object_type.
      p_examp = objinfos-example.
    elseif p_objtyp is  initial and p_intf is initial and p_examp is not initial.
      p_examp = to_upper( p_examp ).
      objinfos = get_object_infos_by_exmplname( conv #( p_examp ) ).
      set_object_infos_in_ui( objinfos ).
      p_intf = objinfos-interface.
      p_objtyp = objinfos-object_type.
    endif.
  endmethod.

  method constructor.
    if generator is supplied.
      me->generator = generator.
    endif.
    if writer is supplied.
      me->writer = writer.
    endif.
    if aff_factory is supplied.
      me->aff_factory = aff_factory.
    endif.
    generator_log = new zcl_aff_log( ).
    aff_framework_log = new cl_aff_log( ).
    if i_gui_frontend is supplied.
      gui_frontend_service = i_gui_frontend.
    else.
      gui_frontend_service = new lcl_gui_frontend( ).
    endif.
  endmethod.

  method set_parameters.
    p_schema = i_schema.
    p_xslt  =      i_xslt  .
    p_repo  =      i_repo  .
    p_whole =      i_whole .
    p_multre =     i_multre.
    p_objtyp =     i_objtyp.
    p_intf  =      i_intf  .
    p_type  =      i_type  .
    p_examp =      i_examp .
    p_consol =     i_consol.
    p_disk  =      i_disk  .
    p_readm  =      i_readm  .

    data object like line of p_multob.
    loop at i_multob assigning field-symbol(<type>).
      object-option = 'EQ'.
      object-sign = 'I'.
      object-low = <type>.
      append object to p_multob.
    endloop.
  endmethod.

  method set_schema_test_content.
    me->schema_test_content = schema_test_content.
  endmethod.

endclass.

class ltc_generator_double definition final for testing.
  public section .
    interfaces lif_generator.
    methods constructor
      importing
        log_to_return                type ref to zif_aff_log
        generate_type_will_raise_err type abap_bool optional.
    data log_to_return type ref to zif_aff_log.
    data generate_type_will_raise_err type abap_bool.
endclass.
class ltc_generator_double implementation.

  method lif_generator~generate_type.
    if generate_type_will_raise_err = abap_true.
      raise exception type zcx_aff_tools.
    else.
      data(type_description) = cl_abap_typedescr=>describe_by_data( data ).
      data(absolutename) = type_description->absolute_name.
*   \INTERFACE=IF_AFF_CHKC_V1\TYPE=TY_MAIN
      split absolutename at '\' into table data(splitted).
      data(objectpart) = splitted[ 2 ].
      replace 'INTERFACE=' in objectpart with ''.
      split objectpart at '_' into table data(splitted2).
      data(objecttype) = splitted2[ lines( splitted2 ) - 1 ].
      result = value #(
          ( |Test ST/Schema for { objecttype }| )
      ).
    endif.
  endmethod.

  method lif_generator~get_log.
    log = log_to_return.
  endmethod.

  method constructor.
    me->log_to_return = log_to_return.
    me->generate_type_will_raise_err = generate_type_will_raise_err.
  endmethod.

endclass.


class ltc_generator definition final for testing
  duration short risk level harmless.

  private section.
    data cut type ref to lcl_generator.
    data generator_double type ref to lif_generator.
    data writer_double type ref to zif_aff_writer.
    data writer_log type ref to zcl_aff_log.
    data generator_log type ref to zcl_aff_log.
    data aff_factory_double type ref to if_aff_factory.
    data file_handler_double type ref to if_aff_object_file_handler.
    data expected_log_messages type zif_aff_log=>tt_log_out.
    data expected_report_log type stringtab.
    data gui_frontend type ref to ltc_gui_frontend.

    constants c_aff_example_intf type string value 'AFF_EXAMPLE_INTF' ##NO_TEXT.
    constants c_aff_example_intf_nspace type string value '/NAMESPACE/AFF_EXAMPLE_INTF' ##NO_TEXT.
    constants c_example_intf_package type string value 'TEST_AFF_EXAMPLES' ##NO_TEXT.
    constants c_intf type string value 'IF_AFF_INTF_V1' ##NO_TEXT.
    constants c_intf_w_namespace type string value '/NAMESP/IF_AFF_INTF_V1' ##NO_TEXT.

    methods setup.

    methods configure_file_handler
      importing objects type if_aff_object_file_handler=>tt_objects
      raising
                zcx_aff_tools.

    methods assert_file_content
      importing file_name_tab type stringtab
                zip           type ref to cl_abap_zip
      raising
                zcx_aff_tools .

    methods insert_objects_into_tadir importing objects type if_aff_object_file_handler=>tt_objects.

    methods create_new_cut_with_new_params
      importing
        i_schema type abap_bool optional
        i_xslt   type abap_bool optional
        i_repo   type abap_bool optional
        i_whole  type abap_bool optional
        i_multre type abap_bool optional
        i_objtyp type trobjtype optional
        i_intf   type sobj_name optional
        i_type   type sobj_name optional
        i_examp  type sobj_name optional
        i_consol type abap_bool optional
        i_disk   type abap_bool optional
        i_multob type stringtab optional.

    methods assert_logs_and_file_handler.
    methods schema_console_intf for testing raising cx_static_check.
    methods schema_zip_intf for testing raising cx_static_check.
    methods xslt_console_intf for testing raising cx_static_check.
    methods xslt_zip_intf for testing raising cx_static_check.
    methods repo_zip_intf for testing raising cx_static_check.
    methods whole_repo_zip for testing raising cx_static_check.
    methods multirepo_zip for testing raising cx_static_check.
    methods error_not_all_paramtrs_supplid for testing raising cx_static_check.
    methods generate_type_raises_error for testing raising cx_static_check.
    methods writer_validate_returns_false for testing raising cx_static_check.
    methods schema_console_func for testing raising cx_static_check.
    methods schema_zip_func for testing raising cx_static_check.
    methods xslt_consl_error_intf_not_exis for testing raising cx_static_check.
    methods xslt_consl_error_type_not_exis for testing raising cx_static_check.
    methods xsl_cons_intf_vers_not_readabl for testing raising cx_static_check.
    methods repo_zip_fugr for testing raising cx_static_check.
    methods repo_zip_intf_w_namespace for testing raising cx_static_check.
    methods repo_zip_tabl for testing raising cx_static_check.
    class-data function_test_environment type ref to if_function_test_environment.
    class-data environment type ref to if_osql_test_environment.
    class-methods class_setup.
    class-methods class_teardown.

endclass.

class ltc_generator implementation.

  method class_setup.
    " keep in mind, the test doubles created for the given function modules would be active for the entire test session
    " and any CALL FUNCTION statement on actual function module would get replaced with the corresponding test double in the session.
    function_test_environment = cl_function_test_environment=>create( value #( ( 'DYNP_VALUES_UPDATE' )
                                                                               ( 'DYNP_VALUES_READ' )
                                                                               ( 'F4IF_INT_TABLE_VALUE_REQUEST' )
                                                                               ) ).
    environment = cl_osql_test_environment=>create(  value #( ( 'TADIR' ) ) ).

  endmethod.

  method class_teardown.
    if environment is bound.
      environment->destroy( ).
    endif.
  endmethod.

  method setup.
    " clear all configurations on test doubles
    function_test_environment->clear_doubles( ).
    environment->clear_doubles( ).

    writer_double ?= cl_abap_testdouble=>create( 'zif_aff_writer' ).
    cl_abap_testdouble=>configure_call( writer_double )->returning( abap_true )->ignore_all_parameters( ).
    writer_double->validate( source = value #( ) log = new zcl_aff_log( ) ).

    file_handler_double ?= cl_abap_testdouble=>create( 'IF_AFF_OBJECT_FILE_HANDLER' ).

    data objects type if_aff_object_file_handler=>tt_objects.
    objects = value #(
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
        ( obj_name = 'Z_AFF_NR' devclass = c_example_intf_package obj_type = 'NROB' )
    ).
    try.
        configure_file_handler( objects ).
      catch zcx_aff_tools.
        cl_abap_unit_assert=>fail( ).
    endtry.
    insert_objects_into_tadir( objects ).

    aff_factory_double ?= cl_abap_testdouble=>create( 'IF_AFF_FACTORY' ).
    cl_abap_testdouble=>configure_call( aff_factory_double )->returning( file_handler_double ).
    aff_factory_double->get_object_file_handler( ).

    writer_log = new zcl_aff_log( ).
    writer_log->zif_aff_log~add_info( message = value #(  msgty = 'I'  msgv1 = 'Writer Log' ) component_name = value #( ) ).

    cl_abap_testdouble=>configure_call( writer_double )->returning( writer_log ).
    writer_double->get_log( ).

    generator_log = new zcl_aff_log( ).
    generator_log->zif_aff_log~add_info( message = value #(  msgty = 'I' msgv1 = 'Generator Log' ) component_name = value #( ) ).
    generator_double = new ltc_generator_double( generator_log ).

    gui_frontend = new ltc_gui_frontend( ).

    cut = new lcl_generator(
      aff_factory    = aff_factory_double
      generator      = generator_double
      writer         = writer_double
      i_gui_frontend = gui_frontend
    ).

    insert lines of generator_double->get_log( )->get_messages( ) into table expected_log_messages.

  endmethod.

  method assert_logs_and_file_handler.
    cl_abap_testdouble=>verify_expectations( file_handler_double ).
    cl_abap_unit_assert=>assert_equals( act = cut->report_log exp = expected_report_log ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut->generator_log->get_messages( ) ) exp = lines( expected_log_messages ) ) .
    loop at expected_log_messages assigning field-symbol(<exp_msg>).
      read table cut->generator_log->get_messages( ) with key text = <exp_msg>-text transporting no fields.
      if sy-subrc <> 0.
        cl_abap_unit_assert=>fail( ).
      endif.
    endloop.
  endmethod.

  method insert_objects_into_tadir.

    data tadir type standard table of tadir.
    loop at objects assigning field-symbol(<object>).
      append value #( pgmid = 'R3TR' object = <object>-obj_type obj_name = <object>-obj_name devclass = <object>-devclass ) to tadir.
    endloop.
    environment->insert_test_data( tadir ).

  endmethod.

  method configure_file_handler.
    data files type if_aff_object_file_handler=>ty_object_files.
    data(text_handler) = new cl_aff_content_handler_text( ).

    loop at objects assigning field-symbol(<object>).
      data(file_name) = |{ <object>-obj_name }.{ <object>-obj_type }.json|.
      if <object>-obj_name = c_aff_example_intf_nspace.
        file_name = |(NAMESPACE)AFF_EXAMPLE_INTF.{ <object>-obj_type }.json|.
      elseif <object>-obj_name = c_intf_w_namespace.
        file_name = |(NAMESP)IF_AFF_INTF_V1.{ <object>-obj_type }.json|.
      endif.
      data(file_content) = text_handler->if_aff_content_handler~serialize( |File of { <object>-obj_name }| ).
      files = value #(
          object_to_file_name = value #(
              ( object = <object> file_name = file_name )
          )
        files = value #(
         ( obj_type = <object>-obj_type obj_name = <object>-obj_name file_name = file_name content = file_content )
        ) )  .
      cl_abap_testdouble=>configure_call( file_handler_double )->returning( files )->ignore_parameter( name = 'LOG' ).
      file_handler_double->serialize_objects( objects =  value #( ( <object> ) ) log = new cl_aff_log( )  ).
    endloop.

    file_name = `file_of_reps_func_fugr.json`.
    file_content = text_handler->if_aff_content_handler~serialize( `File of REPS, FUNC, FUGR` ).
    files = value #(
        object_to_file_name = value #(
            ( object = <object> file_name = file_name )
        )
      files = value #(
       ( obj_type = <object>-obj_type obj_name = <object>-obj_name file_name = file_name content = file_content )
      ) )  .
    cl_abap_testdouble=>configure_call( file_handler_double )->returning( files )->ignore_parameter( name = 'LOG' ).
    file_handler_double->serialize_objects( objects = value #(
                                            ( obj_name = 'IF_AFF_FUGR_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
                                            ( obj_name = 'IF_AFF_FUNC_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
                                            ( obj_name = 'IF_AFF_REPS_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
                                            )
                                            log     = new cl_aff_log( ) ).
    file_name = `file_of_indx_tabl.json`.
    file_content = text_handler->if_aff_content_handler~serialize( `File of INDX, TABL` ).
    files = value #(
        object_to_file_name = value #(
            ( object = <object> file_name = file_name )
        )
      files = value #(
       ( obj_type = <object>-obj_type obj_name = <object>-obj_name file_name = file_name content = file_content )
      ) )  .
    cl_abap_testdouble=>configure_call( file_handler_double )->returning( files )->ignore_parameter( name = 'LOG' ).
    file_handler_double->serialize_objects( objects = value #(
                                            ( obj_name = 'IF_AFF_TABL_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
                                            ( obj_name = 'IF_AFF_INDX_V1' devclass = 'SEO_AFF' obj_type = 'INTF' )
                                            )
                                            log     = new cl_aff_log( ) ).
  endmethod.

  method xslt_console_intf.

    cut->set_parameters(
      i_xslt   = abap_true
      i_objtyp = 'INTF'
      i_intf   = conv #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = value #( )
    ).
    "When
    cut->start_of_selection( ).
    "Then
    expected_report_log = value #( ).
    assert_logs_and_file_handler( ).

  endmethod.

  method xsl_cons_intf_vers_not_readabl.

    cut->set_parameters(
      i_xslt   = abap_true
      i_objtyp = 'INTF'
      i_intf   = 'IF_AFF_INTF_V1X' "version not readable
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = value #( )
    ).
    "When
    cut->start_of_selection( ).
    "Then
    expected_report_log = value #(
  ( `Formatversion couldnt be derived from interface IF_AFF_INTF_V1X. Format version 1 was assumed.` )
  ( `Type \INTERFACE=IF_AFF_INTF_V1X\TYPE=TY_MAIN was not found. Either interface or type doesnt exist.` )
  ).
    clear expected_log_messages.
    assert_logs_and_file_handler( ).

  endmethod.

  method xslt_consl_error_intf_not_exis.

    cut->set_parameters(
      i_xslt   = abap_true
      i_objtyp = 'INTF'
      i_intf   = 'IF_AFF_INTF_V99999' "interface does not exist in system
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = value #( )
    ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_initial( cut->zip ).

    expected_report_log = value #(
  ( `Type \INTERFACE=IF_AFF_INTF_V99999\TYPE=TY_MAIN was not found. Either interface or type doesnt exist.` )
  ).
    clear expected_log_messages.
    assert_logs_and_file_handler( ).

  endmethod.

  method xslt_consl_error_type_not_exis.

    cut->set_parameters(
      i_xslt   = abap_true
      i_objtyp = 'INTF'
      i_intf   = 'IF_AFF_INTF_V1'
      i_type   = 'TY_BLABLA' "type does not exist in interface IF_AFF_INTF_V1
      i_examp  = conv #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = value #( )
    ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_initial( cut->zip ).
    expected_report_log = value #(
  ( `Type \INTERFACE=IF_AFF_INTF_V1\TYPE=TY_BLABLA was not found. Either interface or type doesnt exist.` )
  ).
    clear expected_log_messages.
    assert_logs_and_file_handler( ).
  endmethod.


  method schema_console_intf.
    cut->set_parameters(
      i_schema = abap_true
      i_objtyp = 'INTF'
      i_intf   = conv #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = value #( )
    ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_equals( act = cut->xslt_schema_content exp = value rswsourcet( ( |Test ST/Schema for INTF| ) ) ).

    expected_report_log = value #( ).
    assert_logs_and_file_handler( ).
  endmethod.

  method schema_console_func.
    cut->set_parameters(
      i_schema = abap_true
      i_objtyp = 'FUNC'
      i_intf   = 'IF_AFF_FUNC_V1'
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = value #( )
    ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_equals( act = cut->xslt_schema_content exp = value rswsourcet( ( |Test ST/Schema for FUNC| ) ) ).
    expected_report_log = value #( ).
    assert_logs_and_file_handler( ).

  endmethod.

  method schema_zip_func.
    cut->set_parameters(
      i_schema = abap_true
      i_objtyp = 'FUNC'
      i_intf   = 'IF_AFF_FUNC_V1'
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_disk   = abap_true
      i_multob = value #( )
    ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 1 ]-name exp = `func_schema.txt` ).
    cut->zip->get( exporting name = `func_schema.txt` importing content = data(act_content) ).
    data(text_handler) = new cl_aff_content_handler_text( ).
    data(expected_content) = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for FUNC` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    expected_report_log = value #(
  ( `Success: Zip file created here FULLPATH.zip` )
  ).
    assert_logs_and_file_handler( ).
  endmethod.

  method schema_zip_intf.
    cut->set_parameters(
      i_schema = abap_true
      i_objtyp = 'INTF'
      i_intf   = conv #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_disk   = abap_true
      i_multob = value #( )
    ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 1 ]-name exp = `intf_schema.txt` ).
    cut->zip->get( exporting name = `intf_schema.txt` importing content = data(act_content) ).
    data(text_handler) = new cl_aff_content_handler_text( ).
    data(expected_content) = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for INTF` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    expected_report_log = value #(
  ( `Success: Zip file created here FULLPATH.zip` )
  ).
    assert_logs_and_file_handler( ).
  endmethod.

  method xslt_zip_intf.
    cut->set_parameters(
      i_xslt   = abap_true
      i_objtyp = 'INTF'
      i_intf   = conv #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_disk   = abap_true
      i_multob = value #( )
    ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 1 ]-name exp = `intf_xslt.txt` ).
    cut->zip->get( exporting name = `intf_xslt.txt` importing content = data(act_content) ).
    data(text_handler) = new cl_aff_content_handler_text( ).
    data(expected_content) = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for INTF` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).
    expected_report_log = value #(
  ( `Success: Zip file created here FULLPATH.zip` )
  ).
    assert_logs_and_file_handler( ).
  endmethod.

  method repo_zip_intf.
    cut->set_parameters(
      i_repo   = abap_true
      i_objtyp = 'INTF'
      i_intf   = conv #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_disk   = abap_true
      i_multob = value #( )
    ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 4 ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 1 ]-name exp = `intf/examples/zaff_example_intf.intf.json` ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 2 ]-name exp = `intf/type/zif_aff_intf_v1.intf.json` ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 3 ]-name exp = `intf/intf-v1.json` ).

    data(text_handler) = new cl_aff_content_handler_text( ).
    data(expected_content) = text_handler->if_aff_content_handler~serialize( `File of zaff_example_intf` ).

    cut->zip->get(  exporting name  = `intf/examples/zaff_example_intf.intf.json` importing content  = data(act_content) ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    expected_content = text_handler->if_aff_content_handler~serialize( `File of zif_aff_intf_v1` ).
    cut->zip->get(  exporting name  = `intf/type/zif_aff_intf_v1.intf.json` importing content  = act_content ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    cut->zip->get(  exporting name  = `intf/intf-v1.json` importing content  = act_content ).
    expected_content = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for INTF` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).
    expected_report_log = value #(
  ( `Success: Zip file created here FULLPATH.zip` )
  ).
    assert_logs_and_file_handler( ).
  endmethod.


  method repo_zip_intf_w_namespace.
    cut->set_parameters(
      i_repo   = abap_true
      i_objtyp = 'INTF'
      i_intf   = conv #( c_intf_w_namespace )
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf_nspace )
      i_disk   = abap_true
      i_multob = value #( )
    ).
    cut->set_schema_test_content( value #( ( `TEST ABC` ) ) ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 4 ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 1 ]-name exp = `intf/examples/zaff_example_intf.intf.json` ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 2 ]-name exp = `intf/type/zif_aff_intf_v1.intf.json` ).
    cl_abap_unit_assert=>assert_equals( act = cut->zip->files[ 3 ]-name exp = `intf/intf-v1.json` ).

    data(text_handler) = new cl_aff_content_handler_text( ).
    data(expected_content) = text_handler->if_aff_content_handler~serialize( `File of zaff_example_intf` ).

    cut->zip->get(  exporting name  = `intf/examples/zaff_example_intf.intf.json` importing content  = data(act_content) ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    expected_content = text_handler->if_aff_content_handler~serialize( `File of zif_aff_intf_v1` ).
    cut->zip->get(  exporting name  = `intf/type/zif_aff_intf_v1.intf.json` importing content  = act_content ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    cut->zip->get(  exporting name  = `intf/intf-v1.json` importing content  = act_content ).
    expected_content = text_handler->if_aff_content_handler~serialize( `TEST ABC` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).
    expected_report_log = value #(
  ( `Type \INTERFACE=/NAMESP/IF_AFF_INTF_V1\TYPE=TY_MAIN was not found. Either interface or type doesnt exist.` )
  ( `Success: Zip file created here FULLPATH.zip` )
  ).
    clear expected_log_messages.
    assert_logs_and_file_handler( ).
  endmethod.


  method repo_zip_fugr.
    cut->set_parameters(
      i_repo   = abap_true
      i_objtyp = 'FUGR'
      i_intf   = 'IF_AFF_FUGR_V1'
      i_type   = 'TY_MAIN'
      i_examp  = 'Z_AFF_EXAMPLE_FUGR'
      i_disk   = abap_true
      i_multob = value #( )
    ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 6 ).

    data(text_handler) = new cl_aff_content_handler_text( ).
    data(expected_content) = text_handler->if_aff_content_handler~serialize( `File of Z_AFF_EXAMPLE_FUGR` ).

    cut->zip->get(  exporting name  = `fugr/examples/z_aff_example_fugr.fugr.json` importing content  = data(act_content) ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    expected_content = text_handler->if_aff_content_handler~serialize( `File of REPS, FUNC, FUGR` ).
    cut->zip->get(  exporting name  = `fugr/type/file_of_reps_func_fugr.json` importing content  = act_content ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    cut->zip->get(  exporting name  = `fugr/fugr-v1.json` importing content  = act_content ).
    expected_content = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for FUGR` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).
    cut->zip->get(  exporting name  = `fugr/func-v1.json` importing content  = act_content ).
    expected_content = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for FUNC` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).
    cut->zip->get(  exporting name  = `fugr/reps-v1.json` importing content  = act_content ).
    expected_content = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for REPS` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    expected_report_log = value #(
  ( `Success: Zip file created here FULLPATH.zip` )
  ).
    expected_log_messages = value #(
  ( type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
  ( type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
   ).
    assert_logs_and_file_handler( ).
  endmethod.


  method repo_zip_tabl.
    cut->set_parameters(
      i_repo   = abap_true
      i_objtyp = 'TABL'
      i_intf   = 'IF_AFF_TABL_V1'
      i_type   = 'TY_MAIN'
      i_examp  = 'Z_AFF_EXAMPLE_TABL'
      i_disk   = abap_true
      i_multob = value #( )
    ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 3 ).

    data(text_handler) = new cl_aff_content_handler_text( ).
    data(expected_content) = text_handler->if_aff_content_handler~serialize( `File of INDX, TABL` ).
    cut->zip->get(  exporting name  = `tabl/type/file_of_indx_tabl.json` importing content  = data(act_content) ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    cut->zip->get(  exporting name  = `tabl/indx-v1.json` importing content  = act_content ).
    expected_content = text_handler->if_aff_content_handler~serialize( `Test ST/Schema for INDX` ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).

    expected_report_log = value #(
  ( `Type \INTERFACE=IF_AFF_TABL_V1\TYPE=TY_MAIN was not found. Either interface or type doesnt exist.` )
  ( `The schema for interface IF_AFF_TABL_V1 could not be created.` )
  ( `Success: Zip file created here FULLPATH.zip` )
  ).
*    expected_log_messages = value #(
*  ( object = value #( devclass = 'IF_AFF_INDX_V1' ) type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
*   ).
    assert_logs_and_file_handler( ).
  endmethod.

  method whole_repo_zip.
    cut->set_parameters(
      i_whole  = abap_true
      i_objtyp = 'INTF'
      i_intf   = conv #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_disk   = abap_true
      i_multob = value #( )
    ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_number_between(
      lower  = 48 "DOMA only has a local interface in UIA
      upper  = 53
      number = lines( cut->zip->files )
    ).

    cut->zip->get( exporting name  = `fugr/type/file_of_reps_func_fugr.json` importing content  = data(act_content) ).
    cl_abap_unit_assert=>assert_equals( act = act_content exp = cl_abap_codepage=>convert_to( `File of REPS, FUNC, FUGR` ) ).

    "these are the expected files. In our unit test (for simplicity reasons) the aff of all object types consist only of 1 json file
    data file_name_tab type stringtab.
    file_name_tab = value #(
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
    ( `zif_aff_oo_types_v1.intf.json` )
     ).
*     if lines( cut->zip->files ) > 36.
*        insert lines of value stringtab(
*        ( `doma/examples/z_aff_example_doma.doma.json` )
*    ( `doma/type/zif_aff_doma_v1.intf.json` )
*    ( `doma/doma-v1.json` )
*        ) into table file_name_tab.
*     endif.

    assert_file_content(
      file_name_tab = file_name_tab
      zip           = cut->zip
    ).
*    expected_log_messages = value #(
*  ( object = value #( obj_name  = 'IF_AFF_CHKC_V1' ) type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
*  ( object = value #( obj_name  = 'IF_AFF_CHKO_V1' ) type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
*  ( object = value #( obj_name  = 'IF_AFF_CHKV_V1' ) type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
*  ( object = value #( obj_name  = 'IF_AFF_CLAS_V1' ) type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
*  ( object = value #( obj_name  = 'IF_AFF_DDLS_V1' ) type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
*  ( object = value #( obj_name  = 'IF_AFF_ENHO_V1' ) type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
*  ( object = value #( obj_name  = 'IF_AFF_ENHS_V1' ) type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
*  ( object = value #( obj_name  = 'IF_AFF_FUGR_V1' ) type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
*  ( object = value #( obj_name  = 'IF_AFF_FUNC_V1' ) type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
*  ( object = value #( obj_name  = 'IF_AFF_REPS_V1' ) type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
*  ( object = value #( obj_name  = 'IF_AFF_INTF_V1' ) type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
*  ( object = value #( obj_name  = 'IF_AFF_NROB_V1' ) type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
*  ( object = value #( obj_name  = 'IF_AFF_DDLX_V1' ) type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
*  ( object = value #( obj_name  = 'ZIF_AFF_DOMA_V1' ) type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
*  ).

    cl_abap_testdouble=>verify_expectations( file_handler_double ).

    cl_abap_unit_assert=>assert_number_between(
        lower            = 12 "DOMA only has a local interface in UIA
        upper            = 14
        number           = lines( cut->generator_log->get_messages( ) )
    ).
    if lines( cut->generator_log->get_messages( ) ) = 14.
      cl_abap_unit_assert=>assert_equals( act = cut->generator_log->get_messages( ) exp = expected_log_messages ).
    endif.
  endmethod.

  method multirepo_zip.
    cut->set_parameters(
      i_multre = abap_true
      i_objtyp = 'INTF'
      i_intf   = conv #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_disk   = abap_true
      i_multob = value #( ( `CHKC` ) ( `CHKO` ) ( `CHKV` ) )
    ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_equals( act = lines( cut->zip->files ) exp = 12 ).

    "these are the expected files. In our unit test (for simplicity reasons) the aff of all object types consist only of 1 json file
    data file_name_tab type stringtab.
    file_name_tab = value #(
    ( `chkc/examples/z_aff_example_chkc.chkc.json` )
    ( `chkc/type/zif_aff_chkc_v1.intf.json` )
    ( `chkc/chkc-v1.json` )
    ( `chko/examples/z_aff_example_chko.chko.json` )
    ( `chko/type/zif_aff_chko_v1.intf.json` )
    ( `chko/chko-v1.json` )
    ( `chkv/examples/z_aff_example_chkv.chkv.json` )
    ( `chkv/type/zif_aff_chkv_v1.intf.json` )
    ( `chkv/chkv-v1.json` )
     ).

    assert_file_content(
      file_name_tab = file_name_tab
      zip           = cut->zip
    ).
    expected_report_log = value #(
  ( `Success: Zip file created here FULLPATH.zip` )
  ).
    expected_log_messages = value #(
   ( type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
   ( type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
   ( type = 'I' text = `I::000 Generator Log` message = value #( msgty = 'I' msgv1 = 'Generator Log' ) )
    ).
    assert_logs_and_file_handler( ).
  endmethod.



  method generate_type_raises_error.
    "generator will raise an error when generate_type is called
    generator_double = new ltc_generator_double( log_to_return  = generator_log generate_type_will_raise_err = abap_true ).

    cut = new lcl_generator(
      aff_factory = aff_factory_double
      generator   = generator_double
      writer      = writer_double
    ).

    "schema on console
    cut->set_parameters(
      i_schema = abap_true
      i_objtyp = 'INTF'
      i_intf   = conv #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = value #( )
    ).

    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_initial( cut->zip ).

    expected_report_log = value #(
  ( `The generator couldn't generate the schema/XSLT for type \INTERFACE=IF_AFF_INTF_V1\TYPE=TY_MAIN` )
  ).
    clear expected_log_messages.
    assert_logs_and_file_handler( ).
  endmethod.

  method writer_validate_returns_false.
    "writer returns false when validate is called
    writer_double ?= cl_abap_testdouble=>create( 'zif_aff_writer' ).
    cl_abap_testdouble=>configure_call( writer_double )->returning( abap_false )->ignore_all_parameters( ).
    writer_double->validate( source = value #( ) log = new zcl_aff_log( ) ).

    cl_abap_testdouble=>configure_call( writer_double )->returning( writer_log ).
    writer_double->get_log( ).

    cut = new lcl_generator(
      aff_factory = aff_factory_double
      generator   = generator_double
      writer      = writer_double
    ).

    "schema on console
    cut->set_parameters(
      i_schema = abap_true
      i_objtyp = 'INTF'
      i_intf   = conv #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = value #( )
    ).

    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_not_initial( cut->xslt_schema_content ).
    expected_report_log = value #(
  ( `ATTENTION: The created schema/xslt for type \INTERFACE=IF_AFF_INTF_V1\TYPE=TY_MAIN is not valid.` )
  ).
    assert_logs_and_file_handler( ).
  endmethod.

  method error_not_all_paramtrs_supplid.
    expected_log_messages = value #( ).
    expected_report_log = value #(
  ( `Please fill out all fields (interfacename, objecttype, abaptypename)` )
  ).
    "schema on console
    create_new_cut_with_new_params(
      i_schema = abap_true
*     i_objtyp = 'INTF'          object type not supplied
      i_intf   = conv #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = value #( )
    ).

    create_new_cut_with_new_params(
      i_schema = abap_true
      i_objtyp = 'INTF'
*     i_intf   = conv #( c_intf )    interfacename not supplied
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = value #( )
    ).

    create_new_cut_with_new_params(
      i_schema = abap_true
      i_objtyp = 'INTF'
      i_intf   = conv #( c_intf )
*     i_type   = 'TY_MAIN'               typename not supplied
      i_examp  = conv #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = value #( )
    ).

    expected_report_log = value #(
  ( `Please fill out all fields (objecttype, interfacename, examplename)` )
  ).

    " repo folder for one object
    create_new_cut_with_new_params(
      i_repo   = abap_true
*     i_objtyp = 'INTF'          object type not supplied
      i_intf   = conv #( c_intf )
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = value #( )
    ).

    create_new_cut_with_new_params(
      i_repo   = abap_true
      i_objtyp = 'INTF'
*     i_intf   = conv #( c_intf )    interfacename not supplied
      i_type   = 'TY_MAIN'
      i_examp  = conv #( c_aff_example_intf )
      i_consol = abap_true
      i_multob = value #( )
    ).

    create_new_cut_with_new_params(
      i_repo   = abap_true
      i_objtyp = 'INTF'
      i_intf   = conv #( c_intf )
      i_type   = 'TY_MAIN'
*     i_examp  = conv #( c_aff_example_intf )   examplename not supplied
      i_consol = abap_true
      i_multob = value #( )
    ).

  endmethod.

  method create_new_cut_with_new_params.
    cut = new lcl_generator(
      aff_factory = aff_factory_double
      generator   = generator_double
      writer      = writer_double
    ).

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
      i_multob = i_multob
    ).
    "When
    cut->start_of_selection( ).
    "Then
    cl_abap_unit_assert=>assert_initial( cut->xslt_schema_content ).
    cl_abap_unit_assert=>assert_initial( cut->zip ).

    assert_logs_and_file_handler( ).
  endmethod.

  method assert_file_content.

    data(text_handler) = new cl_aff_content_handler_text( ).

    loop at file_name_tab assigning field-symbol(<filename>).
      zip->get( exporting name  = <filename> importing content  = data(act_content) ).
      if <filename> cp '*-v1.json'.
        split <filename> at '/' into table data(splitted).
        data(objecttype) = splitted[ 2 ].
        replace '-v1.json' in objecttype with ''.
        data(expected_content) = text_handler->if_aff_content_handler~serialize( |Test ST/Schema for { to_upper( objecttype ) }| ).
        cl_abap_unit_assert=>assert_equals( act = act_content exp = expected_content ).
      else.
        split <filename> at '/' into table splitted.
        data(objectname_part) = splitted[ lines( splitted ) ].
        split objectname_part at '.' into table data(splitted2).
        data(objectname) = splitted2[ 1 ].
        data(expected_content_str) = |file of { to_lower( objectname ) }|.
        data(actual) = to_lower( cl_abap_codepage=>convert_from( act_content ) ).

        cl_abap_unit_assert=>assert_equals( act = actual exp = expected_content_str ).
      endif.
    endloop.
  endmethod.

endclass.


initialization.
*  selection-screen begin of block comment with frame.
*    selection-screen comment /05(79) text1.
*    selection-screen comment /05(79) text2.
*    selection-screen skip 1.
*  selection-screen end of block comment.

*  Define your type as described here:
*  https://wiki.wdf.sap.corp/wiki/display/ADTVEGA/Bring+your+own+object#Bringyourownobject-CreateABAPobjecttypeforyourobject
*  text1 = 'Enter an interfacename in field "object name" and a typename in field "type"' ##NO_TEXT.

  helper = new lcl_generator( ).
  helper->modify_screen( ).

  loop at helper->get_table_with_all_githubtypes( ) assigning field-symbol(<type>) ##NEEDED.
    p_multob-option = 'EQ'.
    p_multob-sign = 'I'.
    p_multob-low = <type>.
    append p_multob.
  endloop.

* when enter or F8 is pressed in the  screen
at selection-screen.
  helper->at_selection_screen( ).

at selection-screen on value-request for p_objtyp.
  helper->on_value_request_for_objtype( ).

at selection-screen on value-request for p_intf.
  helper->on_value_request_for_intfname( ).

at selection-screen on value-request for p_examp.
  helper->on_value_request_for_example( ).

at selection-screen on value-request for p_type.
  helper->on_value_request_for_type( ).

at selection-screen output.
* The OUTPUT event is also trigged to re-draw ABAP report screen allowing it to
* be used to hide, display or deactivate fields. Please note at this point sy-ucomm field
* has been refreshed so you need to use value captured above in gd_ucomm
*  case gd_ucomm.
*    when 'UPD'. "radiobutton selection made
*  endcase.

  helper->modify_screen( ).

start-of-selection.

  helper->start_of_selection( ).
  helper->print_logs( ).

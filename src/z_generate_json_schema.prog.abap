*&---------------------------------------------------------------------*
*& Report z_generate_json_schema
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_generate_json_schema.
CLASS lcl_generator_helper DEFINITION
 FINAL
 CREATE PUBLIC.
  PUBLIC SECTION.

    CLASS-METHODS: generate
      IMPORTING generate_schema TYPE abap_bool
                interface_name  TYPE tadir-obj_name
                type_name       TYPE tadir-obj_name
      RETURNING VALUE(result)   TYPE string
      RAISING   zcx_aff_tools.
  PRIVATE SECTION.
    CLASS-METHODS get_format_version
      IMPORTING interface_name TYPE tadir-obj_name
      RETURNING VALUE(result)  TYPE i.
    CLASS-METHODS get_object_type_path
      IMPORTING interface_name TYPE tadir-obj_name
      RETURNING VALUE(path)    TYPE string.

ENDCLASS.

CLASS lcl_generator_helper IMPLEMENTATION.

  METHOD generate.
    DATA(absolute_name) = |\\INTERFACE={ interface_name }\\TYPE={ type_name }|.

    cl_abap_typedescr=>describe_by_name( EXPORTING p_name = absolute_name RECEIVING p_descr_ref = DATA(type_description) EXCEPTIONS type_not_found = 1 ).
    IF sy-subrc = 1.
      RAISE EXCEPTION NEW zcx_aff_tools( ).
    ENDIF.
    DATA element_description TYPE REF TO cl_abap_structdescr.
    element_description ?= type_description.
    DATA field TYPE REF TO data.
    FIELD-SYMBOLS <field> TYPE any.
    CREATE DATA field TYPE HANDLE element_description.
    ASSIGN field->* TO <field>.
    DATA my_type TYPE REF TO data.
    GET REFERENCE OF <field> INTO my_type.

    ASSIGN my_type->* TO <field>.

    DATA(format_version) = get_format_version( interface_name ).
    DATA(object_type_path) = get_object_type_path( interface_name ).
    DATA(schema_id) = |https://github.com/SAP/abap-file-formats/blob/main/file-formats/{ object_type_path }-v{ format_version }.json| ##NO_TEXT.


    DATA writer TYPE REF TO zcl_aff_writer.
    " set up the writer
    IF generate_schema = abap_true.
      writer = NEW zcl_aff_writer_json_schema( schema_id = schema_id format_version = format_version ).
    ELSE.
      writer = NEW zcl_aff_writer_xslt( ).
    ENDIF.

    DATA(generator) = NEW zcl_aff_generator( writer ).
    DATA(result_table) = generator->generate_type( <field> ).
    CONCATENATE LINES OF result_table INTO result SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.

  METHOD get_format_version.
    DATA format_version TYPE string.
    SPLIT interface_name  AT '_' INTO TABLE DATA(splitted_intfname).
    DATA(last) = splitted_intfname[ lines( splitted_intfname ) ].
    REPLACE ALL OCCURRENCES OF 'V' IN last WITH ''.
    TRY.
        DATA(regx) = '[[:alpha:]]+'.
*      check if the token only contains digits
        DATA(contains_chars) = xsdbool( count( val = last regex = regx ) > 0 ) ##REGEX_POSIX.
        DATA(default_format_version) = 1.
        IF contains_chars = abap_false.
          format_version  = last.
        ELSE.
          format_version = default_format_version.
        ENDIF.
      CATCH cx_sy_conversion_no_number.

        format_version = default_format_version.
    ENDTRY.

    result = CONV #( format_version ).
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

ENDCLASS.

START-OF-SELECTION.

  PARAMETERS:
    p_schema TYPE c RADIOBUTTON GROUP sel USER-COMMAND upd DEFAULT 'X',
    p_xslt   TYPE c RADIOBUTTON GROUP sel ##NEEDED,
    p_intf   TYPE tadir-obj_name,
    p_type   TYPE tadir-obj_name DEFAULT 'ty_main'.


  TRY.
      DATA(xslt_content) = lcl_generator_helper=>generate( generate_schema = p_schema interface_name = to_upper( p_intf ) type_name = to_upper( p_type ) ).
      cl_demo_output=>display( xslt_content ).
    CATCH zcx_aff_tools INTO DATA(exception).
      cl_demo_output=>display( exception->get_text( ) ).
  ENDTRY.

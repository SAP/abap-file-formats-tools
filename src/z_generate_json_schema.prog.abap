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
      IMPORTING
                generate_schema TYPE abap_bool
                object_type     TYPE trobjtype
                interface_name  TYPE sobj_name
                type_name       TYPE sobj_name
      RETURNING VALUE(result)   TYPE rswsourcet
      RAISING
                cx_aff_root.
  PRIVATE SECTION.
    CLASS-METHODS get_format_version
      IMPORTING
        interface_name        TYPE sobj_name
      RETURNING
        VALUE(format_version) TYPE string.

ENDCLASS.

CLASS lcl_generator_helper IMPLEMENTATION.

  METHOD generate.
    DATA(absolute_name) = |\\INTERFACE={ interface_name }\\TYPE={ type_name }|.
    DATA r_typedescr TYPE REF TO cl_abap_typedescr.
    DATA r_elemdescr TYPE REF TO cl_abap_structdescr.
    DATA type TYPE if_aff_intf_v1=>ty_main.
    DATA(description) = cl_abap_typedescr=>describe_by_data( type ).
    DATA(type_description) = cl_abap_typedescr=>describe_by_name( absolute_name ).

    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name = absolute_name RECEIVING p_descr_ref = r_typedescr EXCEPTIONS type_not_found = 1 ).
    if sy-subrc = 1.
      raise exception type cx_aff_root.
    endif.
    r_elemdescr ?= r_typedescr.
    DATA r_field TYPE REF TO data.
    FIELD-SYMBOLS <field> TYPE any.
    CREATE DATA r_field TYPE HANDLE r_elemdescr.
    ASSIGN r_field->* TO <field>.
    DATA my_type TYPE REF TO data.
    GET REFERENCE OF <field> INTO my_type.

    ASSIGN my_type->* TO <field>.

    DATA(format_version) = get_format_version( interface_name ).
    DATA(mainobjtype) = object_type.
    IF object_type = 'REPS' OR object_type = 'FUNC'.
      mainobjtype = 'FUGR'.
    ENDIF.
    DATA(schemid) = |https://github.com/SAP/abap-file-formats/blob/main/file-formats/{ to_lower( mainobjtype ) }/{ to_lower( object_type ) }-v{ format_version }.json| ##NO_TEXT.


    DATA writer TYPE REF TO zcl_aff_writer.
    " set up the writer
    IF generate_schema = abap_true.
      writer = NEW zcl_aff_writer_json_schema( schema_id = schemid format_version = CONV #( format_version ) ).
    ELSE.
      writer = NEW zcl_aff_writer_xslt( ).
    ENDIF.

    DATA(generator) = NEW zcl_aff_generator( writer ).
    result = generator->generate_type(  <field> ).

  ENDMETHOD.

  METHOD get_format_version.
    SPLIT interface_name  AT '_' INTO TABLE DATA(splitted_intfname).
    DATA(last) = splitted_intfname[ lines( splitted_intfname ) ].
    REPLACE ALL OCCURRENCES OF 'v' IN last WITH ''.
    REPLACE ALL OCCURRENCES OF 'V' IN last WITH ''.
    TRY.
        DATA(regx) = '[[:alpha:]]+'.
*      check if the token only contains digits
        DATA(contains_chars) = xsdbool( count( val = last regex = regx ) > 0 ) ##REGEX_POSIX.
        data(default_format_version) = 1.
        IF contains_chars = abap_false.
          format_version  = last.
        ELSE.
          format_version = default_format_version.
        ENDIF.
      CATCH cx_sy_conversion_no_number.

        format_version = default_format_version.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  PARAMETERS:
    p_schema TYPE c RADIOBUTTON GROUP sel USER-COMMAND upd DEFAULT 'X',
    p_xslt   TYPE c RADIOBUTTON GROUP sel,
    p_objtyp TYPE trobjtype,
    p_intf   TYPE sobj_name,
    p_type   TYPE sobj_name.


  p_objtyp  = to_upper( p_objtyp ).
  p_intf   = to_upper( p_intf ).
  p_type   = to_upper( p_type ).


  TRY.
      DATA(xslt_content) = lcl_generator_helper=>generate( generate_schema = p_schema interface_name = p_intf object_type = p_objtyp type_name = p_type ).
      cl_demo_output=>write( xslt_content ).
    CATCH cx_aff_root INTO DATA(exception).
      cl_demo_output=>write( exception->get_text( ) ).
  ENDTRY.
  cl_demo_output=>display( ).

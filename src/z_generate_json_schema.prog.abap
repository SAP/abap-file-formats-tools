*&---------------------------------------------------------------------*
*& Report z_generate_json_schema
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_generate_json_schema.

START-OF-SELECTION.


  " use your defined type
  DATA my_type TYPE zif_aff_sajc=>ty_main.

  " set up the writer
  DATA(writer) = NEW zcl_aff_writer_json_schema( '' ).

  DATA(generator) = NEW zcl_aff_generator( writer ).
  TRY.
      DATA(xslt_content) = generator->generate_type( my_type ).
      cl_demo_output=>write( xslt_content ).
    CATCH cx_aff_root INTO DATA(exception).
      cl_demo_output=>write( exception->get_text( ) ).
  ENDTRY.
  cl_demo_output=>display( ).

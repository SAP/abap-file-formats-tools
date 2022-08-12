CLASS cl_run DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run
      RETURNING
        VALUE(result) TYPE string_table.
ENDCLASS.

CLASS cl_run IMPLEMENTATION.
  METHOD run.
    DATA writer    TYPE REF TO zcl_aff_writer_json_schema.
    DATA log       TYPE REF TO zif_aff_log.
    DATA generator TYPE REF TO zcl_aff_generator.
    DATA intf      TYPE zif_aff_intf_v1=>ty_main.

    CREATE OBJECT writer
      EXPORTING
        schema_id = 'sdf'.
    CREATE OBJECT generator
      EXPORTING
        writer = writer.
    result = generator->generate_type( intf ).
    log = generator->get_log( ).
  ENDMETHOD.
ENDCLASS.

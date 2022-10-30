CLASS cl_run DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_row,
             filename TYPE string,
             contents TYPE string,
           END OF ty_row.
    TYPES ty_tab TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
    CLASS-METHODS run
      RETURNING
        VALUE(tab) TYPE ty_tab.
  PRIVATE SECTION.
    CLASS-METHODS run_intf
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.

CLASS cl_run IMPLEMENTATION.
  METHOD run_intf.
    DATA writer     TYPE REF TO zcl_aff_writer_json_schema.
    DATA generator  TYPE REF TO zcl_aff_generator.
    DATA intf       TYPE zif_aff_intf_v1=>ty_main.
    DATA string_tab TYPE string_table.

* TODO
    DATA ref TYPE REF TO data.
    CREATE DATA ref TYPE zif_aff_intf_v1=>ty_main.

    CREATE OBJECT writer
      EXPORTING
        schema_id = 'https://github.com/SAP/abap-file-formats/blob/main/file-formats/intf/intf-v1.json'.
    CREATE OBJECT generator
      EXPORTING
        writer = writer.
    string_tab = generator->generate_type( intf ).
    CONCATENATE LINES OF string_tab INTO result SEPARATED BY |\n|.
  ENDMETHOD.

  METHOD run.
    DATA str TYPE string.
    DATA row LIKE LINE OF tab.

    str = run_intf( ).

    row-filename = 'intf.json'.
    row-contents = str.
    APPEND row TO tab.
  ENDMETHOD.
ENDCLASS.

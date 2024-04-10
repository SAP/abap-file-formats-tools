CLASS lcl_aff_generator DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! Generates the AFF JSON schema.
      "!
      "! @parameter schema_id | The id that should be written in the $id field of the schema
      "! @parameter data | The type where the schema should be generated from
      "! @parameter format_version | The version of the ABAP file format as integer
      "! @parameter result | The generated schema as string
      generate_schema
        IMPORTING schema_id      TYPE string
                  data           TYPE data
                  format_version TYPE i
        RETURNING VALUE(result)  TYPE string
        RAISING   zcx_aff_tools,

      "! Generates the AFF XSLT transformation.
      "!
      "! @parameter data | The type where the schema should be generated from
      "! @parameter result | The generated XSLT as string
      generate_xslt
        IMPORTING data          TYPE data
        RETURNING VALUE(result) TYPE string
        RAISING   zcx_aff_tools.

ENDCLASS.

CLASS lcl_aff_generator IMPLEMENTATION.

  METHOD generate_schema.
    DATA(schema_writer) = NEW zcl_aff_writer_json_schema( schema_id      = schema_id
                                                          format_version = format_version ).
    DATA(generator) = NEW zcl_aff_generator( schema_writer ).
    DATA(result_table) = generator->zif_aff_generator~generate_type( data ).
    CONCATENATE LINES OF result_table INTO result SEPARATED BY cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD generate_xslt.
    DATA(xslt_writer) = NEW zcl_aff_writer_xslt( ).
    DATA(generator) = NEW zcl_aff_generator( xslt_writer ).
    DATA(result_table) = generator->zif_aff_generator~generate_type( data ).
    CONCATENATE LINES OF result_table INTO result SEPARATED BY cl_abap_char_utilities=>newline.
  ENDMETHOD.

ENDCLASS.

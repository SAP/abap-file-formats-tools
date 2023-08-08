INTERFACE zif_aff_generator
  PUBLIC.
  METHODS: generate_type
    IMPORTING
      data          TYPE data
    RETURNING
      VALUE(result) TYPE string_table
    RAISING
      zcx_aff_tools,

    get_log
      RETURNING
        VALUE(log) TYPE REF TO zif_aff_log.

ENDINTERFACE.

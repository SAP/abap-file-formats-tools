INTERFACE zif_aff_writer
  PUBLIC.

  CONSTANTS: BEGIN OF  type_info,
               string    TYPE string VALUE 'string',
               numeric   TYPE string VALUE 'numeric',
               boolean   TYPE string VALUE 'boolean',
               date_time TYPE string VALUE 'date_time',
             END OF type_info.

  CONSTANTS: BEGIN OF operation,
               initial         TYPE string VALUE 'initial',
               write_element   TYPE string VALUE 'write_element',
               open_structure  TYPE string VALUE 'open_structure',
               close_structure TYPE string VALUE 'close_structure',
               open_table      TYPE string VALUE 'open_table',
               close_table     TYPE string VALUE 'close_table',
             END OF operation.


  METHODS:

    write_element
      IMPORTING
        element_name        TYPE string
        element_description TYPE REF TO cl_abap_elemdescr
      RAISING
        zcx_aff_tools,

    "! open node. for example table or object
    "! @parameter node_description | description for node
    open_node
      IMPORTING
        node_description TYPE REF TO cl_abap_typedescr
        node_name        TYPE string
      RAISING
        zcx_aff_tools,

    "! close node. for example table or object
    "! @parameter node_description | description for node
    close_node
      IMPORTING
        node_description TYPE REF TO cl_abap_typedescr
        node_name        TYPE string
      RAISING
        zcx_aff_tools,

    get_output
      RETURNING VALUE(result) TYPE string_table,

    get_log
      RETURNING VALUE(log) TYPE REF TO zif_aff_log,

    "! Validate the given source and writes messages into the log
    "!
    "! @parameter source | Source to validate
    "! @parameter log | log to write messages
    "! @parameter result | true, if the source is valid, false if not
    validate
      IMPORTING source        TYPE string_table
                log           TYPE REF TO zif_aff_log
      RETURNING VALUE(result) TYPE abap_bool,

    open_include
      IMPORTING
        include_description TYPE REF TO cl_abap_structdescr,

    close_include.

ENDINTERFACE.

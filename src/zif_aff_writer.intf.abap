INTERFACE zif_aff_writer
  PUBLIC.

  TYPES:
    BEGIN OF ENUM enum_formatting_option STRUCTURE formatting_option,
      no_formatting VALUE IS INITIAL,
      camel_case    VALUE 1,
    END OF ENUM enum_formatting_option STRUCTURE formatting_option,

    BEGIN OF ENUM enum_type_info STRUCTURE type_info,
      string,
      numeric,
      boolean,
      date_time,
    END OF ENUM enum_type_info STRUCTURE type_info,

    BEGIN OF ENUM enum_operation STRUCTURE operation,
      initial,
      write_element,
      open_structure,
      close_structure,
      open_table,
      close_table,
    END OF ENUM enum_operation STRUCTURE operation.

  TYPES:
    BEGIN OF ty_name_mapping,
      abap TYPE abap_compname,
      json TYPE string,
    END OF ty_name_mapping,
    ty_name_mappings TYPE HASHED TABLE OF ty_name_mapping WITH UNIQUE KEY abap.

  TYPES:
    BEGIN OF ty_value_mapping,
      abap TYPE string,
      json TYPE string,
    END OF ty_value_mapping,
    ty_value_mappings TYPE HASHED TABLE OF ty_value_mapping WITH UNIQUE KEY abap.

  TYPES:
    BEGIN OF ty_abap_value_mapping,
      abap_element   TYPE abap_compname,
      target_type    TYPE enum_type_info,
      value_mappings TYPE ty_value_mappings,
    END OF ty_abap_value_mapping,
    ty_abap_value_mappings TYPE HASHED TABLE OF ty_abap_value_mapping WITH UNIQUE KEY abap_element.

  METHODS:
    set_name_mappings
      IMPORTING
        name_mappings TYPE ty_name_mappings,

    set_abap_value_mappings
      IMPORTING
        abap_value_mappings TYPE ty_abap_value_mappings,

    set_formatting_option
      IMPORTING
        formatting_option TYPE zif_aff_writer=>enum_formatting_option,

    write_element
      IMPORTING
        element_name        TYPE string
        element_description TYPE REF TO cl_abap_elemdescr
      RAISING
        cx_aff_root,

    "! open node. for example table or object
    "! @parameter node_description | description for node
    open_node
      IMPORTING
        node_description TYPE REF TO cl_abap_typedescr
        node_name        TYPE string
      RAISING
        cx_aff_root,

    "! close node. for example table or object
    "! @parameter node_description | description for node
    close_node
      IMPORTING
        node_description TYPE REF TO cl_abap_typedescr
        node_name        TYPE string
      RAISING
        cx_aff_root,

    get_output
      RETURNING VALUE(result) TYPE rswsourcet,

    get_log
      RETURNING VALUE(log) TYPE REF TO if_aff_log,

    "! Validate the given source and writes messages into the log
    "!
    "! @parameter source | Source to validate
    "! @parameter log | log to write messages
    "! @parameter result | true, if the source is valid, false if not
    validate
      IMPORTING source        TYPE rswsourcet
                log           TYPE REF TO if_aff_log
      RETURNING VALUE(result) TYPE abap_bool,

    open_include
      IMPORTING
        include_description TYPE REF TO cl_abap_structdescr,

    close_include.

ENDINTERFACE.

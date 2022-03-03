interface zif_aff_writer
  public .

  types:
    begin of enum enum_formatting_option structure formatting_option,
      no_formatting value is initial,
      camel_case    value 1,
    end of enum enum_formatting_option structure formatting_option,

    begin of enum enum_type_info structure type_info,
      string,
      numeric,
      boolean,
      date_time,
    end of enum enum_type_info structure type_info,

    begin of enum enum_operation structure operation,
      initial,
      write_element,
      open_structure,
      close_structure,
      open_table,
      close_table,
    end of enum enum_operation structure operation.

  types:
    begin of ty_name_mapping,
      abap type abap_compname,
      json type string,
    end of ty_name_mapping,
    ty_name_mappings type hashed table of ty_name_mapping with unique key abap.

  types:
    begin of ty_value_mapping,
      abap type string,
      json type string,
    end of ty_value_mapping,
    ty_value_mappings type hashed table of ty_value_mapping with unique key abap.

  types:
    begin of ty_abap_value_mapping,
      abap_element   type abap_compname,
      target_type    type enum_type_info,
      value_mappings type ty_value_mappings,
    end of ty_abap_value_mapping,
    ty_abap_value_mappings type hashed table of ty_abap_value_mapping with unique key abap_element.

  methods:
    set_name_mappings
      importing
        name_mappings type ty_name_mappings,

    set_abap_value_mappings
      importing
        abap_value_mappings type ty_abap_value_mappings,

    set_formatting_option
      importing
        formatting_option type zif_aff_writer=>enum_formatting_option,

    write_element
      importing
        element_name        type string
        element_description type ref to cl_abap_elemdescr
      raising
        cx_aff_root,

    "! open node. for example table or object
    "! @parameter node_description | description for node
    open_node
      importing
        node_description type ref to cl_abap_typedescr
        node_name        type string
      raising
        cx_aff_root,

    "! close node. for example table or object
    "! @parameter node_description | description for node
    close_node
      importing
        node_description type ref to cl_abap_typedescr
        node_name        type string
      raising
        cx_aff_root,

    get_output
      returning value(result) type rswsourcet,

    get_log
      returning value(log) type ref to if_aff_log,

    "! Validate the given source and writes messages into the log
    "!
    "! @parameter source | Source to validate
    "! @parameter log | log to write messages
    "! @parameter result | true, if the source is valid, false if not
    validate
      importing source        type rswsourcet
                log           type ref to if_aff_log
      returning value(result) type abap_bool,

    open_include
      importing
        include_description type ref to cl_abap_structdescr,

    close_include.

endinterface.

class lcl_aff_conditions definition final.
  public section.
    types:
      begin of enum_member,
        link type string,
        json type string,
      end of enum_member,
      enum_members type standard table of enum_member with empty key,
      begin of node,
        parent type i,
        name type string,
        type_key type string,
        items type abap_bool,
        kind type abap_typekind,
        json_type type string,
        doc type zcl_aff_abap_doc_parser=>abap_doc,
        default_json type string,
        members type enum_members,
      end of node,
      nodes type standard table of node with empty key.
    methods constructor importing occurrences type nodes.
    "! Returns the "allOf" member for the root schema, or nothing if there are no conditions
    methods compile returning value(result) type string raising zcx_aff_tools.
  private section.
    data occurrences type nodes.
    data context type i.
    data component type string.
    methods compile_rule importing target type node condition type string
      returning value(result) type string raising zcx_aff_tools.
    methods resolve importing link type string returning value(result) type i raising zcx_aff_tools.
    methods descend importing start type i names type string_table returning value(result) type i.
    methods literal importing field type i token type string returning value(result) type string raising zcx_aff_tools.
    methods normalize_link importing link type string returning value(result) type string raising zcx_aff_tools.
    methods common_scope importing first type i second type i returning value(result) type i.
    methods wrap importing target type i scope type i json type string guard type abap_bool default abap_false
      returning value(result) type string raising zcx_aff_tools.
    methods quote importing text type string returning value(result) type string.
    methods fail importing text type string raising zcx_aff_tools.
endclass.


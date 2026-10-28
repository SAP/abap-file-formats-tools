CLASS lcl_aff_conditions DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF enum_member,
        link TYPE string,
        json TYPE string,
        doc TYPE zcl_aff_abap_doc_parser=>abap_doc,
      END OF enum_member,
      enum_members TYPE STANDARD TABLE OF enum_member WITH EMPTY KEY,
      BEGIN OF node,
        parent TYPE i,
        name TYPE string,
        type_key TYPE string,
        items TYPE abap_bool,
        kind TYPE abap_typekind,
        json_type TYPE string,
        doc TYPE zcl_aff_abap_doc_parser=>abap_doc,
        default_json TYPE string,
        members TYPE enum_members,
      END OF node,
      nodes TYPE STANDARD TABLE OF node WITH EMPTY KEY.
    METHODS constructor IMPORTING occurrences TYPE nodes.
    "! Returns the "allOf" member for the root schema, or nothing if there are no conditions
    METHODS compile RETURNING VALUE(result) TYPE string RAISING zcx_aff_tools.
  PRIVATE SECTION.
    DATA occurrences TYPE nodes.
    DATA context TYPE i.
    DATA component TYPE string.
    METHODS compile_rule
      IMPORTING target        TYPE node
                condition     TYPE string
                target_index  TYPE i OPTIONAL
                member        TYPE enum_member OPTIONAL
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_aff_tools.
    METHODS restrict_enum
      IMPORTING target        TYPE node
                member        TYPE enum_member
      RETURNING VALUE(result) TYPE string.
    METHODS resolve IMPORTING link TYPE string RETURNING VALUE(result) TYPE i RAISING zcx_aff_tools.
    METHODS descend
      IMPORTING start         TYPE i
                names         TYPE string_table
      RETURNING VALUE(result) TYPE i.
    METHODS literal
      IMPORTING field         TYPE i
                token         TYPE string
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_aff_tools.
    METHODS normalize_link
      IMPORTING link          TYPE string
                constant_link TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_aff_tools.
    METHODS common_scope
      IMPORTING first         TYPE i
                second        TYPE i
      RETURNING VALUE(result) TYPE i.
    METHODS wrap
      IMPORTING target        TYPE i
                scope         TYPE i
                json          TYPE string
                guard         TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_aff_tools.
    METHODS quote IMPORTING text TYPE string RETURNING VALUE(result) TYPE string.
    METHODS fail IMPORTING text TYPE string RAISING zcx_aff_tools.
ENDCLASS.


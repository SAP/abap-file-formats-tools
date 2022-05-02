CLASS zcl_aff_abap_doc_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS:
      create_instance
        IMPORTING
          source        TYPE string_table
        RETURNING
          VALUE(result) TYPE REF TO zcl_aff_abap_doc_reader.
    METHODS get_abap_doc_for_element
      IMPORTING
        version       TYPE r3state DEFAULT 'A'
        element_name  TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        cx_static_check.
  PROTECTED SECTION.
    DATA
      source TYPE string_table.
ENDCLASS.



CLASS zcl_aff_abap_doc_reader IMPLEMENTATION.


  METHOD create_instance.
    result = NEW #( ).
  ENDMETHOD.


  METHOD get_abap_doc_for_element.


    DATA: l_element_name      TYPE string,
          l_scanned_elem_name TYPE string.
    DATA section_source       TYPE string_table.
    DATA scan_abap_doc_blocks TYPE STANDARD TABLE OF lcl_section_source_comments=>ty_comment_block.
    DATA element_was_found    TYPE abap_bool.

    CLEAR: result, element_was_found.

    l_element_name  = element_name.

    TRANSLATE l_element_name  TO UPPER CASE.
    CONDENSE l_element_name.


    DATA(scan_util) = NEW lcl_section_source_comments( ).

    section_source[] = me->source[].

    scan_util->scan_code( EXPORTING source_to_be_scanned = section_source
                          IMPORTING tab_statements       = DATA(scan_statements)
                                    tab_tokens           = DATA(scan_tokens) ).

    scan_abap_doc_blocks = scan_util->identify_abap_doc_blocks_all(
      tab_statements = scan_statements
      tab_tokens     = scan_tokens
      tab_source     = section_source ).

    LOOP AT scan_abap_doc_blocks ASSIGNING FIELD-SYMBOL(<fs_abap_doc_block>).

      IF <fs_abap_doc_block>-hook_relevant_tok_name-str = 'BEGIN'.
        l_scanned_elem_name = <fs_abap_doc_block>-hook_relevant_tok_name_add-str.
      ELSE.
        l_scanned_elem_name = <fs_abap_doc_block>-hook_relevant_tok_name-str.
      ENDIF.

      IF l_scanned_elem_name = l_element_name.

        " prepare the result for required element
        LOOP AT <fs_abap_doc_block>-tab_comments ASSIGNING FIELD-SYMBOL(<adoc_line>).
          CONDENSE <adoc_line>.         " remove leading spaces
          <adoc_line> = <adoc_line>+2.  " remove "!
          CONDENSE <adoc_line>.         " remove again leading spaces
          IF sy-tabix = 1.
            result = <adoc_line>.
          ELSE.
            CONCATENATE result <adoc_line> INTO result SEPARATED BY space.
          ENDIF.
        ENDLOOP.
        element_was_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF element_was_found = abap_false.
      RAISE EXCEPTION NEW zcx_aff_tools( message = l_element_name ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

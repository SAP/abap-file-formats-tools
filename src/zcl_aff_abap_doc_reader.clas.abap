class zcl_aff_abap_doc_reader definition
  public
  final
  create private .

  public section.
    types:
      ty_source    type standard table of string with empty key.

    class-methods:
      create_instance
        importing
          source        type ty_source
        returning
          value(result) type ref to zcl_aff_abap_doc_reader.
    methods get_abap_doc_for_element
      importing
        version       type r3state default 'A'
        element_name  type string
      returning
        value(result) type string
      raising
        cx_oo_clif_not_exists
        cx_oo_access_permission
        cx_oo_clif_scan_error
        cx_oo_abap_doc_reader.
  protected section.
    data
      source type ty_source.
endclass.



class zcl_aff_abap_doc_reader implementation.


  method create_instance.
    result = new #( ).
  endmethod.


  method get_abap_doc_for_element.

    data: scan_util           type ref to lcl_SECTION_SOURCE_COMMENTS,
          l_element_name      type string,
          l_scanned_elem_name type string.
    data section_source       type seo_section_source.
    data scan_abap_doc_blocks type standard table of lcl_SECTION_SOURCE_COMMENTS=>ty_comment_block.
    data element_was_found    type abap_bool.

    clear: result, element_was_found.

    l_element_name  = element_name.

    translate l_element_name  to upper case.
    condense l_element_name.


    create object scan_util.

    section_source[] = me->source[].

    scan_util->scan_code( exporting source_to_be_scanned = section_source
                          importing tab_statements       = data(scan_statements)
                                    tab_tokens           = data(scan_tokens) ).

    scan_util->identify_abap_doc_blocks_all(
      exporting
        tab_statements = scan_statements
        tab_tokens     = scan_tokens
        tab_source     = section_source
      importing
        tab_abap_doc   = scan_abap_doc_blocks ).

    loop at scan_abap_doc_blocks assigning field-symbol(<fs_abap_doc_block>).

      if <fs_abap_doc_block>-hook_relevant_tok_name-str = 'BEGIN'.
        l_scanned_elem_name = <fs_abap_doc_block>-hook_relevant_tok_name_add-str.
      else.
        l_scanned_elem_name = <fs_abap_doc_block>-hook_relevant_tok_name-str.
      endif.

      if l_scanned_elem_name = l_element_name.

        " prepare the result for required element
        loop at <fs_abap_doc_block>-tab_comments assigning field-symbol(<adoc_line>).
          condense <adoc_line>.         " remove leading spaces
          <adoc_line> = <adoc_line>+2.  " remove "!
          condense <adoc_line>.         " remove again leading spaces
          if sy-tabix = 1.
            result = <adoc_line>.
          else.
            concatenate result <adoc_line> into result separated by space.
          endif.
        endloop.
        element_was_found = abap_true.
        exit.
      endif.
    endloop.

    if element_was_found = abap_false.
      raise exception type cx_oo_abap_doc_reader
        exporting
          element_name            = l_element_name.
    endif.

  endmethod.

endclass.

CLASS cl_oo_abap_doc_reader DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS get_abap_doc_for_element
      IMPORTING
        clif_name     TYPE string
        element_name  TYPE string
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.

CLASS cl_oo_abap_doc_reader IMPLEMENTATION.
  METHOD get_abap_doc_for_element.
    ASSERT 1 = 'todo'.
  ENDMETHOD.
ENDCLASS.

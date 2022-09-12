CLASS cl_oo_classname_service DEFINITION PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_rowrow,
             clsname TYPE string,
             cpdname TYPE string,
           END OF ty_rowrow.
    TYPES: BEGIN OF ty_row,
             cpdkey TYPE ty_rowrow,
           END OF ty_row.
    TYPES ty_result TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
    CLASS-METHODS get_all_method_includes
      IMPORTING
        clsname       TYPE string
      RETURNING
        VALUE(result) TYPE ty_result.
ENDCLASS.

CLASS cl_oo_classname_service IMPLEMENTATION.
  METHOD get_all_method_includes.
    ASSERT 1 = 'todo'.
  ENDMETHOD.
ENDCLASS.

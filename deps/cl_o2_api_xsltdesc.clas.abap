CLASS cl_o2_api_xsltdesc DEFINITION PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_error,
             text TYPE string,
           END OF ty_error.
    TYPES ty_error_tab TYPE STANDARD TABLE OF ty_error WITH EMPTY KEY.
    CLASS-METHODS check_transformation_source
      IMPORTING
        i_name       TYPE string
        i_source     TYPE o2pageline_table
      EXPORTING
        e_error_list TYPE ty_error_tab.
ENDCLASS.

CLASS cl_o2_api_xsltdesc IMPLEMENTATION.
  METHOD check_transformation_source.
    ASSERT 1 = 'todo'.
  ENDMETHOD.
ENDCLASS.

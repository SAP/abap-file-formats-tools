CLASS zcl_aff_extreme_values DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      get_max_length
        IMPORTING element_description TYPE REF TO cl_abap_elemdescr
        RETURNING VALUE(result)       TYPE string,

      get_extrema
        IMPORTING element_description TYPE REF TO cl_abap_elemdescr
        EXPORTING VALUE(max)          TYPE string
                  VALUE(min)          TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS
      remove_leading_trailing_spaces
        CHANGING
          string_to_work_on TYPE string.
ENDCLASS.



CLASS zcl_aff_extreme_values IMPLEMENTATION.

  METHOD get_max_length.
    DATA(length) = element_description->output_length.
    IF length > 0.
      DATA length_as_string TYPE string.
      length_as_string = length.
      remove_leading_trailing_spaces( CHANGING string_to_work_on = length_as_string ).
      result = length_as_string.
    ENDIF.
  ENDMETHOD.

  METHOD remove_leading_trailing_spaces.
    SHIFT string_to_work_on RIGHT DELETING TRAILING space.
    SHIFT string_to_work_on LEFT DELETING LEADING space.
  ENDMETHOD.


  METHOD get_extrema.
    DATA r_field TYPE REF TO data.
    FIELD-SYMBOLS <field> TYPE any.
    CREATE DATA r_field TYPE HANDLE element_description.
    ASSIGN r_field->* TO <field>.

    DATA(max_val) = cl_abap_exceptional_values=>get_max_value( <field> ).
    ASSIGN max_val->* TO FIELD-SYMBOL(<max>).
    IF <max> IS ASSIGNED.
      max = <max>.
      REPLACE ALL OCCURRENCES OF 'E' IN max WITH 'e'.
      REPLACE ALL OCCURRENCES OF '+' IN max WITH ''.
      remove_leading_trailing_spaces( CHANGING string_to_work_on = max ).
    ENDIF.

    IF element_description->type_kind = cl_abap_typedescr=>typekind_decfloat OR
          element_description->type_kind = cl_abap_typedescr=>typekind_decfloat16 OR
          element_description->type_kind = cl_abap_typedescr=>typekind_decfloat34.
      IF <max> IS ASSIGNED.
        min = '-' && max.
      ENDIF.
    ELSE.
      DATA(min_val) = cl_abap_exceptional_values=>get_min_value( <field> ).
      ASSIGN min_val->* TO FIELD-SYMBOL(<min>).
      IF <min> IS ASSIGNED.
        DATA min_str TYPE string.
        min_str = <min>.
        DATA(length) = strlen( min_str ) - 1.
        DATA(front) = substring( val = min_str off = 0  len = length ).
        DATA(back) = substring( val = min_str off = length  len = 1 ).
        IF back = '-'.
          min = back && front.
        ELSE.
          min = min_str.
        ENDIF.
        REPLACE ALL OCCURRENCES OF 'E' IN min WITH 'e'.
        REPLACE ALL OCCURRENCES OF '+' IN min WITH ''.
        remove_leading_trailing_spaces( CHANGING string_to_work_on = min ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

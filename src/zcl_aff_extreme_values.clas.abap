class zcl_aff_extreme_values definition
  public
  final
  create public .

  public section.
    methods:
      get_max_length
        importing element_description type ref to cl_abap_elemdescr
        returning value(result)       type string,

      get_extrema
        importing element_description type ref to cl_abap_elemdescr
        exporting value(max)          type string
                  value(min)          type string.
  protected section.
  private section.
    methods
      remove_leading_trailing_spaces
        changing
          string_to_work_on type string.
endclass.



class zcl_aff_extreme_values implementation.

  method get_max_length.
    data(length) = element_description->output_length.
    if length > 0.
      data length_as_string type string.
      length_as_string = length.
      remove_leading_trailing_spaces( changing string_to_work_on = length_as_string ).
      result = length_as_string.
    endif.
  endmethod.

  method remove_leading_trailing_spaces.
    shift string_to_work_on right deleting trailing space.
    shift string_to_work_on left deleting leading space.
  endmethod.


  method get_extrema.
    data r_field type ref to data.
    field-symbols <field> type any.
    create data r_field type handle element_description.
    assign r_field->* to <field>.

    data(max_val) = cl_abap_exceptional_values=>get_max_value( <field> ).
    assign max_val->* to field-symbol(<max>).
    if <max> is assigned.
      max = <max>.
      replace all occurrences of 'E' in max with 'e'.
      replace all occurrences of '+' in max with ''.
      remove_leading_trailing_spaces( changing string_to_work_on = max ).
    endif.

    if element_description->type_kind = cl_abap_typedescr=>typekind_decfloat or
          element_description->type_kind = cl_abap_typedescr=>typekind_decfloat16 or
          element_description->type_kind = cl_abap_typedescr=>typekind_decfloat34.
      if <max> is assigned.
        min = '-' && max.
      endif.
    else.
      data(min_val) = cl_abap_exceptional_values=>get_min_value( <field> ).
      assign min_val->* to field-symbol(<min>).
      if <min> is assigned.
        data min_str type string.
        min_str = <min>.
        data(length) = strlen( min_str ) - 1.
        data(front) = substring( val = min_str off = 0  len = length ).
        data(back) = substring( val = min_str off = length  len = 1 ).
        if back = '-'.
          min = back && front.
        else.
          min = min_str.
        endif.
        replace all occurrences of 'E' in min with 'e'.
        replace all occurrences of '+' in min with ''.
        remove_leading_trailing_spaces( changing string_to_work_on = min ).
      endif.
    endif.
  endmethod.
endclass.

*"* use this source file for your ABAP unit test classes
class ltcl_json_writer definition final for testing
  duration short
  risk level harmless.

  private section.
    methods get_extrema for testing raising cx_static_check.
    methods max_length for testing raising cx_static_check.


endclass.


class ltcl_json_writer implementation.

  method get_extrema.
    data: integer    type i, "-2147483648 to +2147483647 for i
          decfloat16 type decfloat16, "1E385(1E-16 - 1) to -1E-383, 0, +1E-383 to 1E385(1 - 1E-16) for decfloat16
          ftype      type f, "2.2250738585072014E-308 to 1.7976931348623157E+308, positive as well as negative
          packed     type p length 14 decimals 2, "length multiplied by 2 minus 1 digits and can have a maximum of 14 decimal places
          integer1   type int1.
    data table_of_descriptions type standard table of ref to cl_abap_elemdescr with default key.
    types: begin of line_of_table,
             min type string,
             max type string,
           end of line_of_table.
    data table_of_expected type standard table of line_of_table with default key.
    insert cast cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( integer ) ) into table table_of_descriptions.
    insert value #( min = `-2147483648` max = `2147483647` ) into table table_of_expected.

    insert cast cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( decfloat16 ) ) into table table_of_descriptions.
    insert value #( min = `-9.999999999999999e384` max = `9.999999999999999e384` ) into table table_of_expected.

    insert cast cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( ftype ) ) into table table_of_descriptions.
    insert value #( min = `-1.7976931348623157e308` max = `1.7976931348623157e308` ) into table table_of_expected.

    insert cast cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( packed ) ) into table table_of_descriptions.
    insert value #( min = `-9999999999999999999999999.99` max = `9999999999999999999999999.99` ) into table table_of_expected.

    insert cast cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( integer1 ) ) into table table_of_descriptions.
    insert value #( min = `0` max = `255` ) into table table_of_expected.

    data cut type ref to zcl_aff_extreme_values.
    cut = new zcl_aff_extreme_values( ).
    loop at table_of_descriptions assigning field-symbol(<description>).
      data(table_index) = sy-tabix.
      cut->get_extrema(
        exporting
          element_description = <description>
        importing
          max                 = data(max)
          min                 = data(min)
      ).
      cl_abap_unit_assert=>assert_equals( exp = table_of_expected[ table_index ]-min act = min ).
      cl_abap_unit_assert=>assert_equals( exp = table_of_expected[ table_index ]-max act = max ).
    endloop.
  endmethod.

  method max_length.
    data cut type ref to zcl_aff_extreme_values.
    cut = new zcl_aff_extreme_values( ).
    data c_length_30 type c length 30.
    data(max_length) = cut->get_max_length(
        element_description = cast cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( c_length_30 ) )
    ).
    cl_abap_unit_assert=>assert_equals( exp = 30 act = max_length ).

    data c_length_60 type c length 60.
    max_length = cut->get_max_length(
        element_description = cast cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( c_length_60 ) )
    ).
    cl_abap_unit_assert=>assert_equals( exp = 60 act = max_length ).

    data n_length_10 type n length 10.
    max_length = cut->get_max_length(
        element_description = cast cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( n_length_10 ) )
    ).
    cl_abap_unit_assert=>assert_equals( exp = 10 act = max_length ).
  endmethod.

endclass.

class zcl_aff_test_types definition
  public
  final
  create public.

  public section.

    types:
      "! $hiddenabc
      unknown_annotation type string.

    types:
      "! <p class="shorttext">title</p>
      "! description
      "! <p class="shorttext2">Title</p>
      description_not_found type string.

    types:
      "! <p class="shorttext">Type With Long Description</p>
      "! This is a type with a very very long description.
      "! To ensure that the generated schema can be inserted into an include without loss of information,
      "! the length of the description should not be longer then 253 characters.
      "! So please shorten your description. Description should not replace a lexicon.
      type_with_long_description type i.

    types:
      "! Default type does not match constant type
      "! $default {@link zcl_aff_test.data:co_test.test}
      default_link type i.
    constants:
      begin of co_test,
        test type string value ' ',
      end of co_test.

    types:
      begin of struc_link_wrong_type,
        default_link type default_link,
      end of struc_link_wrong_type.


    types:
      "! in ST val(I()) only allow integers
      "! $values {@link zcl_aff_test_types.data:co_enum}
      enum type i.

    constants:
      begin of co_enum,
        test  type string value ' ',
        test2 type string value 'A',
      end of co_enum.

*  type to test name mapping from format_version to formatVersion
    types:
      "! <p class="shorttext">Constant With Field Format Version</p>
      "! Constant with field format_version
      begin of ty_format_version,
        "! <p class="shorttext">ABAP File Format Version</p>
        "! The ABAP file format version
        format_version type string,
        "! <p class="shorttext">Other Field</p>
        "! Other field
        field1         type i,
      end of ty_format_version.

*  numerical text field
    types:
      "! <p   class="shorttext">Numerical Text Field</p>
      "! A numerical text field of length 4
      num_text type n length 4.

    types:
      "! <p   class="shorttext">Structure With Numerical Text Field</p>
      "! Structure with a numerical text field of length 4
      begin of struc_with_num_text,
        numerical_text1 type num_text,
        numerical_text2 type num_text,
        "! $showAlways
        numerical_text3 type num_text,
        numerical_text4 type num_text,
      end of struc_with_num_text.


*  numerical text field with title and description which need to be escaped in the json schema
*    " needs to be escaped (/")
*    \ needs to be escaped (\\)
    types:
      "! <p   class="shorttext" lang="en"   >Test title "\</p>
      "! Test description "\
      num_text1 type n length 4.


* integer
    types:
      "! <p   class="shorttext" lang="en"   >myInteger</p>
      "! A simple Integer
      integer type i.


* string
    types:
      "! <p class="shorttext">myStringName</p>
      "! This is a string
      "! $maxLength 3
      mystring type string.


* date
    types:
      "! <p   class="shorttext">Date</p>
      "! This is a date
      my_date type d.


* simple structure
    types:
      "! <p class="shorttext synchronized" >mySimpleStructure</p>
      "! This is a simple structure
      begin of my_structure,
        "! <p class="shorttext">First Element</p>
        "! This is the first element
        "! $minLength 4
        my_first_element  type mystring,
        "! <p class="shorttext">Second Element</p>
        "! This is the second element
        my_second_element type i,
      end of my_structure.


* simple table:
    types:
    "! <p class="shorttext">A Standard Table</p>
    "! A standard table of myString
    my_standard_table type standard table of mystring with default key.

    types:
    "! <p class="shorttext">A Hashed Table</p>
    "! A hashed table of my_structure
    my_hashed_table type hashed table of my_structure with unique key my_first_element.

* structure with different table types
    types:
    "! <p class="shorttext">A Sorted Table</p>
    "! A sorted table of my_structure with unique key
    my_sorted_table_unique type sorted table of my_structure with unique key my_second_element.

    types:
    "! <p class="shorttext">A Sorted Table</p>
    "! A sorted table of my_structure with non unique key
    my_sorted_table_n_unique type sorted table of my_structure with non-unique key my_second_element.

    types:
      "! <p class="shorttext">A Structure With Tables</p>
      "! A structure with different table types
      begin of my_structure_with_tables,
        "! <p class="shorttext">First Table</p>
        "! First table
        first_table  type my_sorted_table_unique,
        "! <p class="shorttext">Second Table</p>
        "! Second table
        second_table type my_sorted_table_n_unique,
      end of my_structure_with_tables.


* simple structure
    types:
      "! <p class="shorttext synchronized" >mySimpleStructure</p>
      "! This is a $ simple st$ructure
      begin of my_structure2,
        "! <p class="shorttext">First Element</p>
        "! This is the first element
        "! $required
        my_first_element  type mystring,
        "! <p class="shorttext">Second Element</p>
        "! This is the second element
        my_second_element type i,
      end of my_structure2.


* nested Structure
    types:
      "! <p class="shorttext">Nested Structure</p>
      "! This is a nested structure
      begin of my_nested_structure,
        "! <p class="shorttext synchronized" >myElementComponente </p>
        "! This is a string element
        my_element type string,
      end of my_nested_structure,
      "! <p class="shorttext">myStructure</p>
      "! This is a complex structure
      begin of my_structure3,
        "! <p class="shorttext">nestedStruc</p>
        "! This is the nested structure
        nested_struc type my_nested_structure,
        "! <p class="shorttext">My Element</p>
        "! This is my element
        "! $required
        my_element   type string,
      end of my_structure3.


* type with enum values:
    types:
     "! <p class="shorttext"> myCategory </p>
     "! This is an enum
     "! $values    {    @link    zcl_aff_test_types.data:enum_values    }
     category type n length 2.

    constants:
      "! <p class="shorttext">Interface Category</p>
      "! Interface category
      begin of enum_values,
        "! <p class="shorttext">generalCategory</p>
        "! General interface
        general      type category value '00',
        "! Interface definition of a classic BAdI
        classic_badi type category value '01',
      end of enum_values ##NEEDED.

* type with enum values without initial:
    types:
     "! <p class="shorttext">Enum </p>
     "! This is an enum
     "! $values    {    @link    zcl_aff_test_types.data:enum_values_no_initial    }
     category_no_initial type n length 2.

    constants:
      "! <p class="shorttext">Interface Category</p>
      "! Interface category
      begin of enum_values_no_initial,
        "! <p class="shorttext">Component 1</p>
        "! Component 1
        component_1 type category_no_initial value '01',
        "! <p class="shorttext">Component 2</p>
        "! Component 2
        component_2 type category_no_initial value '02',
      end of enum_values_no_initial ##NEEDED.

    types:
      "! <p class="shorttext">Structure with Different Enum Types</p>
      "! Structure with different enum types
      begin of structure_with_different_enum,
        enum_without_all   type category_no_initial,
        "! $default {@link zcl_aff_test_types.data:enum_values_no_initial.component_2 }
        enum_with_default  type category_no_initial,
        "! $required
        enum_with_required type category_no_initial,
      end of structure_with_different_enum.


* complex structure with enum_values
    types:
      "! <p class="shorttext"> Class Properties </p>
      "! Class properties
      begin of ty_class_properties,
        header         type if_aff_types_v1=>ty_header_60,
        "! <p class="shorttext"> Class Category </p>
        "! Class category
        "! $values {@link zcl_aff_test_types.data:co_class_category}
        class_category type n length 2,
      end of ty_class_properties.

    constants:
      "! <p class="shorttext">Class Category</p>
      "! Class category
      begin of co_class_category,
        "! <p class="shorttext">General</p>
        "! General
        general    type vseoclass-category  value '00',
        "! <p class="shorttext">Exit Class</p>
        "! Exit class
        exit_class type vseoclass-category  value '01',
      end of co_class_category.


* deep nested structure
    types:
      "! <p class="shorttext">outerStructure</p>
      "! ABAP Doc Comment TYPES list first level
      begin of list,
        "! <p class="shorttext">Outer Element 1</p>
        "! ABAP Doc field1
        "! $showAlways
        field1 type i,
        "! <p class="shorttext">Outer Element 2</p>
        "! ABAP Doc field2
        "! $required
        field2 type c length 2,
        "! <p class="shorttext">middleStructure</p>
        "! ABAP Doc list second level
        "! $required
        begin of list1,
          "! <p class="shorttext">Middle Element</p>
          "! ABAP Doc second level
          "! $required
          element_of_list1 type i,
          "! <p class="shorttext">innerStructure</p>
          "! ABAP Doc third level
          begin of list2,
            "! <p class="shorttext">Inner Element</p>
            "! ABAP Doc third level
            "! $required
            element_of_list2 type string,
          end of list2,
        end of list1,
        "! <p class="shorttext">Outer Element 3</p>
        "! ABAP Doc field3
        "! $required
        field3 type c length 2,
      end of list.


* nested structure with table
    types:
      "! <p class="shorttext">outerStructure</p>
      "! ABAP Doc Comment TYPES list first level
      begin of outer_struc,
        "! <p class="shorttext">Outer Element 1</p>
        "! ABAP Doc field1 first level
        field1 type i,
        "! <p class="shorttext">Inner Structure 1</p>
        "! Inner structure
        begin of inner_struc,
          "! <p class="shorttext">Inner Element</p>
          "! ABAP Doc element second level
          "! $required
          element_of_inner_struc type i,
          "! <p class="shorttext">inner Table</p>
          "! ABAP Doc element second level
          "! $required
          inner_table_var        type standard table of string with non-unique default key,
        end of inner_struc,
        "! <p class="shorttext">Outer Element 2</p>
        "! ABAP Doc field2 first level
        "! $required
        field2 type c length 2,
      end of outer_struc.


* nested structure with table and enum value
    types:
      "! <p class="shorttext">Title of aff_test_type</p>
      "! Description of aff_test_type
      begin of aff_test_type,
        "! <p class="shorttext">Title of Field1</p>
        "! Description of field1
        field1            type i,
        "! <p class="shorttext">Title of inner_struc</p>
        "! Description of inner_struc
        "! $showAlways
        begin of inner_struc,
          "! <p class="shorttext">Title of inner_element</p>
          "! Description of inner_element
          "! $required
          inner_element type i,
          "! <p class="shorttext">Title of inner_table</p>
          "! Description of inner_table
          inner_table   type standard table of string with non-unique default key,
        end of inner_struc,
        "! <p class="shorttext">Title of field2</p>
        "! Description of field2
        "! $required
        field2            type c length 2,
        "! <p class="shorttext">Title of field_with_values</p>
        "! Description of field_with_values
        "! $values {@link zcl_aff_test_types.data:co_class_category}
        field_with_values type n length 2,
      end of aff_test_type.


* nested table
    types:
      "! <p class="shorttext">Inner Table</p>
      "! This is the inner Table
      nested_table type standard table of string with default key,
      "! <p class="shorttext">Outer Table</p>
      "! This is the outer table
      first_table  type standard table of nested_table with default key.


* type with enum values, but wrong link:
    types:
     "! <p class="shorttext"> myCategory </p>
     "! This is an enum
     "! $values    {    @link    zcl_aff_test_types.data:enum_values_wrong    }
     category_wrong type n length 2.

*  structure with component to wrong enum_values link
    types:
      "! <p class="shorttext"> Structure with Wrong Link </p>
      "! This is a structure with wrong enum_values link
      begin of structure_with_wrong_link,
        "! <p class="shorttext"> First Element </p>
        "! First element
        element_one type string,
        "! <p class="shorttext"> Second Element </p>
        "! Second element
        element_two type category_wrong,
      end of structure_with_wrong_link.

* structure with enum value whose link can be found outside
    "! <p class="shorttext"> ABAP Language Version </p>
    "! ABAP language version
    "! $values {@link zcl_aff_test_types.data:co_abap_language_version}
    types language_version type c length 1.

    constants:
      "! <p class="shorttext"> ABAP Language Version </p>
      "! ABAP language version
      begin of co_abap_language_version,
        "! <p class="shorttext">Standard</p>
        "! Standard
        standard          type language_version value ' ',
        "! <p class="shorttext">ABAP Cloud Development</p>
        "! ABAP cloud development
        cloud_development type language_version value '5',
      end of co_abap_language_version.

    types:
      "! <p class="shorttext"> Header </p>
      "! The header for an ABAP main object
      begin of header,
        "! <p class="shorttext"> Description</p>
        "! Description of the ABAP object
        description        type string,
        "! <p class="shorttext"> Original Language</p>
        "! Original language of the ABAP object
        master_language    type c length 2,
        "! <p class="shorttext"> ABAP Language Version</p>
        "! ABAP language version
        abap_langu_version type language_version,
      end of header.


    types:
      "! <p class="shorttext"> String Table</p>
      "! Abap Doc of the table
      string_table type standard table of string with default key.
    types:
      "! <p class="shorttext"> Inner Structure</p>
      "! Abap Doc of the structure
      begin of inner_struc,
        "! <p class="shorttext">Field</p>
        "! Field
        field type i,
      end of inner_struc.
    types:
      "! <p class="shorttext"> Structure With ABAP Doc Outside</p>
      "! Structure with ABAP Doc outside
      begin of structure_with_doc_outside,
        "! <p class="shorttext">Table1</p>
        "! Table1
        "! $required
        table1    type string_table,
        "! <p class="shorttext">Structure</p>
        "! Structure
        structure type inner_struc,
        "! <p class="shorttext">Table2</p>
        "! Table2
        table2    type string_table,
      end of structure_with_doc_outside.


* Types for number annotations

    types:
      "! <p class="shorttext">Integer Outside</p>
      "! Integer outside
      "! $minimum: -25
      integer_outside type i.


    types:
      "! <p class="shorttext">Structure With Number Types </p>
      "! This is a structure with different number types
      begin of structure_with_numbers,
        "! <p class="shorttext">Integer With Maximum </p>
        "! Integer with maximum
        "! $maximum: 10
        integer                 type i,
        "! <p class="shorttext">Float With Minimum And Exclusive Maximum </p>
        "! Float with minimum and exclusive maximum
        "! $exclusiveMaximum:100.9
        "! $minimum: -0.42
        float                   type decfloat16,
        "! <p class="shorttext">Packed Number With Given Multiple</p>
        "! Packed number with given multiple
        "! $multipleOf: 0.1
        "! $exclusiveMinimum: 0
        "! $maximum: 99999.90
        packed_with_multiple    type p length 4 decimals 2,
        "! <p class="shorttext">Packed Number With No Given Multiple</p>
        "! Packed number with no given multiple
        "! $exclusiveMinimum: 0
        packed_without_multiple type p length 4 decimals 1,
        "! <p class="shorttext">Integer Defined Outside</p>
        "! Integer defined outside and ABAP Doc number annotation outside
        integer_out             type integer_outside,
        "! <p class="shorttext">Integer Defined Outside</p>
        "! Integer defined outside but with ABAP Doc number annotation here
        "! $maximum: 42
        integer_out_with_doc    type integer_outside,
      end of structure_with_numbers.


* Types for default annotations

    types:
      "! <p class="shorttext">Structure With Default</p>
      "! Structure to test default checks in simple transformation
      begin of structure_different_default,
        "! <p class="shorttext">Four Byte Integer</p>
        "! Four byte integer
        "! $default '5'
        four_byte_int    type i,
        "! <p class="shorttext">Eight Byte Integer</p>
        "! Eight byte integer
        "! $default '55'
        eight_byte_int   type int8,
        "! <p class="shorttext">Binary Floating Point Number</p>
        "! Binary floating point number
        "! $default '4.3'
        bin_float        type f,
        "! <p class="shorttext">Byte Like</p>
        "! Byte like
        "! $default 'FFFF'
        byte_like        type x length 2,
        "! <p class="shorttext">Byte Like2</p>
        "! Byte like2
        "! $default 'FF00FF'
        byte_like2       type xstring,
        "! <p class="shorttext">Decimal Floating Point Number</p>
        "! Decimal floating point number with 16 places
        "! $default '25.26'
        decimal_float_16 type decfloat16,
        "! <p class="shorttext">Decimal Floating Point Number</p>
        "! Decimal floating point number with 34 places
        "! $default '123.05'
        decimal_float_34 type decfloat34,
        "! <p class="shorttext">Packed Number</p>
        "! Packed number
        "! $default '123.45'
        packed_number    type p length 3 decimals 2,
        "! <p class="shorttext">Numeric Text Field</p>
        "! Numeric text field
        "! $default '1067'
        numeric_text     type n length 4,
        "! <p class="shorttext">Character Text</p>
        "! Character text
        "! $default 'abcde'
        character_text   type c length 5,
        "! <p class="shorttext">String Text</p>
        "! String text
        "! $default 'Default text'
        string_text      type string,
        "! <p class="shorttext">Date</p>
        "! Date
        "! $default '19720401'
        date_field       type d,
        "! <p class="shorttext">Time</p>
        "! Time
        "! $default '201500'
        time_field       type t,
        "!  <p class="shorttext">Date Time</p>
        "! Date time: No support
        "! $default '9999-12-31T23:59:59.9999999'
        date_time_field  type utclong,
        "! <p class="shorttext">Boolean</p>
        "! Boolean with default abap_true
        "! $default 'abap_true'
        bool_true        type abap_bool,
        "! <p class="shorttext">Boolean</p>
        "! Boolean with default abap_false
        "! $default 'abap_false'
        bool_false       type abap_bool,
        "! <p class="shorttext">Enum Type</p>
        "! Enum type
        "! $default {@link zcl_aff_test_types.data:co_class_category.exit_class }
        "! $values  {@link zcl_aff_test_types.data:co_class_category }
        enum_type        type n length 2,
      end of structure_different_default.

    types:
      "! <p class="shorttext">Structure With Default</p>
      "! Structure with default
      begin of structure_with_default_problem,
        "! <p class="shorttext">Integer</p>
        "! Integer
        "! $default: '5'
        "! $required
        integer          type i,
        "! <p class="shorttext">String Element</p>
        "! String element with default value
        "! $default: 'DefaultString'
        "! $showAlways
        string_element   type string,
        "! <p class="shorttext">Enum Value</p>
        "! Enum value with default
        "! $default {@link zcl_aff_test_types.data:co_class_category.exit_class }
        "! $values  {@link zcl_aff_test_types.data:co_class_category }
        "! $required
        enum_required    type n length 2,
        "! <p class="shorttext">Enum Value</p>
        "! Enum value with default
        "! $default {@link zcl_aff_test_types.data:co_class_category.exit_class }
        "! $values  {@link zcl_aff_test_types.data:co_class_category }
        "! $showAlways
        enum_show_always type n length 2,
      end of structure_with_default_problem.

    types:
      "! <p class="shorttext">Inner Structure</p>
      "! Inner structure
      begin of inner_struc_with_default,
        "! <p class="shorttext">Inner Component</p>
        "! Inner component
        "! $default 'Default Value'
        inner_component type string,
      end of inner_struc_with_default,

      "! <p class="shorttext">Middle Structure</p>
      "! Middle structure
      begin of middle_struc_with_default,
        "! <p class="shorttext">Middle Component</p>
        "! Middle component
        "! $default 'abcd'
        middle_component type c length 4,
        "! <p class="shorttext">Inner Structure</p>
        "! Inner structure
        inner_struc      type inner_struc_with_default,
      end of middle_struc_with_default,

      "! <p class="shorttext">Nested Structure</p>
      "! Nested structure
      begin of nested_struc_with_default,
        "! <p class="shorttext">Outer Component</p>
        "! Outer component
        "! $default '10'
        outer_component type i,
        "! <p class="shorttext">Middle Structure</p>
        "! Middle structure
        middle_struc    type middle_struc_with_default,
      end of nested_struc_with_default.

*  component with wrong default links
    types:
      "! <p class="shorttext">Structure With Wrong Default</p>
      "! Structure with wrong default
      begin of structure_with_wrong_default,
        "! <p class="shorttext">First Element</p>
        "! First element
        "! $default {@link zcl_aff_test_types.data:enum_values.wrong_component }
        element_one type category,
        "! <p class="shorttext">Second Element</p>
        "! Second element
        "! $default {@link wrong_link }
        element_two type category,
      end of structure_with_wrong_default.


* Types for callbackClass annotation

* string with callbackClass
    types:
      "! <p class="shorttext">String With Callback</p>
      "! This is a String with a CallbackClass
      "! $maxLength 3
      "! $callbackClass {     @link    zcl_aff_test_types    }
    string_callback type string.


* table with callback, components are strings
    types:
      "! <p class="shorttext">my_table</p>
      "! A standard table of strings with CallbackClass
      "! $callbackClass {     @link    zcl_aff_test_types    }
      "! $required
      table_with_callback type standard table of mystring with default key.


* table with callback, components are structures
    types:
      "! <p class="shorttext">my_table</p>
      "! A standard table of my_structure
      "! $callbackClass {     @link    zcl_aff_test_types    }
      table_call_of_struc type standard table of my_structure with default key.


* table with callback, components are tables
    types:
      "! <p class="shorttext">my_table_of_table</p>
      "! A standard table of my_table
      "! $callbackClass {     @link    zcl_aff_test_types   }
      table_call_of_table type standard table of my_standard_table with default key.


* simple structure with callback
    types:
      "! <p class="shorttext">Structure With Callback</p>
      "! Structure with callback
      "! $callbackClass {     @link    zcl_aff_test_types    }
      begin of structure_with_callback,
        "! <p class="shorttext">First Element</p>
        "! This is the first element
        my_element type i,
      end of structure_with_callback.



* table of my_structure_with_callback
    types:
      "! <p class="shorttext">my_table</p>
      "! A standard table of my_structure_with_callback
      table_of_struc_with_callback type standard table of structure_with_callback with default key.


* simple structure with component my_table_with_callback
    types:
      "! <p class="shorttext synchronized" >mySimpleStructure</p>
      "! This is a simple structure
      begin of struc_of_table_with_callback,
        "! <p class="shorttext synchronized" >First Element</p>
        "! This is the first element
        "! $required
        my_table_with_callback type table_with_callback,
        "! <p class="shorttext synchronized" >Second Element</p>
        "! This is the second element
        my_second_element      type i,
      end of struc_of_table_with_callback.

* simple structure with component my_structure_with_callback
    types:
      "! <p class="shorttext synchronized" >mySimpleStructure</p>
      "! This is a simple structure
      begin of struc_in_struc_with_callback,
        "! <p class="shorttext synchronized" >First Element</p>
        "! This is the first element
        my_first_element       type string,
        "! <p class="shorttext synchronized" >Second Element</p>
        "! This is the second element
        "! $required
        my_struc_with_callback type structure_with_callback,
        "! <p class="shorttext synchronized" >Third Element</p>
        "! This is the third element
        my_third_element       type i,
      end of struc_in_struc_with_callback.

*  simple structure with component callback
    types:
      "! <p class="shorttext synchronized" >Simple Structure</p>
      "! This is a  simple structure
      begin of structure_with_elem_callback,
        "! <p class="shorttext synchronized" >First Element</p>
        "! This is the first element
        "! $minLength 2
        "! $required
        "! $callbackClass {     @link    zcl_aff_test_types    }
        my_first_element  type mystring,
        "! <p class="shorttext synchronized" >Second Element</p>
        "! This is the second element
        my_second_element type i,
      end of structure_with_elem_callback.

*      simple structure with wrong callbackclass link
    types:
      "! <p class="shorttext synchronized" >Structure With Wrong Callback</p>
      "! Structure with wrong callback
      begin of structure_with_wrong_callback,
        "! <p class="shorttext synchronized" >First Element</p>
        "! This is the first element
        "! $minLength 2
        "! $callbackClass {     @link    cl_aff_notest_types_for_writer    }
        "! $required
        my_first_element  type mystring,
        "! <p class="shorttext synchronized" >Second Element</p>
        "! This is the second element
        "! $callbackClass {   wrong_link    }
        "! $maximum 4
        my_second_element type i,
      end of structure_with_wrong_callback.

*      Types with missing titles and description
    types:
      element_no_title_descr type string.

    types:
      begin of inner_struc_no_title_descr,
        "! <p class="shorttext" >Inner Field</p>
        "! Inner field
        inner_field type i,
      end of inner_struc_no_title_descr.

    types:
      table_no_title_descr type standard table of string with empty key.

    types:
      begin of structure_no_title_descr,
        "! Only description
        field1      type string,
        "! <p class="shorttext synchronized" >Only Title</p>
        inner_struc type inner_struc_no_title_descr,
        inner_table type table_no_title_descr,
      end of structure_no_title_descr.


    types:
      begin of ty_include_type,
        "! <p class="shorttext">First Element In Include</p>
        "! $required
        first_element  type string,
        second_element type my_structure,
        "! <p class="shorttext">Third Element In Include</p>
        "! Third element in include
        "! $default '10'
        third_element  type i,
      end of ty_include_type.

    types:
      "! <p class="shorttext">Structure With Include</p>
      "! Structure with include
      begin of structure_with_include.
        include type ty_include_type.
    types:
      "! Other element
      "! $required
        other_element   type i,
        "! <p class="shorttext">Other structure</p>
        "! Other Structure
        other_structure type my_structure,
      end of structure_with_include.

    constants: begin of choose_codes,
                 inner_element   type string value `inner_element`,
                 outer_element   type string value `outer_element`,
                 inner_structure type string value `inner_structure`,
                 outer_structure type string value `outer_structure`,
                 inner_table     type string value `inner_table`,
                 outer_table     type string value `outer_table`,
               end of choose_codes.

    class-data subschema type rswsourcet.

    class-data choose_code type string.

    class-data expected_var type ref to data.

    class-methods get_subschema
      returning value(subschema) type rswsourcet.

    class-methods serialize
      importing
        writer       type ref to if_sxml_writer
        element_name type any.

    class-methods deserialize
      importing
        reader       type ref to if_sxml_reader
      exporting
        element_name type any
      raising
        cx_sxml_error.

    class-methods set_expected
      importing
        expected_variable type any.

  protected section.
  private section.
    class-methods jump_to_end
      importing
        reader type ref to if_sxml_reader
      raising
        cx_sxml_parse_error.
endclass.



class zcl_aff_test_types implementation.


  method get_subschema.
    subschema = zcl_aff_test_types=>subschema.
  endmethod.


  method serialize.
    case choose_code.
      when choose_codes-inner_element.
        writer->write_attribute( name = 'name' value = 'elementName' ) ##NO_TEXT.
        writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
      when choose_codes-outer_element.
        writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
      when choose_codes-inner_structure.
        writer->write_attribute( name = 'name' value = 'elementName' ) ##NO_TEXT.
        writer->open_element( name = 'str' ).
        writer->write_attribute( name = 'name' value = 'component' ).
        writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
        writer->close_element(  ).
      when choose_codes-outer_table.
        writer->open_element( name   = 'str' ).
        writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
        writer->close_element(  ).
      when choose_codes-inner_table.
        writer->write_attribute( name = 'name' value = 'elementName' ) ##NO_TEXT.
        writer->open_element( name   = 'str' ).
        writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
        writer->close_element(  ).
      when choose_codes-outer_structure.
        writer->open_element( name   = 'str' ).
        writer->write_attribute( name = 'name' value = 'elementName' ) ##NO_TEXT.
        writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
        writer->close_element(  ).
      when others.
    endcase.
  endmethod.


  method deserialize.
    field-symbols:
        <attr>    type data.
    assign expected_var->* to <attr>.
    element_name = <attr>.
    jump_to_end( reader ).
  endmethod.


  method jump_to_end.
    reader->next_node( ).
    data(type_start) = reader->name.
    data exit type abap_boolean.
    if reader->node_type = if_sxml_node=>co_nt_element_close or choose_code = choose_codes-inner_element or choose_code = choose_codes-outer_element.
      exit = abap_true.
    endif.
    while exit = abap_false.
      reader->next_node( ).
      if reader->node_type = if_sxml_node=>co_nt_element_close and reader->name = type_start.
        exit = abap_true.
      endif.
    endwhile.
  endmethod.


  method set_expected.
    get reference of expected_variable into expected_var.
  endmethod.
endclass.

CLASS zcl_aff_test_types DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      "! $hiddenabc
      unknown_annotation TYPE string.

    TYPES:
      "! <p class="shorttext">title</p>
      "! description
      "! <p class="shorttext2">Title</p>
      description_not_found TYPE string.

    TYPES:
      "! <p class="shorttext">Type With Long Description</p>
      "! This is a type with a very very long description.
      "! To ensure that the generated schema can be inserted into an include without loss of information,
      "! the length of the description should not be longer then 253 characters.
      "! So please shorten your description. Description should not replace a lexicon.
      type_with_long_description TYPE i.

    TYPES:
      "! Default type does not match constant type
      "! $default {@link zcl_aff_test_types.data:co_test.test}
      default_link TYPE i.
    CONSTANTS:
      BEGIN OF co_test,
        test TYPE string VALUE ' ',
      END OF co_test.

    TYPES:
      BEGIN OF struc_link_wrong_type,
        default_link TYPE default_link,
      END OF struc_link_wrong_type.


    TYPES:
      "! in ST val(I()) only allow integers
      "! $values {@link zcl_aff_test_types.data:co_enum}
      enum TYPE i.

    CONSTANTS:
      BEGIN OF co_enum,
        test  TYPE string VALUE ' ',
        test2 TYPE string VALUE 'A',
      END OF co_enum.

*  type to test name mapping from format_version to formatVersion
    TYPES:
      "! <p class="shorttext">Constant With Field Format Version</p>
      "! Constant with field format_version
      BEGIN OF ty_format_version,
        "! <p class="shorttext">ABAP File Format Version</p>
        "! The ABAP file format version
        format_version TYPE string,
        "! <p class="shorttext">Other Field</p>
        "! Other field
        field1         TYPE i,
      END OF ty_format_version.

*  numerical text field
    TYPES:
      "! <p   class="shorttext">Numerical Text Field</p>
      "! A numerical text field of length 4
      num_text TYPE n LENGTH 4.

    TYPES:
      "! <p   class="shorttext">Structure With Numerical Text Field</p>
      "! Structure with a numerical text field of length 4
      BEGIN OF struc_with_num_text,
        numerical_text1 TYPE num_text,
        numerical_text2 TYPE num_text,
        "! $showAlways
        numerical_text3 TYPE num_text,
        numerical_text4 TYPE num_text,
      END OF struc_with_num_text.


*  numerical text field with title and description which need to be escaped in the json schema
*    " needs to be escaped (/")
*    \ needs to be escaped (\\)
    TYPES:
      "! <p   class="shorttext" lang="en"   >Test title "\</p>
      "! Test description "\
      num_text1 TYPE n LENGTH 4.


* integer
    TYPES:
      "! <p   class="shorttext" lang="en"   >myInteger</p>
      "! A simple Integer
      integer TYPE i.


* string
    TYPES:
      "! <p class="shorttext">myStringName</p>
      "! This is a string
      "! $maxLength 3
      mystring TYPE string.


* date
    TYPES:
      "! <p   class="shorttext">Date</p>
      "! This is a date
      my_date TYPE d.


* simple structure
    TYPES:
      "! <p class="shorttext synchronized" >mySimpleStructure</p>
      "! This is a simple structure
      BEGIN OF my_structure,
        "! <p class="shorttext">First Element</p>
        "! This is the first element
        "! $minLength 4
        my_first_element  TYPE mystring,
        "! <p class="shorttext">Second Element</p>
        "! This is the second element
        my_second_element TYPE i,
      END OF my_structure.

* simple structure, single field
    TYPES:
      "! <p class="shorttext synchronized" >mySimpleStructure</p>
      "! This is a simple structure
      BEGIN OF my_structure_single,
        "! <p class="shorttext">Foo Element</p>
        "! This is the first element
        "! $minLength 4
        foo_element TYPE mystring,
      END OF my_structure_single.

* simple structure, two fields
    TYPES:
      "! This is a two field structure
      BEGIN OF my_structure_two,
        "! foo1 element
        foo1 TYPE i,
        "! foo2 element
        foo2 TYPE i,
      END OF my_structure_two.

* simple table:
    TYPES:
    "! <p class="shorttext">A Standard Table</p>
    "! A standard table of myString
    my_standard_table TYPE STANDARD TABLE OF mystring WITH DEFAULT KEY.

    TYPES:
    "! <p class="shorttext">A Hashed Table</p>
    "! A hashed table of my_structure
    my_hashed_table TYPE HASHED TABLE OF my_structure WITH UNIQUE KEY my_first_element.

* structure with different table types
    TYPES:
    "! <p class="shorttext">A Sorted Table</p>
    "! A sorted table of my_structure with unique key
    my_sorted_table_unique TYPE SORTED TABLE OF my_structure WITH UNIQUE KEY my_second_element.

    TYPES:
    "! <p class="shorttext">A Sorted Table</p>
    "! A sorted table of my_structure with non unique key
    my_sorted_table_n_unique TYPE SORTED TABLE OF my_structure WITH NON-UNIQUE KEY my_second_element.

    TYPES:
      "! <p class="shorttext">A Structure With Tables</p>
      "! A structure with different table types
      BEGIN OF my_structure_with_tables,
        "! <p class="shorttext">First Table</p>
        "! First table
        first_table  TYPE my_sorted_table_unique,
        "! <p class="shorttext">Second Table</p>
        "! Second table
        second_table TYPE my_sorted_table_n_unique,
      END OF my_structure_with_tables.


* simple structure
    TYPES:
      "! <p class="shorttext synchronized" >mySimpleStructure</p>
      "! This is a $ simple st$ructure
      BEGIN OF my_structure2,
        "! <p class="shorttext">First Element</p>
        "! This is the first element
        "! $required
        my_first_element  TYPE mystring,
        "! <p class="shorttext">Second Element</p>
        "! This is the second element
        my_second_element TYPE i,
      END OF my_structure2.


* nested Structure
    TYPES:
      "! <p class="shorttext">Nested Structure</p>
      "! This is a nested structure
      BEGIN OF my_nested_structure,
        "! <p class="shorttext synchronized" >myElementComponente </p>
        "! This is a string element
        my_element TYPE string,
      END OF my_nested_structure,
      "! <p class="shorttext">myStructure</p>
      "! This is a complex structure
      BEGIN OF my_structure3,
        "! <p class="shorttext">nestedStruc</p>
        "! This is the nested structure
        nested_struc TYPE my_nested_structure,
        "! <p class="shorttext">My Element</p>
        "! This is my element
        "! $required
        my_element   TYPE string,
      END OF my_structure3.


* type with enum values:
    TYPES:
     "! <p class="shorttext"> myCategory </p>
     "! This is an enum
     "! $values    {    @link    zcl_aff_test_types.data:enum_values    }
     category TYPE n LENGTH 2.

    CONSTANTS:
      "! <p class="shorttext">Interface Category</p>
      "! Interface category
      BEGIN OF enum_values,
        "! <p class="shorttext">generalCategory</p>
        "! General interface
        general      TYPE category VALUE '00',
        "! Interface definition of a classic BAdI
        classic_badi TYPE category VALUE '01',
      END OF enum_values ##NEEDED.

* type with enum values without initial:
    TYPES:
     "! <p class="shorttext">Enum </p>
     "! This is an enum
     "! $values    {    @link    zcl_aff_test_types.data:enum_values_no_initial    }
     category_no_initial TYPE n LENGTH 2.

    CONSTANTS:
      "! <p class="shorttext">Interface Category</p>
      "! Interface category
      BEGIN OF enum_values_no_initial,
        "! <p class="shorttext">Component 1</p>
        "! Component 1
        component_1 TYPE category_no_initial VALUE '01',
        "! <p class="shorttext">Component 2</p>
        "! Component 2
        component_2 TYPE category_no_initial VALUE '02',
      END OF enum_values_no_initial ##NEEDED.

    TYPES:
      "! <p class="shorttext">Structure with Different Enum Types</p>
      "! Structure with different enum types
      BEGIN OF structure_with_different_enum,
        enum_without_all   TYPE category_no_initial,
        "! $default {@link zcl_aff_test_types.data:enum_values_no_initial.component_2 }
        enum_with_default  TYPE category_no_initial,
        "! $required
        enum_with_required TYPE category_no_initial,
      END OF structure_with_different_enum.

    "! <p class="shorttext"> ABAP Language Version </p>
    "! ABAP language version
    "! $values {@link zcl_aff_test_types.data:co_abap_language_version}
    "! $default {@link zcl_aff_test_types.data:co_abap_language_version.standard}
    TYPES language_version TYPE c LENGTH 1.

    CONSTANTS:
      "! <p class="shorttext"> ABAP Language Version </p>
      "! ABAP language version
      BEGIN OF co_abap_language_version,
        "! <p class="shorttext">Standard</p>
        "! Standard
        standard          TYPE language_version VALUE ' ',
        "! <p class="shorttext">ABAP Cloud Development</p>
        "! ABAP cloud development
        cloud_development TYPE language_version VALUE '5',
      END OF co_abap_language_version.

    TYPES:
      "! <p class="shorttext"> Header </p>
      "! The header for an ABAP main object
      BEGIN OF header,
        "! <p class="shorttext"> Description</p>
        "! Description of the ABAP object
        description           TYPE string,
        "! <p class="shorttext"> Original Language</p>
        "! Original language of the ABAP object
        original_language     TYPE sy-langu,
        "! <p class="shorttext"> ABAP Language Version</p>
        "! ABAP language version
        abap_language_version TYPE language_version,
      END OF header.

* complex structure with enum_values
    TYPES:
      "! <p class="shorttext"> Class Properties </p>
      "! Class properties
      BEGIN OF ty_class_properties,
        header         TYPE header,
        "! <p class="shorttext"> Class Category </p>
        "! Class category
        "! $values {@link zcl_aff_test_types.data:co_class_category}
        "! $default {@link zcl_aff_test_types.data:co_class_category.general}
        class_category TYPE n LENGTH 2,
      END OF ty_class_properties.

    CONSTANTS:
      "! <p class="shorttext">Class Category</p>
      "! Class category
      BEGIN OF co_class_category,
        "! <p class="shorttext">General</p>
        "! General
        general    TYPE n LENGTH 2 VALUE '00',
        "! <p class="shorttext">Exit Class</p>
        "! Exit class
        exit_class TYPE n LENGTH 2 VALUE '01',
      END OF co_class_category.


* deep nested structure
    TYPES:
      "! <p class="shorttext">outerStructure</p>
      "! ABAP Doc Comment TYPES list first level
      BEGIN OF list,
        "! <p class="shorttext">Outer Element 1</p>
        "! ABAP Doc field1
        "! $showAlways
        field1 TYPE i,
        "! <p class="shorttext">Outer Element 2</p>
        "! ABAP Doc field2
        "! $required
        field2 TYPE c LENGTH 2,
        "! <p class="shorttext">middleStructure</p>
        "! ABAP Doc list second level
        "! $required
        BEGIN OF list1,
          "! <p class="shorttext">Middle Element</p>
          "! ABAP Doc second level
          "! $required
          element_of_list1 TYPE i,
          "! <p class="shorttext">innerStructure</p>
          "! ABAP Doc third level
          BEGIN OF list2,
            "! <p class="shorttext">Inner Element</p>
            "! ABAP Doc third level
            "! $required
            element_of_list2 TYPE string,
          END OF list2,
        END OF list1,
        "! <p class="shorttext">Outer Element 3</p>
        "! ABAP Doc field3
        "! $required
        field3 TYPE c LENGTH 2,
      END OF list.

* deep nested structure, simple
    TYPES:
      BEGIN OF nsimple,
        BEGIN OF list1,
          "! <p class="shorttext">Inner Element</p>
          "! sdfsdf
          "! $required
          element TYPE string,
        END OF list1,
      END OF nsimple.

* nested structure with table
    TYPES:
      "! <p class="shorttext">outerStructure</p>
      "! ABAP Doc Comment TYPES list first level
      BEGIN OF outer_struc,
        "! <p class="shorttext">Outer Element 1</p>
        "! ABAP Doc field1 first level
        field1 TYPE i,
        "! <p class="shorttext">Inner Structure 1</p>
        "! Inner structure
        BEGIN OF inner_struc,
          "! <p class="shorttext">Inner Element</p>
          "! ABAP Doc element second level
          "! $required
          element_of_inner_struc TYPE i,
          "! <p class="shorttext">inner Table</p>
          "! ABAP Doc element second level
          "! $required
          inner_table_var        TYPE STANDARD TABLE OF string WITH NON-UNIQUE DEFAULT KEY,
        END OF inner_struc,
        "! <p class="shorttext">Outer Element 2</p>
        "! ABAP Doc field2 first level
        "! $required
        field2 TYPE c LENGTH 2,
      END OF outer_struc.


* nested structure with table and enum value
    TYPES:
      "! <p class="shorttext">Title of aff_test_type</p>
      "! Description of aff_test_type
      BEGIN OF aff_test_type,
        "! <p class="shorttext">Title of Field1</p>
        "! Description of field1
        field1            TYPE i,
        "! <p class="shorttext">Title of inner_struc</p>
        "! Description of inner_struc
        "! $showAlways
        BEGIN OF inner_struc,
          "! <p class="shorttext">Title of inner_element</p>
          "! Description of inner_element
          "! $required
          inner_element TYPE i,
          "! <p class="shorttext">Title of inner_table</p>
          "! Description of inner_table
          inner_table   TYPE STANDARD TABLE OF string WITH NON-UNIQUE DEFAULT KEY,
        END OF inner_struc,
        "! <p class="shorttext">Title of field2</p>
        "! Description of field2
        "! $required
        field2            TYPE c LENGTH 2,
      END OF aff_test_type.


* nested table
    TYPES:
      "! <p class="shorttext">Inner Table</p>
      "! This is the inner Table
      nested_table TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      "! <p class="shorttext">Outer Table</p>
      "! This is the outer table
      first_table  TYPE STANDARD TABLE OF nested_table WITH DEFAULT KEY.


* type with enum values, but wrong link:
    TYPES:
     "! <p class="shorttext"> myCategory </p>
     "! This is an enum
     "! $values    {    @link    zcl_aff_test_types.data:enum_values_wrong    }
     category_wrong TYPE n LENGTH 2.

*  structure with component to wrong enum_values link
    TYPES:
      "! <p class="shorttext"> Structure with Wrong Link </p>
      "! This is a structure with wrong enum_values link
      BEGIN OF structure_with_wrong_link,
        "! <p class="shorttext"> First Element </p>
        "! First element
        element_one TYPE string,
        "! <p class="shorttext"> Second Element </p>
        "! Second element
        element_two TYPE category_wrong,
      END OF structure_with_wrong_link.

* structure with enum value whose link can be found outside

    TYPES:
      "! <p class="shorttext"> String Table</p>
      "! Abap Doc of the table
      string_table TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    TYPES:
      "! <p class="shorttext"> Inner Structure</p>
      "! Abap Doc of the structure
      BEGIN OF inner_struc,
        "! <p class="shorttext">Field</p>
        "! Field
        field TYPE i,
      END OF inner_struc.
    TYPES:
      "! <p class="shorttext"> Structure With ABAP Doc Outside</p>
      "! Structure with ABAP Doc outside
      BEGIN OF structure_with_doc_outside,
        "! <p class="shorttext">Table1</p>
        "! Table1
        "! $required
        table1    TYPE string_table,
        "! <p class="shorttext">Structure</p>
        "! Structure
        structure TYPE inner_struc,
        "! <p class="shorttext">Table2</p>
        "! Table2
        table2    TYPE string_table,
      END OF structure_with_doc_outside.


* Types for number annotations

    TYPES:
      "! <p class="shorttext">Integer Outside</p>
      "! Integer outside
      "! $minimum: -25
      integer_outside TYPE i.


    TYPES:
      "! <p class="shorttext">Structure With Number Types </p>
      "! This is a structure with different number types
      BEGIN OF structure_with_numbers,
        "! <p class="shorttext">Integer With Maximum </p>
        "! Integer with maximum
        "! $maximum: 10
        integer                 TYPE i,
        "! <p class="shorttext">Float With Minimum And Exclusive Maximum </p>
        "! Float with minimum and exclusive maximum
        "! $exclusiveMaximum:100.9
        "! $minimum: -0.42
        float                   TYPE decfloat16,
        "! <p class="shorttext">Packed Number With Given Multiple</p>
        "! Packed number with given multiple
        "! $multipleOf: 0.1
        "! $exclusiveMinimum: 0
        "! $maximum: 99999.90
        packed_with_multiple    TYPE p LENGTH 4 DECIMALS 2,
        "! <p class="shorttext">Packed Number With No Given Multiple</p>
        "! Packed number with no given multiple
        "! $exclusiveMinimum: 0
        packed_without_multiple TYPE p LENGTH 4 DECIMALS 1,
        "! <p class="shorttext">Integer Defined Outside</p>
        "! Integer defined outside and ABAP Doc number annotation outside
        integer_out             TYPE integer_outside,
        "! <p class="shorttext">Integer Defined Outside</p>
        "! Integer defined outside but with ABAP Doc number annotation here
        "! $maximum: 42
        integer_out_with_doc    TYPE integer_outside,
      END OF structure_with_numbers.


* Types for default annotations

    TYPES:
      "! <p class="shorttext">Structure With Default</p>
      "! Structure to test default checks in simple transformation
      BEGIN OF structure_different_default,
        "! <p class="shorttext">Four Byte Integer</p>
        "! Four byte integer
        "! $default '5'
        four_byte_int    TYPE i,
        "! <p class="shorttext">Eight Byte Integer</p>
        "! Eight byte integer
        "! $default '55'
        eight_byte_int   TYPE int8,
        "! <p class="shorttext">Binary Floating Point Number</p>
        "! Binary floating point number
        "! $default '4.3'
        bin_float        TYPE f,
        "! <p class="shorttext">Byte Like</p>
        "! Byte like
        "! $default 'FFFF'
        byte_like        TYPE x LENGTH 2,
        "! <p class="shorttext">Byte Like2</p>
        "! Byte like2
        "! $default 'FF00FF'
        byte_like2       TYPE xstring,
        "! <p class="shorttext">Decimal Floating Point Number</p>
        "! Decimal floating point number with 16 places
        "! $default '25.26'
        decimal_float_16 TYPE decfloat16,
        "! <p class="shorttext">Decimal Floating Point Number</p>
        "! Decimal floating point number with 34 places
        "! $default '123.05'
        decimal_float_34 TYPE decfloat34,
        "! <p class="shorttext">Packed Number</p>
        "! Packed number
        "! $default '123.45'
        packed_number    TYPE p LENGTH 3 DECIMALS 2,
        "! <p class="shorttext">Numeric Text Field</p>
        "! Numeric text field
        "! $default '1067'
        numeric_text     TYPE n LENGTH 4,
        "! <p class="shorttext">Character Text</p>
        "! Character text
        "! $default 'abcde'
        character_text   TYPE c LENGTH 5,
        "! <p class="shorttext">String Text</p>
        "! String text
        "! $default 'Default text'
        string_text      TYPE string,
        "! <p class="shorttext">Date</p>
        "! Date
        "! $default '19720401'
        date_field       TYPE d,
        "! <p class="shorttext">Time</p>
        "! Time
        "! $default '201500'
        time_field       TYPE t,
        "!  <p class="shorttext">Date Time</p>
        "! Date time: No support
        "! $default '9999-12-31T23:59:59.9999999'
        date_time_field  TYPE utclong,
        "! <p class="shorttext">Boolean</p>
        "! Boolean with default abap_true
        "! $default 'abap_true'
        bool_true        TYPE abap_bool,
        "! <p class="shorttext">Boolean</p>
        "! Boolean with default abap_false
        "! $default 'abap_false'
        bool_false       TYPE abap_bool,
        "! <p class="shorttext">Enum Type</p>
        "! Enum type
        "! $default {@link zcl_aff_test_types.data:co_class_category.exit_class }
        "! $values  {@link zcl_aff_test_types.data:co_class_category }
        enum_type        TYPE n LENGTH 2,
      END OF structure_different_default.

    TYPES:
      "! <p class="shorttext">Structure With Default</p>
      "! Structure with default
      BEGIN OF structure_with_default_problem,
        "! <p class="shorttext">Integer</p>
        "! Integer
        "! $default: '5'
        "! $required
        integer          TYPE i,
        "! <p class="shorttext">String Element</p>
        "! String element with default value
        "! $default: 'DefaultString'
        "! $showAlways
        string_element   TYPE string,
        "! <p class="shorttext">Enum Value</p>
        "! Enum value with default
        "! $default {@link zcl_aff_test_types.data:co_class_category.exit_class }
        "! $values  {@link zcl_aff_test_types.data:co_class_category }
        "! $required
        enum_required    TYPE n LENGTH 2,
        "! <p class="shorttext">Enum Value</p>
        "! Enum value with default
        "! $default {@link zcl_aff_test_types.data:co_class_category.exit_class }
        "! $values  {@link zcl_aff_test_types.data:co_class_category }
        "! $showAlways
        enum_show_always TYPE n LENGTH 2,
      END OF structure_with_default_problem.

    "! <p class="shorttext">Original Language</p>
    "! Original language of the ABAP object
    TYPES ty_original_language TYPE sy-langu.
    TYPES: BEGIN OF ty_header_60_src,
             original_language TYPE ty_original_language,
           END OF ty_header_60_src.

    TYPES:
      "! <p class="shorttext">Inner Structure</p>
      "! Inner structure
      BEGIN OF inner_struc_with_default,
        "! <p class="shorttext">Inner Component</p>
        "! Inner component
        "! $default 'Default Value'
        inner_component TYPE string,
      END OF inner_struc_with_default,

      "! <p class="shorttext">Middle Structure</p>
      "! Middle structure
      BEGIN OF middle_struc_with_default,
        "! <p class="shorttext">Middle Component</p>
        "! Middle component
        "! $default 'abcd'
        middle_component TYPE c LENGTH 4,
        "! <p class="shorttext">Inner Structure</p>
        "! Inner structure
        inner_struc      TYPE inner_struc_with_default,
      END OF middle_struc_with_default,

      "! <p class="shorttext">Nested Structure</p>
      "! Nested structure
      BEGIN OF nested_struc_with_default,
        "! <p class="shorttext">Outer Component</p>
        "! Outer component
        "! $default '10'
        outer_component TYPE i,
        "! <p class="shorttext">Middle Structure</p>
        "! Middle structure
        middle_struc    TYPE middle_struc_with_default,
      END OF nested_struc_with_default.

*  component with wrong default links
    TYPES:
      "! <p class="shorttext">Structure With Wrong Default</p>
      "! Structure with wrong default
      BEGIN OF structure_with_wrong_default,
        "! <p class="shorttext">First Element</p>
        "! First element
        "! $default {@link zcl_aff_test_types.data:enum_values.wrong_component }
        element_one TYPE category,
        "! <p class="shorttext">Second Element</p>
        "! Second element
        "! $default {@link wrong_link }
        element_two TYPE category,
      END OF structure_with_wrong_default.


* Types for callbackClass annotation

* string with callbackClass
    TYPES:
      "! <p class="shorttext">String With Callback</p>
      "! This is a String with a CallbackClass
      "! $maxLength 3
      "! $callbackClass {     @link    zcl_aff_test_types    }
    simple_callback TYPE string.


* table with callback, components are strings
    TYPES:
      "! <p class="shorttext">my_table</p>
      "! A standard table of strings with CallbackClass
      "! $callbackClass {     @link    zcl_aff_test_types    }
      "! $required
      table_callback TYPE STANDARD TABLE OF mystring WITH DEFAULT KEY.


* table with callback, components are tables
    TYPES:
      "! <p class="shorttext">my_table_of_table</p>
      "! A standard table of my_table
      "! $callbackClass {     @link    zcl_aff_test_types   }
      table_call_of_table TYPE STANDARD TABLE OF my_standard_table WITH DEFAULT KEY.


* simple structure with callback
    TYPES:
      "! <p class="shorttext">Structure With Callback</p>
      "! Structure with callback
      "! $callbackClass {     @link    zcl_aff_test_types    }
      BEGIN OF structure_callback,
        "! <p class="shorttext">First Element</p>
        "! This is the first element
        element_name TYPE i,
      END OF structure_callback.



* table of my_structure_with_callback
    TYPES:
      "! <p class="shorttext">my_table</p>
      "! A standard table of my_structure_with_callback
      table_of_struc_with_callback TYPE STANDARD TABLE OF structure_callback WITH DEFAULT KEY.


* simple structure with component my_table_with_callback
    TYPES:
      "! <p class="shorttext synchronized" >mySimpleStructure</p>
      "! This is a simple structure
      BEGIN OF struc_of_table_with_callback,
        "! <p class="shorttext synchronized" >First Element</p>
        "! This is the first element
        "! $required
        element_table_callback TYPE table_callback,
        "! <p class="shorttext synchronized" >Second Element</p>
        "! This is the second element
        my_second_element      TYPE i,
      END OF struc_of_table_with_callback.

* simple structure with component my_structure_with_callback
    TYPES:
      "! <p class="shorttext synchronized" >mySimpleStructure</p>
      "! This is a simple structure
      BEGIN OF struc_in_struc_with_callback,
        "! <p class="shorttext synchronized" >First Element</p>
        "! This is the first element
        my_first_element           TYPE string,
        "! <p class="shorttext synchronized" >Second Element</p>
        "! This is the second element
        "! $required
        element_structure_callback TYPE structure_callback,
        "! <p class="shorttext synchronized" >Third Element</p>
        "! This is the third element
        my_third_element           TYPE i,
      END OF struc_in_struc_with_callback.

*  simple structure with component callback
    TYPES:
      "! <p class="shorttext synchronized" >Simple Structure</p>
      "! This is a  simple structure
      BEGIN OF structure_with_elem_callback,
        "! <p class="shorttext synchronized" >First Element</p>
        "! This is the first element
        "! $minLength 2
        "! $required
        "! $callbackClass {     @link    zcl_aff_test_types    }
        element_callback  TYPE mystring,
        "! <p class="shorttext synchronized" >Second Element</p>
        "! This is the second element
        my_second_element TYPE i,
      END OF structure_with_elem_callback.

*      simple structure with wrong callbackclass link
    TYPES:
      "! <p class="shorttext synchronized" >Structure With Wrong Callback</p>
      "! Structure with wrong callback
      BEGIN OF structure_with_wrong_callback,
        "! <p class="shorttext synchronized" >First Element</p>
        "! This is the first element
        "! $minLength 2
        "! $callbackClass {     @link    cl_aff_notest_types_for_writer    }
        "! $required
        my_first_element  TYPE mystring,
        "! <p class="shorttext synchronized" >Second Element</p>
        "! This is the second element
        "! $callbackClass {   wrong_link    }
        "! $maximum 4
        my_second_element TYPE i,
      END OF structure_with_wrong_callback.

*      Types with missing titles and description
    TYPES:
      element_no_title_descr TYPE string.

    TYPES:
      BEGIN OF inner_struc_no_title_descr,
        "! <p class="shorttext" >Inner Field</p>
        "! Inner field
        inner_field TYPE i,
      END OF inner_struc_no_title_descr.

    TYPES:
      table_no_title_descr TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    TYPES:
      BEGIN OF structure_no_title_descr,
        "! Only description
        field1      TYPE string,
        "! <p class="shorttext synchronized" >Only Title</p>
        inner_struc TYPE inner_struc_no_title_descr,
        inner_table TYPE table_no_title_descr,
      END OF structure_no_title_descr.


    TYPES:
      BEGIN OF ty_include_type,
        "! <p class="shorttext">First Element In Include</p>
        "! $required
        first_element  TYPE string,
        second_element TYPE my_structure,
        "! <p class="shorttext">Third Element In Include</p>
        "! Third element in include
        "! $default '10'
        third_element  TYPE i,
      END OF ty_include_type.

    TYPES:
      "! <p class="shorttext">Structure With Include</p>
      "! Structure with include
      BEGIN OF structure_with_include.
        INCLUDE TYPE ty_include_type.
    TYPES:
      "! Other element
      "! $required
        other_element   TYPE i,
        "! <p class="shorttext">Other structure</p>
        "! Other Structure
        other_structure TYPE my_structure,
      END OF structure_with_include.

    CONSTANTS:
      BEGIN OF co_overwritten_values,
        "! <p class="shorttext">Option 1</p>
        "! Option 1
        "! $enumValue 'AAAA'
        first_value  TYPE c LENGTH 2 VALUE 'AA',
        "! <p class="shorttext">Option 2</p>
        "! Option 2
        "! $enumValue 'BBBB'
        second_value TYPE c LENGTH 2 VALUE 'BB',
      END OF co_overwritten_values.

    TYPES:
      "! <p class="shorttext">Structure With Overwritten Enum Values</p>
      "! Structure with overwritten enum values
      BEGIN OF struc_with_own_enum_values,
        "! <p class="shorttext">Enum Component</p>
        "! Enum component
        "! $values  {@link zcl_aff_test_types.data:co_overwritten_values }
        "! $default {@link zcl_aff_test_types.data:co_overwritten_values.first_value }
        enum_component TYPE c LENGTH 2,
      END OF struc_with_own_enum_values.

    CLASS-DATA subschema TYPE string_table.

    CLASS-DATA expected_var TYPE REF TO data.

    CLASS-METHODS get_subschema
      RETURNING VALUE(subschema) TYPE string_table.

    CLASS-METHODS serialize
      IMPORTING
        writer                     TYPE REF TO if_sxml_writer
        simple_callback            TYPE simple_callback OPTIONAL
        structure_callback         TYPE structure_callback OPTIONAL
        table_callback             TYPE table_callback OPTIONAL
        element_callback           TYPE string OPTIONAL
        element_structure_callback TYPE structure_callback OPTIONAL
        element_table_callback     TYPE table_callback OPTIONAL.

    CLASS-METHODS deserialize
      IMPORTING
        reader                     TYPE REF TO if_sxml_reader
      EXPORTING
        simple_callback            TYPE simple_callback
        structure_callback         TYPE structure_callback
        table_callback             TYPE table_callback
        element_callback           TYPE string
        element_structure_callback TYPE structure_callback
        element_table_callback     TYPE table_callback
      RAISING
        cx_sxml_error.

    CLASS-METHODS set_expected
      IMPORTING
        expected_variable TYPE any.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS jump_to_end
      IMPORTING
        reader TYPE REF TO if_sxml_reader
      RAISING
        cx_sxml_parse_error.
ENDCLASS.



CLASS zcl_aff_test_types IMPLEMENTATION.


  METHOD get_subschema.
    subschema = zcl_aff_test_types=>subschema.
  ENDMETHOD.


  METHOD serialize.
    IF ( simple_callback IS SUPPLIED ).
      writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
    ELSEIF ( structure_callback IS SUPPLIED ).
      writer->open_element( name   = 'str' ).
      writer->write_attribute( name = 'name' value = 'elementName' ) ##NO_TEXT.
      writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
      writer->close_element( ).
    ELSEIF ( table_callback IS SUPPLIED ).
      writer->open_element( name   = 'str' ).
      writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
      writer->close_element( ).
    ELSEIF ( element_callback IS SUPPLIED ).
      writer->write_attribute( name = 'name' value = 'elementCallback' ) ##NO_TEXT.
      writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
    ELSEIF ( element_structure_callback IS SUPPLIED ).
      writer->write_attribute( name = 'name' value = 'elementStructureCallback' ) ##NO_TEXT.
      writer->open_element( name = 'str' ).
      writer->write_attribute( name = 'name' value = 'elementName' ).
      writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
      writer->close_element( ).
    ELSEIF ( element_table_callback IS SUPPLIED ).
      writer->write_attribute( name = 'name' value = 'elementTableCallback' ) ##NO_TEXT.
      writer->open_element( name   = 'str' ).
      writer->write_value( 'callbackClass was called' ) ##NO_TEXT.
      writer->close_element( ).
    ENDIF.
  ENDMETHOD.


  METHOD deserialize.
    FIELD-SYMBOLS:
        <attr>    TYPE data.

    reader->next_node( ).

    ASSIGN expected_var->* TO <attr>.
    IF ( simple_callback IS SUPPLIED ).
      simple_callback = <attr>.
    ELSEIF ( structure_callback IS SUPPLIED ).
      structure_callback = <attr>.
      jump_to_end( reader ).
    ELSEIF ( table_callback IS SUPPLIED ).
      table_callback = <attr>.
      jump_to_end( reader ).
    ELSEIF ( element_callback IS SUPPLIED ).
      element_callback = <attr>.
    ELSEIF ( element_structure_callback IS SUPPLIED ).
      element_structure_callback = <attr>.
      jump_to_end( reader ).
    ELSEIF ( element_table_callback IS SUPPLIED ).
      element_table_callback = <attr>.
      jump_to_end( reader ).
    ENDIF.
  ENDMETHOD.


  METHOD jump_to_end.
    DATA(type_start) = reader->name.
    DATA exit TYPE abap_boolean VALUE abap_false.
    IF reader->node_type = if_sxml_node=>co_nt_element_close.
      exit = abap_true.
    ENDIF.
    WHILE exit = abap_false.
      reader->next_node( ).
      IF reader->node_type = if_sxml_node=>co_nt_element_close AND reader->name = type_start.
        exit = abap_true.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.


  METHOD set_expected.
    GET REFERENCE OF expected_variable INTO expected_var.
  ENDMETHOD.
ENDCLASS.

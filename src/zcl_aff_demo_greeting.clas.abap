CLASS zcl_aff_demo_greeting DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    "! Returns a greeting for the given name
    "! @parameter name | The person to greet
    "! @parameter result | The greeting string
    METHODS greet
      IMPORTING name          TYPE string
      RETURNING VALUE(result) TYPE string.
ENDCLASS.

CLASS zcl_aff_demo_greeting IMPLEMENTATION.
  METHOD greet.
    result = |Hello, { name }!|.
  ENDMETHOD.
ENDCLASS.

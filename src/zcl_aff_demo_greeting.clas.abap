CLASS zcl_aff_demo_greeting DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    "! Returns a greeting for the given name
    "! @parameter name | The person to greet
    "! @parameter result | The greeting string
    METHODS greet
      IMPORTING name          TYPE string
      RETURNING VALUE(result) TYPE string.
protected section.
private section.
ENDCLASS.



CLASS ZCL_AFF_DEMO_GREETING IMPLEMENTATION.


  METHOD greet.
    result = |Hello, { name }, good morning|.
  ENDMETHOD.
ENDCLASS.

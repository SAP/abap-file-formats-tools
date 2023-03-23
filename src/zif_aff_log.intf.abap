"! Log for the Schema/Simple Transformation writers.
INTERFACE zif_aff_log
  PUBLIC.

  CONSTANTS:
    BEGIN OF c_message_type,
      error   TYPE symsgty VALUE 'E',
      warning TYPE symsgty VALUE 'W',
      info    TYPE symsgty VALUE 'I',
    END OF c_message_type.
  CONSTANTS:
    co_msg106 TYPE string VALUE `Callback class is invalid` ##NO_TEXT,
    co_msg113 TYPE string VALUE `Title is at wrong position` ##NO_TEXT,
    co_msg114 TYPE string VALUE `Default argument is invalid` ##NO_TEXT,
    co_msg115 TYPE string VALUE `Description is at wrong position` ##NO_TEXT,
    co_msg116 TYPE string VALUE `Text between annotations will not be parsed` ##NO_TEXT,
    co_msg123 TYPE string VALUE `No structure was provided for type generator` ##NO_TEXT,
    co_msg124 TYPE string VALUE `Given structure must have mandatory fields format_version and header` ##NO_TEXT,
    co_msg126 TYPE string VALUE `For required fields, a default handling is not possible` ##NO_TEXT,
    co_msg127 TYPE string VALUE `Elements of type enum should be required or have a default` ##NO_TEXT,
    co_msg128 TYPE string VALUE `Components on top level (except format_version) should be structures or tables` ##NO_TEXT,
    co_msg129 TYPE string VALUE `Fields of a different type than integer should not use the annototation $multipleOf` ##NO_TEXT,
    co_msg132 TYPE string VALUE `Only elements of character-like type can have annotation $contentEncoding or $contentMediaType` ##NO_TEXT,
    co_msg133 TYPE string VALUE `The content encoding is unknown` ##NO_TEXT.

  TYPES:
    "! A single message entry in the log
    BEGIN OF ty_log_out,
      "! The name of the component for which the message was logged
      component_name TYPE string,
      "! The type of the message
      type           TYPE symsgty,
      "! The text of the message
      message_text   TYPE string,
      "! The message
      message        TYPE symsg,
    END OF ty_log_out,
    tt_log_out TYPE STANDARD TABLE OF ty_log_out WITH NON-UNIQUE DEFAULT KEY.

  METHODS:
    "! Adds an info message (type I) to the log.
    "!
    "! @parameter  message_text  | the text of the message
    "! @parameter component_name | the name of the element for which the log entry is created
    add_info
      IMPORTING message_text   TYPE string
                component_name TYPE string,

    "! Adds a warning message (type W) to the log.
    "!
    "! @parameter message_text   | the text of the message
    "! @parameter component_name | the name of the element for which the log entry is created
    add_warning
      IMPORTING message_text   TYPE string
                component_name TYPE string,

    "! Adds an error message (type E) to the log.
    "!
    "! @parameter message_text   | the text of the message
    "! @parameter component_name | the name of the element for which the log entry is created
    add_error
      IMPORTING message_text   TYPE string
                component_name TYPE string,

    "! Returns message for a given msg number
    "! This emulates the behaviour of the object type message classes
    "!
    "! @parameter msgno | the message number
    get_message_text
      IMPORTING
                msgno               TYPE symsgno
                msgv1               TYPE symsgv OPTIONAL
                msgv2               TYPE symsgv OPTIONAL
                msgv3               TYPE symsgv OPTIONAL
                msgv4               TYPE symsgv OPTIONAL
      RETURNING VALUE(message_text) TYPE string,



    "! Adds an exception to the log. Actually not the exception is added
    "! but the message of the exception. The message type can be submitted.
    "!
    "! @parameter exception | the exception containing the message
    "! @parameter message_type | the type of the message
    "! @parameter component_name | the name of the element for which the log entry is created
    add_exception
      IMPORTING exception      TYPE REF TO cx_root
                message_type   TYPE symsgty DEFAULT c_message_type-error
                component_name TYPE string,

    "! Returns the logged messages. The log is NOT cleared afterwards.
    "! The caller has to {@link METH.clear} it in case it should be reused.
    "!
    "! @parameter messages | the logged messages
    get_messages
      RETURNING VALUE(messages) TYPE tt_log_out,

    "! Join the messages of another log with this log. Afterwards this log contains
    "! the messages of the other log.
    "!
    "! @parameter log_to_join | the other log
    join
      IMPORTING log_to_join TYPE REF TO zif_aff_log,

    "! Clears all messages of this log.
    "!
    clear,

    "! Calculates the maximum severity of the logged messages.
    "! This is (in order):
    "! E - Error
    "! W - Warning
    "! I - Information
    "!
    "! @parameter max_severity | the maximum severity of the logged messages
    get_max_severity
      RETURNING VALUE(max_severity) TYPE symsgty,

    "! Returns true if the log contains messages, false otherwise.
    "!
    "! @parameter has_messages | true or false
    has_messages
      RETURNING VALUE(has_messages) TYPE abap_bool.

ENDINTERFACE.

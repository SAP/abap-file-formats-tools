*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES:
  BEGIN OF ty_comment_block,
    tab_comments                 TYPE string_table,
    column_first_comment         TYPE i,
    hook_relevant_tok_type       TYPE stokesx,
    hook_relevant_tok_name       TYPE stokesx,
    hook_relevant_tok_name_add   TYPE stokesx,
    hook_relevant_tok_type_stmnt TYPE stokesx,
    hook_relevant_tok_name_stmnt TYPE stokesx,
  END OF ty_comment_block.
TYPES:
  ty_comment_blocks TYPE STANDARD TABLE OF ty_comment_block WITH EMPTY KEY.

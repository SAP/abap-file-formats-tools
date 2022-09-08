CLASS lcl_section_source_comments DEFINITION
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_stokesx TYPE STANDARD TABLE OF stokesx .
    TYPES:
      ty_sstmnt TYPE TABLE OF sstmnt .

    METHODS scan_code
      IMPORTING
        !source_to_be_scanned TYPE string_table
      EXPORTING
        !tab_statements       TYPE ty_sstmnt
        !tab_tokens           TYPE ty_stokesx .
    METHODS identify_abap_doc_blocks_all
      IMPORTING
        !tab_statements     TYPE ty_sstmnt
        !tab_tokens         TYPE ty_stokesx
        !tab_source         TYPE string_table
      RETURNING
        VALUE(tab_abap_doc) TYPE ty_comment_blocks .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA clsname TYPE string.

    TYPES: BEGIN OF ty_node,
             depth          TYPE i,
             node_name      TYPE string,
             parent_node    TYPE string,
             stmnt_from_idx TYPE i,
             stmnt_to_idx   TYPE i,
           END OF ty_node.
    TYPES ty_nodes TYPE STANDARD TABLE OF ty_node.

    DATA depth TYPE i.
    DATA hierarchy_nodes TYPE ty_nodes.

    METHODS is_within_data_begin_end_of
      IMPORTING
        tab_statements TYPE ty_sstmnt
        tab_tokens     TYPE ty_stokesx
        limit          TYPE i
        limit_col      TYPE int2 OPTIONAL
      RETURNING
        VALUE(result)  TYPE abap_bool .
    METHODS is_within_types_begin_end_of
      IMPORTING
        tab_statements TYPE ty_sstmnt
        tab_tokens     TYPE ty_stokesx
        limit          TYPE i
        limit_col      TYPE int2 OPTIONAL
      RETURNING
        VALUE(result)  TYPE abap_bool .

    METHODS build_hierarchy_nodes
      IMPORTING
        tab_statements TYPE ty_sstmnt
        tab_tokens     TYPE ty_stokesx
      CHANGING
        nodes          TYPE ty_nodes.
ENDCLASS.



CLASS lcl_section_source_comments IMPLEMENTATION.



  METHOD is_within_data_begin_end_of.
    FIELD-SYMBOLS: <fs_tok_prev_prev>  TYPE stokesx.
    FIELD-SYMBOLS: <fs_tok_prev_prev2> TYPE stokesx.
    FIELD-SYMBOLS: <fs_tok_prev_prev1> TYPE stokesx.
    FIELD-SYMBOLS: <fs_stmnt_tmp>      TYPE sstmnt.

    DATA counter_begin_of TYPE i.
    DATA counter_end_of TYPE i.

    result = abap_false.
    CLEAR me->depth.

    IF limit_col > 0.
      LOOP AT tab_statements ASSIGNING <fs_stmnt_tmp> WHERE ( type <> 'P' AND type <> 'S' AND type <> 'G' ) AND from <= limit.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev>   INDEX <fs_stmnt_tmp>-from.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev1>  INDEX <fs_stmnt_tmp>-from + 1.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev2>  INDEX <fs_stmnt_tmp>-from + 2.
        IF ( ( <fs_tok_prev_prev>-str = 'DATA' OR <fs_tok_prev_prev>-str = 'CLASS-DATA' OR
                <fs_tok_prev_prev>-str = 'CONSTANTS' ) AND <fs_tok_prev_prev1>-str = 'BEGIN' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_begin_of = counter_begin_of + 1.
          me->depth = me->depth + 1.

        ELSEIF ( ( <fs_tok_prev_prev>-str = 'DATA' OR <fs_tok_prev_prev>-str = 'CLASS-DATA' OR
                   <fs_tok_prev_prev>-str = 'CONSTANTS' ) AND <fs_tok_prev_prev1>-str = 'END' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_end_of = counter_end_of + 1.
          me->depth = me->depth - 1.
        ENDIF.

      ENDLOOP.
    ELSE.
      LOOP AT tab_statements ASSIGNING <fs_stmnt_tmp> WHERE ( type <> 'P' AND type <> 'S' AND type <> 'G' ) AND from < limit.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev>   INDEX <fs_stmnt_tmp>-from.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev1>  INDEX <fs_stmnt_tmp>-from + 1.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev2>  INDEX <fs_stmnt_tmp>-from + 2.
        IF ( ( <fs_tok_prev_prev>-str = 'DATA' OR <fs_tok_prev_prev>-str = 'CLASS-DATA' OR
                <fs_tok_prev_prev>-str = 'CONSTANTS' ) AND <fs_tok_prev_prev1>-str = 'BEGIN' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_begin_of = counter_begin_of + 1.
          me->depth = me->depth + 1.

        ELSEIF ( ( <fs_tok_prev_prev>-str = 'DATA' OR <fs_tok_prev_prev>-str = 'CLASS-DATA' OR
                   <fs_tok_prev_prev>-str = 'CONSTANTS' ) AND <fs_tok_prev_prev1>-str = 'END' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_end_of = counter_end_of + 1.
          me->depth = me->depth - 1.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF counter_begin_of > counter_end_of.
      result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_within_types_begin_end_of.
    FIELD-SYMBOLS: <fs_tok_prev_prev>  TYPE stokesx.
    FIELD-SYMBOLS: <fs_tok_prev_prev2> TYPE stokesx.
    FIELD-SYMBOLS: <fs_tok_prev_prev1> TYPE stokesx.
    FIELD-SYMBOLS: <fs_stmnt_tmp>      TYPE sstmnt.

    DATA counter_begin_of TYPE i.
    DATA counter_end_of TYPE i.

    result = abap_false.
    CLEAR me->depth.

    IF limit_col > 0.  " end row comment
      LOOP AT tab_statements ASSIGNING <fs_stmnt_tmp> WHERE ( type <> 'P' AND type <> 'S' AND type <> 'G' ) AND from <= limit.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev>   INDEX <fs_stmnt_tmp>-from.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev1>  INDEX <fs_stmnt_tmp>-from + 1.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev2>  INDEX <fs_stmnt_tmp>-from + 2.
        IF ( <fs_tok_prev_prev>-str = 'TYPES' AND <fs_tok_prev_prev1>-str = 'BEGIN' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_begin_of = counter_begin_of + 1.
          me->depth = me->depth + 1.

        ELSEIF ( <fs_tok_prev_prev>-str = 'TYPES' AND <fs_tok_prev_prev1>-str = 'END' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_end_of = counter_end_of + 1.
          me->depth = me->depth - 1.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT tab_statements ASSIGNING <fs_stmnt_tmp> WHERE ( type <> 'P' AND type <> 'S' AND type <> 'G' ) AND from < limit.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev>   INDEX <fs_stmnt_tmp>-from.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev1>  INDEX <fs_stmnt_tmp>-from + 1.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev_prev2>  INDEX <fs_stmnt_tmp>-from + 2.
        IF ( <fs_tok_prev_prev>-str = 'TYPES' AND <fs_tok_prev_prev1>-str = 'BEGIN' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_begin_of = counter_begin_of + 1.
          me->depth = me->depth + 1.

        ELSEIF ( <fs_tok_prev_prev>-str = 'TYPES' AND <fs_tok_prev_prev1>-str = 'END' AND <fs_tok_prev_prev2>-str = 'OF' ).
          counter_end_of = counter_end_of + 1.
          me->depth = me->depth - 1.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF counter_begin_of > counter_end_of.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD scan_code.
    SCAN ABAP-SOURCE source_to_be_scanned TOKENS INTO tab_tokens
                                          STATEMENTS INTO tab_statements
                                          WITH ANALYSIS
                                          WITH COMMENTS
                                          WITH PRAGMAS '*'.
  ENDMETHOD.



  METHOD identify_abap_doc_blocks_all.
    DATA l_node               TYPE ty_node.
    DATA l_node_new           TYPE ty_node.
    DATA l_name_node          TYPE string.
    DATA l_name_concatenated  TYPE string.
    DATA l_parent             TYPE string.
    DATA l_line_code          LIKE LINE OF tab_source.
    DATA line_comment_block   TYPE ty_comment_block.
    DATA tab_comments_to_save TYPE string_table.

    DATA current_statement TYPE i.
    DATA relevant_token1   TYPE stokesx.
    DATA relevant_token2   TYPE stokesx.
    DATA relevant_token3   TYPE stokesx.
    DATA l_depth           TYPE i.
    DATA l_length          TYPE i.
    DATA l_count           TYPE i.
    DATA l_from            TYPE i.
    DATA l_to              TYPE i.
*
    DATA embeded_types              TYPE abap_bool VALUE abap_false.
    DATA embeded_data_const         TYPE abap_bool VALUE abap_false.
    DATA nodes                      TYPE string_table.
    DATA hierarchy_nodes_descending TYPE ty_nodes.

    FIELD-SYMBOLS <fs_stmnt>           TYPE sstmnt.
    FIELD-SYMBOLS <fs_stmnt_prev>      TYPE sstmnt.
    FIELD-SYMBOLS <fs_stmnt_next>      TYPE sstmnt.
    FIELD-SYMBOLS <fs_tok>             TYPE stokesx.
    FIELD-SYMBOLS <fs_tok_prev>        TYPE stokesx.
    FIELD-SYMBOLS <fs_tok_prev_plus_1> TYPE stokesx.

    CLEAR tab_abap_doc.

    me->build_hierarchy_nodes(
      EXPORTING
        tab_statements = tab_statements
        tab_tokens     = tab_tokens
      CHANGING
        nodes          = me->hierarchy_nodes ).

    LOOP AT tab_statements ASSIGNING <fs_stmnt> WHERE type = 'P' OR type = 'S'.
      CLEAR: tab_comments_to_save, relevant_token1, relevant_token2, relevant_token3, line_comment_block, l_name_concatenated.

      current_statement  = sy-tabix.

*     get the comment block in sections
      LOOP AT tab_tokens ASSIGNING <fs_tok> FROM <fs_stmnt>-from TO <fs_stmnt>-to.
        READ TABLE tab_source INTO l_line_code INDEX <fs_tok>-row.
        IF <fs_tok>-col > 0.
          l_length = <fs_tok>-col - 1.
          IF l_length > 0.
            CHECK ( l_line_code(l_length) CO space ).
          ENDIF.
        ENDIF.

        DATA(l_line_code_condensed) = l_line_code.
        CONDENSE l_line_code_condensed.
        IF l_line_code_condensed(2) = '"!'.           " filter only ABAP Doc comments
          APPEND l_line_code TO tab_comments_to_save.
        ENDIF.
      ENDLOOP.

      CHECK tab_comments_to_save IS NOT INITIAL.

      embeded_types      = abap_false.
      embeded_data_const = abap_false.

*     consider data and types typtype 4
      LOOP AT tab_statements ASSIGNING <fs_stmnt_prev> TO current_statement WHERE ( type <> 'P' AND type <> 'S' AND type <> 'G' ).
      ENDLOOP.

      " is_within_.._begin_of sets the required me->depth value
      IF <fs_stmnt_prev> IS ASSIGNED.
        READ TABLE tab_tokens ASSIGNING <fs_tok_prev>  INDEX <fs_stmnt_prev>-from.  " fist token in statement
        IF <fs_tok_prev>-str = 'INCLUDE'.
          READ TABLE tab_tokens ASSIGNING <fs_tok_prev_plus_1> INDEX <fs_stmnt_prev>-from + 1.  " second token in statement
        ENDIF.

        IF ( <fs_tok_prev>-str = 'ENDCLASS' ).
          EXIT.  " get ABAP Docs only in definition part
        ENDIF.

        IF <fs_tok_prev>-str = 'TYPES' OR ( <fs_tok_prev>-str = 'INCLUDE' AND
                                             <fs_tok_prev_plus_1> IS ASSIGNED AND <fs_tok_prev_plus_1>-str = 'TYPE' ).
          IF me->is_within_types_begin_end_of(
           tab_statements = tab_statements
           tab_tokens     = tab_tokens
           limit          = <fs_stmnt>-from )  = abap_true.
            embeded_types = abap_true.
          ENDIF.
        ENDIF.

        IF ( <fs_tok_prev>-str = 'DATA' OR <fs_tok_prev>-str = 'CLASS-DATA' OR <fs_tok_prev>-str = 'CONSTANTS' ).
          IF me->is_within_data_begin_end_of(
             tab_statements = tab_statements
             tab_tokens     = tab_tokens
             limit          = <fs_stmnt>-from )  = abap_true.
            embeded_data_const = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.

*     identify the next statement in section and consider the first 2 tokens as hook
*     where the comment has to be attached in the new section
      LOOP AT tab_statements ASSIGNING <fs_stmnt_next> FROM current_statement + 1  WHERE ( type <> 'P' AND type <> 'S' AND type <> 'G' ).
        EXIT.
      ENDLOOP.

      IF <fs_stmnt_next> IS ASSIGNED.
        LOOP AT tab_tokens ASSIGNING <fs_tok> FROM <fs_stmnt_next>-from WHERE type <> 'C'.
          IF relevant_token2 IS INITIAL AND relevant_token1 IS NOT INITIAL.
            relevant_token2 = <fs_tok>.
            IF relevant_token1-str = 'CLASS'.
              READ TABLE tab_tokens INTO relevant_token3 INDEX sy-tabix + 1.
              IF relevant_token3-str = 'DEFINITION' AND relevant_token2-str <> clsname.
                relevant_token2-str = clsname. " it happens at COPY: you have old clsname there
              ENDIF.
              CLEAR relevant_token3.
            ENDIF.

            IF ( relevant_token1-str = 'TYPES' AND relevant_token2-str = 'BEGIN' ) OR
               ( relevant_token1-str = 'DATA' AND relevant_token2-str = 'BEGIN' ) OR
               ( relevant_token1-str = 'CLASS-DATA' AND relevant_token2-str = 'BEGIN' ) OR
               ( relevant_token1-str = 'CONSTANTS' AND relevant_token2-str = 'BEGIN' ).
*             we need the third token in this special case ...
              READ TABLE tab_tokens INTO relevant_token3 INDEX sy-tabix + 2.
*             Check whether this is a TYPES BEGIN OF MESH my_mesh statement.
              IF (  relevant_token1-str = 'TYPES' AND relevant_token3-str = 'MESH' AND <fs_stmnt_next>-to - <fs_stmnt_next>-from = 4 ).
                READ TABLE tab_tokens INTO relevant_token3 INDEX sy-tabix + 1.
              ELSEIF (  relevant_token1-str = 'TYPES' AND relevant_token3-str = 'ENUM' AND <fs_stmnt_next>-to - <fs_stmnt_next>-from > 5 ).
                READ TABLE tab_tokens INTO relevant_token3 INDEX sy-tabix + 1.
              ENDIF.
            ENDIF.
            EXIT.
          ENDIF.
          IF relevant_token1 IS INITIAL.
            relevant_token1 = <fs_tok>.
          ENDIF.
        ENDLOOP.

        IF relevant_token3-str IS NOT INITIAL.
          l_name_node = relevant_token3-str.
        ELSE.
          l_name_node = relevant_token2-str.
        ENDIF.


        IF relevant_token1-str = 'TYPES' OR relevant_token1-str = 'DATA' OR relevant_token1-str = 'CLASS-DATA' OR relevant_token1-str = 'CONSTANTS'.
          APPEND l_name_node TO nodes.
          CLEAR: l_count, l_from, l_to.

          " get the relevant node-interval hierarchy interval
          LOOP AT me->hierarchy_nodes INTO DATA(l_hier).
            CHECK l_hier-depth = 0 AND l_hier-stmnt_from_idx > 0 AND l_hier-stmnt_from_idx <= <fs_stmnt_next>-from.
            l_from = sy-tabix.
          ENDLOOP.

          IF l_from > 0.
            LOOP AT me->hierarchy_nodes FROM l_from + 1 INTO l_hier WHERE depth = 0.
              l_to = sy-tabix.
              EXIT.
            ENDLOOP.
            IF sy-subrc <> 0 OR l_to = 0.
              l_to = lines( me->hierarchy_nodes ).
            ELSEIF l_to > l_from.
              l_to = l_to - 1.
            ENDIF.
            " operate only within relavent node_interval
            LOOP AT me->hierarchy_nodes FROM l_from TO l_to INTO l_node WHERE node_name = l_name_node AND depth = me->depth.
              l_depth = l_node-depth.
              l_parent = l_node-parent_node.
              WHILE l_depth <> 0.

                CLEAR hierarchy_nodes_descending.
                LOOP AT me->hierarchy_nodes INTO l_node_new FROM l_from TO l_to
                                                            WHERE node_name = l_parent AND depth < l_depth.
                  APPEND l_node_new TO hierarchy_nodes_descending.
                ENDLOOP.
                " eed to insert nodes from bottom up
                SORT hierarchy_nodes_descending BY depth DESCENDING. "#EC CI_SORTLOOP
                LOOP AT hierarchy_nodes_descending INTO l_node_new WHERE node_name = l_parent AND depth < l_depth.
                  INSERT l_node_new-node_name INTO nodes INDEX 1.
                ENDLOOP.
                IF sy-subrc <> 0.
                  EXIT.  " while
                ENDIF.
                l_depth  = l_node_new-depth.
                l_parent = l_node_new-parent_node.
              ENDWHILE.
            ENDLOOP.
          ENDIF.
        ENDIF.

        IF lines( nodes ) <= 1.
          CLEAR l_name_concatenated.
        ELSE.
          LOOP AT nodes INTO l_name_node.
            IF l_name_concatenated IS INITIAL.
              l_name_concatenated = l_name_node.
            ELSE.
              l_name_concatenated = l_name_concatenated && '-' && l_name_node.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      line_comment_block-tab_comments               = tab_comments_to_save.
      line_comment_block-hook_relevant_tok_type     = relevant_token1.
      line_comment_block-hook_relevant_tok_name     = relevant_token2.
      line_comment_block-hook_relevant_tok_name_add = relevant_token3.

      IF l_name_concatenated CS '-' AND ( embeded_types = abap_true OR embeded_data_const = abap_true ).
        IF line_comment_block-hook_relevant_tok_name_add IS NOT INITIAL.
          line_comment_block-hook_relevant_tok_name_add-str = l_name_concatenated.
        ELSEIF line_comment_block-hook_relevant_tok_name IS NOT INITIAL.
          line_comment_block-hook_relevant_tok_name-str = l_name_concatenated.
        ENDIF.
      ENDIF.

      APPEND line_comment_block TO tab_abap_doc.
      CLEAR: nodes, l_name_concatenated.

    ENDLOOP.

  ENDMETHOD.


  METHOD build_hierarchy_nodes.

    DATA l_count_begin TYPE i.
    DATA l_count_end   TYPE i.
    DATA l_node_root   TYPE string.
    DATA l_node        TYPE ty_node.
    DATA l_depth       TYPE i.
    DATA parents       TYPE string_table.

    FIELD-SYMBOLS <fs_stmnt>     TYPE sstmnt.
    FIELD-SYMBOLS <fs_tok>       TYPE stokesx.
    FIELD-SYMBOLS <fs_tok_next2> TYPE stokesx.
    FIELD-SYMBOLS <fs_tok_next1> TYPE stokesx.
    FIELD-SYMBOLS <fs_tok_next3> TYPE stokesx.
    FIELD-SYMBOLS <fs_tok_next4> TYPE stokesx.

    CLEAR nodes.

    LOOP AT tab_statements ASSIGNING <fs_stmnt>  WHERE ( type <> 'P' AND type <> 'S' AND type <> 'G' ). " no comments

      " check first token in statement
      IF NOT ( tab_tokens[ <fs_stmnt>-from ]-str = 'TYPES' OR tab_tokens[ <fs_stmnt>-from ]-str = 'DATA' OR
               tab_tokens[ <fs_stmnt>-from ]-str = 'CLASS-DATA' OR tab_tokens[ <fs_stmnt>-from ]-str = 'CONSTANTS' ).
        CONTINUE.
      ENDIF.

      LOOP AT tab_tokens ASSIGNING <fs_tok> FROM <fs_stmnt>-from TO <fs_stmnt>-to.
        CHECK ( <fs_tok>-str = 'TYPES' OR <fs_tok>-str = 'DATA' OR
                <fs_tok>-str = 'CLASS-DATA' OR <fs_tok>-str = 'CONSTANTS' ).

        READ TABLE tab_tokens ASSIGNING <fs_tok_next1>  INDEX <fs_stmnt>-from  + 1.
        READ TABLE tab_tokens ASSIGNING <fs_tok_next2>  INDEX <fs_stmnt>-from  + 2.
        READ TABLE tab_tokens ASSIGNING <fs_tok_next3>  INDEX <fs_stmnt>-from  + 3.

        IF <fs_tok_next1> IS ASSIGNED AND <fs_tok_next2> IS ASSIGNED AND <fs_tok_next3> IS ASSIGNED.
          IF ( <fs_tok_next1>-str = 'BEGIN' AND <fs_tok_next2>-str = 'OF' ).

            l_node-node_name      = <fs_tok_next3>-str.
            l_node-stmnt_from_idx = <fs_stmnt>-from.
            l_node-stmnt_to_idx   = <fs_stmnt>-to.

            IF <fs_tok>-str = 'TYPES' AND ( l_node-node_name = 'MESH' OR l_node-node_name = 'ENUM' ).
              READ TABLE tab_tokens ASSIGNING <fs_tok_next4>  INDEX <fs_stmnt>-from  + 4.
              l_node-node_name = <fs_tok_next4>-str.
            ENDIF.

            IF l_depth = 0.
              l_node_root         = <fs_tok_next3>-str.
              l_node-depth        = 0.
              APPEND l_node TO nodes.
              l_node-parent_node = l_node_root.
            ELSE.
              l_node-depth = l_depth.
              APPEND l_node TO nodes.
            ENDIF.

            APPEND l_node-node_name TO parents.
            l_node-parent_node = l_node-node_name.  " for the child nodes

            l_depth       = l_depth + 1.
            l_count_begin = l_count_begin + 1.
          ELSEIF ( <fs_tok_next1>-str = 'END' AND <fs_tok_next2>-str = 'OF' ).
            l_depth = l_depth - 1.
            l_count_end = l_count_end + 1.

            IF l_depth = 0.
              CLEAR: l_count_begin, l_count_end, l_node, l_node_root, parents.
              CONTINUE.
            ENDIF.

            DATA(l_lines) = lines( parents ).
            DELETE parents INDEX l_lines.
            l_node-parent_node = parents[ l_depth ].

          ELSE.  " TYPES /DATA/CLASS-DATA...     embeded in BEGIN or stand-alone
            l_node-stmnt_from_idx = <fs_stmnt>-from.
            l_node-stmnt_to_idx   = <fs_stmnt>-to.
            l_node-node_name = <fs_tok_next1>-str.
            l_node-depth     = l_depth.
            IF NOT line_exists( nodes[ table_line = l_node ] ).
              APPEND l_node TO nodes.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

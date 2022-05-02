class lcl_SECTION_SOURCE_COMMENTS definition
  final
  create public .

  public section.

    types:
      TY_STOKESX type standard table of STOKESX .
    types:
      TY_SSTMNT type table of SSTMNT .
    types:
      begin of TY_COMMENT_BLOCK,
        TAB_COMMENTS                 type SEO_SECTION_SOURCE,
        COLUMN_FIRST_COMMENT         type I,
        HOOK_RELEVANT_TOK_TYPE       type STOKESX,
        HOOK_RELEVANT_TOK_NAME       type STOKESX,
        HOOK_RELEVANT_TOK_NAME_ADD   type STOKESX,
        HOOK_RELEVANT_TOK_TYPE_STMNT type STOKESX,
        HOOK_RELEVANT_TOK_NAME_STMNT type STOKESX,
      end of TY_COMMENT_BLOCK .
    types:
      TY_COMMENT_BLOCKS type table of TY_COMMENT_BLOCK .
    types:
      begin of TY_COMMENT_FOR_CMPNAME,
        CMPNAME      type SEOCMPNAME,
        TAB_COMMENTS type SEO_SECTION_SOURCE,
      end of TY_COMMENT_FOR_CMPNAME .
    types:
      TY_COMMENTS_FOR_CMPNAMES type table of TY_COMMENT_FOR_CMPNAME .


    methods SCAN_CODE
      importing
        !SOURCE_TO_BE_SCANNED type SEO_SECTION_SOURCE
      exporting
        !TAB_STATEMENTS       type TY_SSTMNT
        !TAB_TOKENS           type TY_STOKESX .
    methods IDENTIFY_abap_doc_BLOCKS_ALL
      importing
        !TAB_STATEMENTS type TY_SSTMNT
        !TAB_TOKENS     type TY_STOKESX
        !TAB_SOURCE     type SEO_SECTION_SOURCE
      exporting
        !TAB_abap_doc   type TY_COMMENT_BLOCKS .
  protected section.
  private section.
    data: CLIF_NAME type SEOCLSKEY.
    types:
      TY_TAB_SUBCOTEXT type standard table of SEOSUBCOTX with default key .
    types: begin of TY_NODE,
             DEPTH          type I,
             NODE_NAME      type STRING,
             PARENT_NODE    type STRING,
             STMNT_FROM_IDX type I,
             STMNT_TO_IDX   type I,
           end of TY_NODE.
    types TY_NODES type standard table of TY_NODE.

    data:
      TAB_TOKENS_OLD           type table of STOKESX .
    data:
      TAB_STATEMENTS_OLD       type table of SSTMNT .
    data DEPTH type I.
    data HIERARCHY_NODES type TY_NODES.
    class-data CO_REGEX_FOR_TAG_END_PARAGR type STRING value '</p>\s*' ##NO_TEXT.
    class-data CO_REGEX_FOR_SHORTTXT type STRING value `<p[\s\n]+class="shorttext synchronized"[\s\n]+lang="(\w\w)"[\s\n]*>` ##NO_TEXT.

    methods IS_WITHIN_DATA_BEGIN_END_OF
      importing
        value(TAB_STATEMENTS) type CL_OO_SECTION_SOURCE_COMMENTS=>TY_SSTMNT
        value(TAB_TOKENS)     type CL_OO_SECTION_SOURCE_COMMENTS=>TY_STOKESX
        value(LIMIT)          type I
        value(LIMIT_col)      type Int2 optional
      returning
        value(RESULT)         type ABAP_BOOL .
    methods IS_WITHIN_TYPES_BEGIN_END_OF
      importing
        value(TAB_STATEMENTS) type CL_OO_SECTION_SOURCE_COMMENTS=>TY_SSTMNT
        value(TAB_TOKENS)     type CL_OO_SECTION_SOURCE_COMMENTS=>TY_STOKESX
        value(LIMIT)          type I
        value(LIMIT_col)      type Int2 optional
      returning
        value(RESULT)         type ABAP_BOOL .

    methods BUILD_HIERARCHY_NODES
      importing
        value(TAB_STATEMENTS) type CL_OO_SECTION_SOURCE_COMMENTS=>TY_SSTMNT
        value(TAB_TOKENS)     type CL_OO_SECTION_SOURCE_COMMENTS=>TY_STOKESX
      changing
        NODES                 type TY_NODES.
endclass.



class lcl_SECTION_SOURCE_COMMENTS implementation.



  method IS_WITHIN_DATA_BEGIN_END_OF.
    field-symbols: <FS_TOK_PREV_PREV>  type STOKESX.
    field-symbols: <FS_TOK_PREV_PREV2> type STOKESX.
    field-symbols: <FS_TOK_PREV_PREV1> type STOKESX.
    field-symbols: <FS_STMNT_TMP>      type SSTMNT.

    data COUNTER_BEGIN_OF type I.
    data COUNTER_END_OF type I.

    RESULT = ABAP_FALSE.
    clear ME->DEPTH.

    if LIMIT_COL > 0.
      loop at TAB_STATEMENTS assigning <FS_STMNT_TMP> where ( TYPE <> 'P' and TYPE <> 'S' and TYPE <> 'G' ) and FROM <= LIMIT.
        read table TAB_TOKENS assigning <FS_TOK_PREV_PREV>   index <FS_STMNT_TMP>-FROM.
        read table TAB_TOKENS assigning <FS_TOK_PREV_PREV1>  index <FS_STMNT_TMP>-FROM + 1.
        read table TAB_TOKENS assigning <FS_TOK_PREV_PREV2>  index <FS_STMNT_TMP>-FROM + 2.
        if  ( ( <FS_TOK_PREV_PREV>-STR = 'DATA' or <FS_TOK_PREV_PREV>-STR = 'CLASS-DATA' or
                <FS_TOK_PREV_PREV>-STR = 'CONSTANTS' ) and <FS_TOK_PREV_PREV1>-STR = 'BEGIN' and <FS_TOK_PREV_PREV2>-STR =  'OF' ).
          COUNTER_BEGIN_OF = COUNTER_BEGIN_OF + 1.
          ME->DEPTH = ME->DEPTH + 1.

        elseif ( ( <FS_TOK_PREV_PREV>-STR = 'DATA' or <FS_TOK_PREV_PREV>-STR = 'CLASS-DATA' or
                   <FS_TOK_PREV_PREV>-STR = 'CONSTANTS' ) and <FS_TOK_PREV_PREV1>-STR = 'END' and <FS_TOK_PREV_PREV2>-STR =  'OF' ).
          COUNTER_END_OF = COUNTER_END_OF + 1.
          ME->DEPTH = ME->DEPTH - 1.
        endif.

      endloop.
    else.
      loop at TAB_STATEMENTS assigning <FS_STMNT_TMP> where ( TYPE <> 'P' and TYPE <> 'S' and TYPE <> 'G' ) and FROM < LIMIT.
        read table TAB_TOKENS assigning <FS_TOK_PREV_PREV>   index <FS_STMNT_TMP>-FROM.
        read table TAB_TOKENS assigning <FS_TOK_PREV_PREV1>  index <FS_STMNT_TMP>-FROM + 1.
        read table TAB_TOKENS assigning <FS_TOK_PREV_PREV2>  index <FS_STMNT_TMP>-FROM + 2.
        if  ( ( <FS_TOK_PREV_PREV>-STR = 'DATA' or <FS_TOK_PREV_PREV>-STR = 'CLASS-DATA' or
                <FS_TOK_PREV_PREV>-STR = 'CONSTANTS' ) and <FS_TOK_PREV_PREV1>-STR = 'BEGIN' and <FS_TOK_PREV_PREV2>-STR =  'OF' ).
          COUNTER_BEGIN_OF = COUNTER_BEGIN_OF + 1.
          ME->DEPTH = ME->DEPTH + 1.

        elseif ( ( <FS_TOK_PREV_PREV>-STR = 'DATA' or <FS_TOK_PREV_PREV>-STR = 'CLASS-DATA' or
                   <FS_TOK_PREV_PREV>-STR = 'CONSTANTS' ) and <FS_TOK_PREV_PREV1>-STR = 'END' and <FS_TOK_PREV_PREV2>-STR =  'OF' ).
          COUNTER_END_OF = COUNTER_END_OF + 1.
          ME->DEPTH = ME->DEPTH - 1.
        endif.
      endloop.
    endif.

    if  COUNTER_BEGIN_OF > COUNTER_END_OF.
      RESULT = ABAP_TRUE.
    endif.
  endmethod.


  method IS_WITHIN_TYPES_BEGIN_END_OF.
    field-symbols: <FS_TOK_PREV_PREV>  type STOKESX.
    field-symbols: <FS_TOK_PREV_PREV2> type STOKESX.
    field-symbols: <FS_TOK_PREV_PREV1> type STOKESX.
    field-symbols: <FS_STMNT_TMP>      type SSTMNT.

    data COUNTER_BEGIN_OF type I.
    data COUNTER_END_OF type I.

    RESULT = ABAP_FALSE.
    clear ME->DEPTH.

    if LIMIT_COL > 0.  " end row comment
      loop at TAB_STATEMENTS assigning <FS_STMNT_TMP> where ( TYPE <> 'P' and TYPE <> 'S' and TYPE <> 'G' ) and FROM <= LIMIT.
        read table TAB_TOKENS assigning <FS_TOK_PREV_PREV>   index <FS_STMNT_TMP>-FROM.
        read table TAB_TOKENS assigning <FS_TOK_PREV_PREV1>  index <FS_STMNT_TMP>-FROM + 1.
        read table TAB_TOKENS assigning <FS_TOK_PREV_PREV2>  index <FS_STMNT_TMP>-FROM + 2.
        if  ( <FS_TOK_PREV_PREV>-STR = 'TYPES' and <FS_TOK_PREV_PREV1>-STR = 'BEGIN' and <FS_TOK_PREV_PREV2>-STR =  'OF' ).
          COUNTER_BEGIN_OF = COUNTER_BEGIN_OF + 1.
          ME->DEPTH = ME->DEPTH + 1.

        elseif ( <FS_TOK_PREV_PREV>-STR = 'TYPES' and <FS_TOK_PREV_PREV1>-STR = 'END' and <FS_TOK_PREV_PREV2>-STR =  'OF' ).
          COUNTER_END_OF = COUNTER_END_OF + 1.
          ME->DEPTH = ME->DEPTH - 1.
        endif.
      endloop.
    else.
      loop at TAB_STATEMENTS assigning <FS_STMNT_TMP> where ( TYPE <> 'P' and TYPE <> 'S' and TYPE <> 'G' ) and FROM < LIMIT.
        read table TAB_TOKENS assigning <FS_TOK_PREV_PREV>   index <FS_STMNT_TMP>-FROM.
        read table TAB_TOKENS assigning <FS_TOK_PREV_PREV1>  index <FS_STMNT_TMP>-FROM + 1.
        read table TAB_TOKENS assigning <FS_TOK_PREV_PREV2>  index <FS_STMNT_TMP>-FROM + 2.
        if  ( <FS_TOK_PREV_PREV>-STR = 'TYPES' and <FS_TOK_PREV_PREV1>-STR = 'BEGIN' and <FS_TOK_PREV_PREV2>-STR =  'OF' ).
          COUNTER_BEGIN_OF = COUNTER_BEGIN_OF + 1.
          ME->DEPTH = ME->DEPTH + 1.

        elseif ( <FS_TOK_PREV_PREV>-STR = 'TYPES' and <FS_TOK_PREV_PREV1>-STR = 'END' and <FS_TOK_PREV_PREV2>-STR =  'OF' ).
          COUNTER_END_OF = COUNTER_END_OF + 1.
          ME->DEPTH = ME->DEPTH - 1.
        endif.
      endloop.
    endif.

    if  COUNTER_BEGIN_OF > COUNTER_END_OF.
      RESULT = ABAP_TRUE.
    endif.
  endmethod.

  method SCAN_CODE.
    scan abap-source SOURCE_TO_BE_SCANNED tokens    into TAB_TOKENS
                                          statements into TAB_STATEMENTS
                                          with analysis
                                          with comments
                                          with pragmas '*'.
  endmethod.



  method IDENTIFY_ABAP_DOC_BLOCKS_ALL.
    data L_NODE               type TY_NODE.
    data L_NODE_NEW           type TY_NODE.
    data L_NAME_NODE          type STRING.
    data L_NAME_CONCATENATED  type STRING.
    data L_PARENT             type STRING.
    data L_LINE_CODE          type SEO_SECTION_SOURCE_LINE.
    data LINE_COMMENT_BLOCK   type TY_COMMENT_BLOCK.
    data TAB_COMMENTS_TO_SAVE type SEO_SECTION_SOURCE.

    data CURRENT_STATEMENT type I.
    data RELEVANT_TOKEN1   type STOKESX.
    data RELEVANT_TOKEN2   type STOKESX.
    data RELEVANT_TOKEN3   type STOKESX.
    data L_DEPTH           type I.
    data L_LENGTH          type I.
    data L_COUNT           type I.
    data L_FROM            type I.
    data L_TO              type I.
*
    data EMBEDED_TYPES              type ABAP_BOOL value ABAP_FALSE.
    data EMBEDED_DATA_CONST         type ABAP_BOOL value ABAP_FALSE.
    data NODES                      type RSWSOURCET.
    data HIERARCHY_NODES_DESCENDING type TY_NODES.

    field-symbols <FS_STMNT>           type SSTMNT.
    field-symbols <FS_STMNT_PREV>      type SSTMNT.
    field-symbols <FS_STMNT_NEXT>      type SSTMNT.
    field-symbols <FS_TOK>             type STOKESX.
    field-symbols <FS_TOK_PREV>        type STOKESX.
    field-symbols <FS_TOK_PREV_PLUS_1> type STOKESX.

    clear TAB_ABAP_DOC.

    ME->BUILD_HIERARCHY_NODES(
      exporting
        TAB_STATEMENTS = TAB_STATEMENTS
        TAB_TOKENS     = TAB_TOKENS
      changing
        NODES          = ME->HIERARCHY_NODES ).

    loop at TAB_STATEMENTS assigning <FS_STMNT> where TYPE = 'P' or TYPE = 'S'.
      clear: TAB_COMMENTS_TO_SAVE, RELEVANT_TOKEN1, RELEVANT_TOKEN2, RELEVANT_TOKEN3, LINE_COMMENT_BLOCK, L_NAME_CONCATENATED.

      CURRENT_STATEMENT  = SY-TABIX.

*     get the comment block in sections
      loop at TAB_TOKENS assigning <FS_TOK> from <FS_STMNT>-FROM to <FS_STMNT>-TO.
        read table TAB_SOURCE into L_LINE_CODE index <FS_TOK>-ROW.
        if <FS_TOK>-COL > 0.
          L_LENGTH = <FS_TOK>-COL - 1.
          if L_LENGTH > 0.
            check  ( L_LINE_CODE(L_LENGTH) co SPACE ) .
          endif.
        endif.

        data(L_LINE_CODE_CONDENSED) = L_LINE_CODE.
        condense L_LINE_CODE_CONDENSED.
        if L_LINE_CODE_CONDENSED(2) = '"!'.           " filter only ABAP Doc comments
          append L_LINE_CODE to TAB_COMMENTS_TO_SAVE.
        endif.
      endloop.

      check TAB_COMMENTS_TO_SAVE is not initial.

      EMBEDED_TYPES      = ABAP_FALSE.
      EMBEDED_DATA_CONST = ABAP_FALSE.

*     consider data and types typtype 4
      loop at TAB_STATEMENTS assigning <FS_STMNT_PREV> to CURRENT_STATEMENT where ( TYPE <> 'P' and TYPE <> 'S' and TYPE <> 'G' ).
      endloop.

      " is_within_.._begin_of sets the required me->depth value
      if <FS_STMNT_PREV> is assigned.
        read table TAB_TOKENS assigning <FS_TOK_PREV>  index <FS_STMNT_PREV>-FROM.  " fist token in statement
        if <FS_TOK_PREV>-STR = 'INCLUDE'.
          read table TAB_TOKENS assigning <FS_TOK_PREV_PLUS_1> index <FS_STMNT_PREV>-FROM + 1.  " second token in statement
        endif.

        if ( <FS_TOK_PREV>-STR = 'ENDCLASS' ).
          exit.  " get ABAP Docs only in definition part
        endif.

        if  <FS_TOK_PREV>-STR = 'TYPES' or ( <FS_TOK_PREV>-STR = 'INCLUDE' and
                                             <FS_TOK_PREV_PLUS_1> is assigned and <FS_TOK_PREV_PLUS_1>-STR = 'TYPE' ).
          if ME->IS_WITHIN_TYPES_BEGIN_END_OF(
           TAB_STATEMENTS = TAB_STATEMENTS
           TAB_TOKENS     = TAB_TOKENS
           LIMIT          = <FS_STMNT>-FROM )  = ABAP_TRUE.
            EMBEDED_TYPES = ABAP_TRUE.
          endif.
        endif.

        if ( <FS_TOK_PREV>-STR = 'DATA' or  <FS_TOK_PREV>-STR = 'CLASS-DATA' or <FS_TOK_PREV>-STR = 'CONSTANTS' ).
          if ME->IS_WITHIN_DATA_BEGIN_END_OF(
             TAB_STATEMENTS = TAB_STATEMENTS
             TAB_TOKENS     = TAB_TOKENS
             LIMIT          = <FS_STMNT>-FROM )  = ABAP_TRUE.
            EMBEDED_DATA_CONST = ABAP_TRUE.
          endif.
        endif.
      endif.

*     identify the next statement in section and consider the first 2 tokens as hook
*     where the comment has to be attached in the new section
      loop at TAB_STATEMENTS assigning <FS_STMNT_NEXT> from CURRENT_STATEMENT + 1  where ( TYPE <> 'P' and TYPE <> 'S' and TYPE <> 'G' ).
        exit.
      endloop.

      if <FS_STMNT_NEXT> is assigned.
        loop at TAB_TOKENS assigning <FS_TOK> from <FS_STMNT_NEXT>-FROM where TYPE <> 'C'.
          if RELEVANT_TOKEN2 is initial and RELEVANT_TOKEN1 is not initial.
            RELEVANT_TOKEN2 = <FS_TOK>.
            if RELEVANT_TOKEN1-STR = 'CLASS'.
              read table TAB_TOKENS into RELEVANT_TOKEN3 index SY-TABIX + 1.
              if RELEVANT_TOKEN3-STR = 'DEFINITION' and RELEVANT_TOKEN2-STR <> CLIF_NAME-CLSNAME.
                RELEVANT_TOKEN2-STR = CLIF_NAME-CLSNAME. " it happens at COPY: you have old clsname there
              endif.
              clear RELEVANT_TOKEN3.
            endif.

            if ( RELEVANT_TOKEN1-STR = 'TYPES' and RELEVANT_TOKEN2-STR = 'BEGIN' ) or
               ( RELEVANT_TOKEN1-STR = 'DATA' and RELEVANT_TOKEN2-STR = 'BEGIN' ) or
               ( RELEVANT_TOKEN1-STR = 'CLASS-DATA' and RELEVANT_TOKEN2-STR = 'BEGIN' ) or
               ( RELEVANT_TOKEN1-STR = 'CONSTANTS' and RELEVANT_TOKEN2-STR = 'BEGIN' ).
*             we need the third token in this special case ...
              read table TAB_TOKENS into RELEVANT_TOKEN3 index SY-TABIX + 2.
*             Check whether this is a TYPES BEGIN OF MESH my_mesh statement.
              if (  RELEVANT_TOKEN1-STR = 'TYPES' and RELEVANT_TOKEN3-STR = 'MESH' and <FS_STMNT_NEXT>-TO - <FS_STMNT_NEXT>-FROM = 4  ).
                read table TAB_TOKENS into RELEVANT_TOKEN3 index SY-TABIX + 1.
              elseif (  RELEVANT_TOKEN1-STR = 'TYPES' and RELEVANT_TOKEN3-STR = 'ENUM' and <FS_STMNT_NEXT>-TO - <FS_STMNT_NEXT>-FROM > 5 ).
                read table TAB_TOKENS into RELEVANT_TOKEN3 index SY-TABIX + 1.
              endif.
              exit.
            else.
              exit.
            endif.
          endif.
          if RELEVANT_TOKEN1 is initial.
            RELEVANT_TOKEN1 = <FS_TOK>.
          endif.
        endloop.

        if RELEVANT_TOKEN3-STR is not initial.
          L_NAME_NODE = RELEVANT_TOKEN3-STR.
        else.
          L_NAME_NODE = RELEVANT_TOKEN2-STR.
        endif.


        if RELEVANT_TOKEN1-STR = 'TYPES' or RELEVANT_TOKEN1-STR = 'DATA' or RELEVANT_TOKEN1-STR = 'CLASS-DATA' or RELEVANT_TOKEN1-STR = 'CONSTANTS' .
          append L_NAME_NODE to NODES.
          clear: L_COUNT, L_FROM, L_TO.

          " get the relevant node-interval hierarchy interval
          loop at ME->HIERARCHY_NODES into data(L_HIER).
            check L_HIER-DEPTH = 0 and L_HIER-STMNT_FROM_IDX > 0 and L_HIER-STMNT_FROM_IDX <= <FS_STMNT_NEXT>-FROM.
            L_FROM = SY-TABIX.
          endloop.

          if L_FROM > 0.
            loop at ME->HIERARCHY_NODES from L_FROM + 1 into L_HIER where DEPTH = 0 .
              L_TO = SY-TABIX.
              exit.
            endloop.
            if SY-SUBRC <> 0 or L_TO = 0.
              L_TO = LINES( ME->HIERARCHY_NODES ).
            elseif L_TO > L_FROM.
              L_TO = L_TO - 1.
            endif.
            " operate only within relavent node_interval
            loop at  ME->HIERARCHY_NODES from L_FROM to L_TO into L_NODE where NODE_NAME = L_NAME_NODE and DEPTH = ME->DEPTH.
              L_DEPTH = L_NODE-DEPTH.
              L_PARENT = L_NODE-PARENT_NODE.
              while L_DEPTH <> 0.

                clear HIERARCHY_NODES_DESCENDING.
                loop at ME->HIERARCHY_NODES into L_NODE_NEW from L_FROM to L_TO
                                                            where NODE_NAME = L_PARENT and DEPTH < L_DEPTH.
                  append L_NODE_NEW to HIERARCHY_NODES_DESCENDING.
                endloop.
                " eed to insert nodes from bottom up
                sort HIERARCHY_NODES_DESCENDING by DEPTH descending. "#EC CI_SORTLOOP
                loop at HIERARCHY_NODES_DESCENDING into L_NODE_NEW where NODE_NAME = L_PARENT and DEPTH < L_DEPTH.
                  insert L_NODE_NEW-NODE_NAME into NODES index 1.
                endloop.
                if SY-SUBRC <> 0.
                  exit.  " while
                endif.
                L_DEPTH  = L_NODE_NEW-DEPTH.
                L_PARENT = L_NODE_NEW-PARENT_NODE.
              endwhile.
            endloop.
          endif.
        endif.

        if LINES( NODES ) <= 1.
          clear L_NAME_CONCATENATED.
        else.
          loop at NODES into L_NAME_NODE.
            if L_NAME_CONCATENATED is initial.
              L_NAME_CONCATENATED = L_NAME_NODE.
            else.
              L_NAME_CONCATENATED = L_NAME_CONCATENATED && '-' && L_NAME_NODE.
            endif.
          endloop.
        endif.
      endif.

      LINE_COMMENT_BLOCK-TAB_COMMENTS               = TAB_COMMENTS_TO_SAVE .
      LINE_COMMENT_BLOCK-HOOK_RELEVANT_TOK_TYPE     = RELEVANT_TOKEN1.
      LINE_COMMENT_BLOCK-HOOK_RELEVANT_TOK_NAME     = RELEVANT_TOKEN2.
      LINE_COMMENT_BLOCK-HOOK_RELEVANT_TOK_NAME_ADD = RELEVANT_TOKEN3.

      if L_NAME_CONCATENATED cs '-' and ( EMBEDED_TYPES = ABAP_TRUE or EMBEDED_DATA_CONST = ABAP_TRUE ).
        if LINE_COMMENT_BLOCK-HOOK_RELEVANT_TOK_NAME_ADD is not initial.
          LINE_COMMENT_BLOCK-HOOK_RELEVANT_TOK_NAME_ADD-STR = L_NAME_CONCATENATED.
        elseif LINE_COMMENT_BLOCK-HOOK_RELEVANT_TOK_NAME is not initial.
          LINE_COMMENT_BLOCK-HOOK_RELEVANT_TOK_NAME-STR = L_NAME_CONCATENATED.
        endif.
      endif.

      append LINE_COMMENT_BLOCK to TAB_ABAP_DOC.
      clear: NODES, L_NAME_CONCATENATED.

    endloop.

  endmethod.


  method BUILD_HIERARCHY_NODES.

    data L_COUNT_BEGIN type I.
    data L_COUNT_END   type I.
    data L_NODE_ROOT   type STRING.
    data L_NODE        type TY_NODE.
    data L_DEPTH       type I.
    data PARENTS       type RSWSOURCET.

    field-symbols <FS_STMNT>     type SSTMNT.
    field-symbols <FS_TOK>       type STOKESX.
    field-symbols <FS_TOK_NEXT2> type STOKESX.
    field-symbols <FS_TOK_NEXT1> type STOKESX.
    field-symbols <FS_TOK_NEXT3> type STOKESX.
    field-symbols <FS_TOK_NEXT4> type STOKESX.

    clear NODES.

    loop at TAB_STATEMENTS assigning <FS_STMNT>  where ( TYPE <> 'P' and TYPE <> 'S' and TYPE <> 'G' ). " no comments

      " check first token in statement
      if not ( TAB_TOKENS[ <FS_STMNT>-FROM ]-STR = 'TYPES' or TAB_TOKENS[ <FS_STMNT>-FROM ]-STR = 'DATA' or
               TAB_TOKENS[ <FS_STMNT>-FROM ]-STR = 'CLASS-DATA' or TAB_TOKENS[ <FS_STMNT>-FROM ]-STR = 'CONSTANTS' ).
        continue.
      endif.

      loop at TAB_TOKENS assigning <FS_TOK> from <FS_STMNT>-FROM to <FS_STMNT>-TO.
        check ( <FS_TOK>-STR = 'TYPES' or <FS_TOK>-STR = 'DATA' or
                <FS_TOK>-STR = 'CLASS-DATA' or <FS_TOK>-STR = 'CONSTANTS' ).

        read table TAB_TOKENS assigning <FS_TOK_NEXT1>  index  <FS_STMNT>-FROM  + 1.
        read table TAB_TOKENS assigning <FS_TOK_NEXT2>  index  <FS_STMNT>-FROM  + 2.
        read table TAB_TOKENS assigning <FS_TOK_NEXT3>  index  <FS_STMNT>-FROM  + 3.

        if <FS_TOK_NEXT1> is assigned and <FS_TOK_NEXT2> is assigned and <FS_TOK_NEXT3> is assigned.
          if ( <FS_TOK_NEXT1>-STR = 'BEGIN' and <FS_TOK_NEXT2>-STR = 'OF' ) .

            L_NODE-NODE_NAME      = <FS_TOK_NEXT3>-STR.
            L_NODE-STMNT_FROM_IDX = <FS_STMNT>-FROM.
            L_NODE-STMNT_TO_IDX   = <FS_STMNT>-TO.

            if  <FS_TOK>-STR = 'TYPES' and ( L_NODE-NODE_NAME = 'MESH' or L_NODE-NODE_NAME = 'ENUM' ).
              read table TAB_TOKENS assigning <FS_TOK_NEXT4>  index  <FS_STMNT>-FROM  + 4.
              L_NODE-NODE_NAME = <FS_TOK_NEXT4>-STR.
            endif.

            if L_DEPTH = 0.
              L_NODE_ROOT         = <FS_TOK_NEXT3>-STR.
              L_NODE-DEPTH        = 0.
              append L_NODE to NODES.
              L_NODE-PARENT_NODE = L_NODE_ROOT.
            else.
              L_NODE-DEPTH = L_DEPTH.
              append L_NODE to NODES.
            endif.

            append L_NODE-NODE_NAME to PARENTS.
            L_NODE-PARENT_NODE = L_NODE-NODE_NAME.  " for the child nodes

            L_DEPTH       = L_DEPTH + 1.
            L_COUNT_BEGIN = L_COUNT_BEGIN + 1.
          elseif ( <FS_TOK_NEXT1>-STR = 'END' and <FS_TOK_NEXT2>-STR = 'OF' ) .
            L_DEPTH = L_DEPTH - 1.
            L_COUNT_END = L_COUNT_END + 1.

            if L_DEPTH = 0.
              clear: L_COUNT_BEGIN, L_COUNT_END, L_NODE, L_NODE_ROOT, PARENTS.
              continue.
            endif.

            data(L_LINES) = LINES( PARENTS ).
            delete PARENTS index L_LINES.
            L_NODE-PARENT_NODE = PARENTS[ L_DEPTH ].

          else.  " TYPES /DATA/CLASS-DATA...     embeded in BEGIN or stand-alone
            L_NODE-STMNT_FROM_IDX = <FS_STMNT>-FROM.
            L_NODE-STMNT_TO_IDX   = <FS_STMNT>-TO.
            L_NODE-NODE_NAME = <FS_TOK_NEXT1>-STR.
            L_NODE-DEPTH     = L_DEPTH.
            if not LINE_EXISTS( NODES[ TABLE_LINE = L_NODE ] ).
              append L_NODE to NODES.
            endif.
          endif.
        endif.
      endloop.

    endloop.

  endmethod.
endclass.

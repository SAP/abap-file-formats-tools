CLASS lcl_aff_conditions IMPLEMENTATION.
  METHOD constructor.
    me->occurrences = occurrences.
  ENDMETHOD.

  METHOD quote.
    result = '"' && escape( val = text format = cl_abap_format=>e_json_string ) && '"'.
  ENDMETHOD.

  METHOD fail.
    RAISE EXCEPTION NEW zcx_aff_tools( message = |Conditional annotation on { component }: { text }| ).
  ENDMETHOD.

  METHOD normalize_link.
    result = link.
    REPLACE ALL OCCURRENCES OF PCRE '\s' IN result WITH ''.
    IF NOT matches( val = result pcre = '\{@link[^{}]+\}' ).
      fail( 'Expected an ABAP Doc link' ).
    ENDIF.
    result = substring( val = result off = 6 len = strlen( result ) - 7 ).
    REPLACE ALL OCCURRENCES OF 'data:' IN result WITH ''.
    result = to_upper( result ).
    IF NOT matches( val = result pcre = '[A-Z0-9_/]+\.[A-Z0-9_]+(\.[A-Z0-9_]+)+' ).
      fail( 'Invalid component link' ).
    ENDIF.
  ENDMETHOD.

  METHOD descend.
    result = start.
    LOOP AT names INTO DATA(name).
      result = line_index( occurrences[ parent = result name = to_mixed( to_lower( name ) ) items = abap_false ] ).
      IF result = 0.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD resolve.
    DATA(key) = normalize_link( link ).
    SPLIT key AT '.' INTO TABLE DATA(parts).
    DATA(type_key) = parts[ 1 ] && '.' && parts[ 2 ].
    DELETE parts FROM 1 TO 2.
    " A declaration in the nearest enclosing occurrence denotes this row/object.
    DATA(ancestor) = context.
    WHILE ancestor > 0.
      IF occurrences[ ancestor ]-type_key = type_key.
        result = descend( start = ancestor names = parts ).
        IF result = 0.
          fail( |Unknown component in { key }| ).
        ENDIF.
        RETURN.
      ENDIF.
      ancestor = occurrences[ ancestor ]-parent.
    ENDWHILE.
    " Else the declaration must have one unambiguous reachable occurrence.
    LOOP AT occurrences TRANSPORTING NO FIELDS WHERE type_key = type_key.
      DATA(candidate) = descend( start = sy-tabix names = parts ).
      IF candidate > 0.
        IF result > 0.
          fail( |Ambiguous field link { key }| ).
        ENDIF.
        result = candidate.
      ENDIF.
    ENDLOOP.
    IF result = 0.
      fail( |Unknown or unreachable field link { key }| ).
    ENDIF.
  ENDMETHOD.

  METHOD literal.
    DATA(node) = occurrences[ field ].
    IF token(1) = '{'.
      DATA(key) = normalize_link( token ).
      IF node-members IS INITIAL.
        fail( 'Constant links require a declared AFF enum' ).
      ENDIF.
      result = VALUE #( node-members[ link = key ]-json OPTIONAL ).
      IF result IS INITIAL.
        fail( |Constant { key } is not a member of the field enum| ).
      ENDIF.
      RETURN.
    ENDIF.
    IF node-members IS NOT INITIAL.
      fail( 'Enum comparison values must use constant links' ).
    ENDIF.
    " The condition pattern only admits boolean, quoted string and numeric literals.
    DATA(literal_type) = COND string( WHEN token = 'true' OR token = 'false' THEN 'boolean'
                                      WHEN token(1) = `'` THEN 'string'
                                      ELSE 'numeric' ).
    IF literal_type <> node-json_type.
      fail( |{ literal_type } literal used for a { node-json_type } field| ).
    ENDIF.
    DATA(decimal_kinds) = cl_abap_typedescr=>typekind_packed && cl_abap_typedescr=>typekind_float
      && cl_abap_typedescr=>typekind_decfloat16 && cl_abap_typedescr=>typekind_decfloat34.
    IF literal_type = 'string'.
      result = quote( replace( val = substring( val = token off = 1 len = strlen( token ) - 2 ) sub = `''` with = `'` occ = 0 ) ).
    ELSEIF literal_type = 'numeric' AND token CS '.' AND node-kind NA decimal_kinds.
      fail( 'Non-integral literal used for an integer field' ).
    ELSE.
      result = token.
    ENDIF.
  ENDMETHOD.

  METHOD common_scope.
    " The root is an ancestor of every node, so the search always succeeds.
    result = first.
    WHILE result > 0.
      DATA(ancestor) = second.
      WHILE ancestor > 0.
        IF ancestor = result.
          RETURN.
        ENDIF.
        ancestor = occurrences[ ancestor ]-parent.
      ENDWHILE.
      result = occurrences[ result ]-parent.
    ENDWHILE.
  ENDMETHOD.

  METHOD wrap.
    result = json.
    DATA(current) = target.
    WHILE current <> scope.
      DATA(node) = occurrences[ current ].
      IF node-items = abap_true.
        IF guard = abap_true.
          fail( 'Comparisons cannot traverse table rows outside their own row scope' ).
        ENDIF.
        result = '{"items":' && result && '}'.
      ELSE.
        DATA(required) = COND string( WHEN guard = abap_true AND node-doc-required = abap_false
          THEN '"required":[' && quote( node-name ) && '],' ).
        result = '{' && required && '"properties":{' && quote( node-name ) && ':' && result && '}}'.
      ENDIF.
      current = node-parent.
    ENDWHILE.
  ENDMETHOD.

  METHOD compile.
    DATA rules TYPE string_table.
    LOOP AT occurrences INTO DATA(target) WHERE doc-available_when IS NOT INITIAL OR doc-required_when IS NOT INITIAL.
      component = COND #( WHEN target-parent > 0 THEN occurrences[ target-parent ]-type_key && '.' ) && target-name.
      IF target-parent = 0 OR target-items = abap_true.
        fail( 'Conditional annotations must be on structure components' ).
      ENDIF.
      DATA(conditions) = VALUE string_table( ( target-doc-available_when ) ( target-doc-required_when ) ).
      DELETE conditions WHERE table_line IS INITIAL.
      DELETE ADJACENT DUPLICATES FROM conditions.
      LOOP AT conditions INTO DATA(condition).
        APPEND compile_rule( target = target condition = condition ) TO rules.
      ENDLOOP.
    ENDLOOP.
    IF rules IS NOT INITIAL.
      result = '"allOf":[' && concat_lines_of( table = rules sep = ',' ) && ']'.
    ENDIF.
  ENDMETHOD.

  METHOD compile_rule.
    context = target-parent.
    FIND FIRST OCCURRENCE OF PCRE
          `^\s*(\{\s*@link[^{}]+\})\s*(!?=)\s*(\{\s*@link[^{}]+\}|'(?:''|[^'])*'|true|false|-?(?:0|[1-9][0-9]*)(?:\.[0-9]+)?)\s*$`
      IN condition SUBMATCHES DATA(field_link) DATA(operator) DATA(value_token).
    IF sy-subrc <> 0.
      fail( 'Only a single ABAP field link = or != literal or enum constant link is supported' ).
    ENDIF.
    DATA(field) = resolve( field_link ).
    DATA(node) = occurrences[ field ].
    IF node-json_type = 'array' OR node-json_type = 'object'.
      fail( 'Only scalar fields can be compared' ).
    ENDIF.
    DATA(value) = literal( field = field token = value_token ).
    " The rule is built where both fields are reachable and then nested into the root schema.
    DATA(scope) = common_scope( first = target-parent second = node-parent ).

    DATA(presence) = COND string( WHEN node-doc-required = abap_false AND node-default_json <> value
      THEN '"required":[' && quote( node-name ) && '],' ).
    " properties alone also matches an absent field. Allow that only when the
    " field is already required or its AFF default equals the comparison value.
    DATA(test) = wrap( target = node-parent scope = scope guard = abap_true
      json = '{' && presence && '"properties":{' && quote( node-name ) && ':{"const":' && value && '}}}' ).
    DATA(required) = COND string( WHEN condition = target-doc-required_when THEN
      wrap( target = target-parent scope = scope json = '{"required":[' && quote( target-name ) && ']}' ) ).
    DATA(forbidden) = COND string( WHEN condition = target-doc-available_when THEN
      wrap( target = target-parent scope = scope json = '{"properties":{' && quote( target-name ) && ':false}}' ) ).

    " The if schema tests equality, so != swaps then and else.
    DATA(negated) = xsdbool( operator = '!=' ).
    DATA(then_schema) = COND string( WHEN negated = abap_true THEN forbidden ELSE required ).
    DATA(else_schema) = COND string( WHEN negated = abap_true THEN required ELSE forbidden ).
    DATA(rule) = '{"if":' && test
      && COND string( WHEN then_schema IS NOT INITIAL THEN ',"then":' && then_schema )
      && COND string( WHEN else_schema IS NOT INITIAL THEN ',"else":' && else_schema ) && '}'.
    result = wrap( target = scope scope = 1 json = rule ).
  ENDMETHOD.
ENDCLASS.


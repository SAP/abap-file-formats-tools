class lcl_aff_conditions implementation.
  method constructor.
    me->occurrences = occurrences.
  endmethod.

  method quote.
    result = '"' && escape( val = text format = cl_abap_format=>e_json_string ) && '"'.
  endmethod.

  method fail.
    raise exception new zcx_aff_tools( message = |Conditional annotation on { component }: { text }| ).
  endmethod.

  method normalize_link.
    result = link.
    replace all occurrences of pcre '\s' in result with ''.
    if not matches( val = result pcre = '\{@link[^{}]+\}' ).
      fail( 'Expected an ABAP Doc link' ).
    endif.
    result = substring( val = result off = 6 len = strlen( result ) - 7 ).
    replace all occurrences of 'data:' in result with ''.
    result = to_upper( result ).
    if not matches( val = result pcre = '[A-Z0-9_/]+\.[A-Z0-9_]+(\.[A-Z0-9_]+)+' ).
      fail( 'Invalid component link' ).
    endif.
  endmethod.

  method descend.
    result = start.
    loop at names into data(name).
      result = line_index( occurrences[ parent = result name = to_mixed( to_lower( name ) ) items = abap_false ] ).
      if result = 0.
        return.
      endif.
    endloop.
  endmethod.

  method resolve.
    data(key) = normalize_link( link ).
    split key at '.' into table data(parts).
    data(type_key) = parts[ 1 ] && '.' && parts[ 2 ].
    delete parts from 1 to 2.
    " A declaration in the nearest enclosing occurrence denotes this row/object.
    data(ancestor) = context.
    while ancestor > 0.
      if occurrences[ ancestor ]-type_key = type_key.
        result = descend( start = ancestor names = parts ).
        if result = 0.
          fail( |Unknown component in { key }| ).
        endif.
        return.
      endif.
      ancestor = occurrences[ ancestor ]-parent.
    endwhile.
    " Else the declaration must have one unambiguous reachable occurrence.
    loop at occurrences transporting no fields where type_key = type_key.
      data(candidate) = descend( start = sy-tabix names = parts ).
      if candidate > 0.
        if result > 0.
          fail( |Ambiguous field link { key }| ).
        endif.
        result = candidate.
      endif.
    endloop.
    if result = 0.
      fail( |Unknown or unreachable field link { key }| ).
    endif.
  endmethod.

  method literal.
    data(node) = occurrences[ field ].
    if token(1) = '{'.
      data(key) = normalize_link( token ).
      if node-members is initial.
        fail( 'Constant links require a declared AFF enum' ).
      endif.
      result = value #( node-members[ link = key ]-json optional ).
      if result is initial.
        fail( |Constant { key } is not a member of the field enum| ).
      endif.
      return.
    endif.
    if node-members is not initial.
      fail( 'Enum comparison values must use constant links' ).
    endif.
    " The condition pattern only admits boolean, quoted string and numeric literals.
    data(literal_type) = cond string( when token = 'true' or token = 'false' then 'boolean'
                                      when token(1) = `'` then 'string'
                                      else 'numeric' ).
    if literal_type <> node-json_type.
      fail( |{ literal_type } literal used for a { node-json_type } field| ).
    endif.
    data(decimal_kinds) = cl_abap_typedescr=>typekind_packed && cl_abap_typedescr=>typekind_float
      && cl_abap_typedescr=>typekind_decfloat16 && cl_abap_typedescr=>typekind_decfloat34.
    if literal_type = 'string'.
      result = quote( replace( val = substring( val = token off = 1 len = strlen( token ) - 2 ) sub = `''` with = `'` occ = 0 ) ).
    elseif literal_type = 'numeric' and token cs '.' and node-kind na decimal_kinds.
      fail( 'Non-integral literal used for an integer field' ).
    else.
      result = token.
    endif.
  endmethod.

  method common_scope.
    " The root is an ancestor of every node, so the search always succeeds.
    result = first.
    while result > 0.
      data(ancestor) = second.
      while ancestor > 0.
        if ancestor = result.
          return.
        endif.
        ancestor = occurrences[ ancestor ]-parent.
      endwhile.
      result = occurrences[ result ]-parent.
    endwhile.
  endmethod.

  method wrap.
    result = json.
    data(current) = target.
    while current <> scope.
      data(node) = occurrences[ current ].
      if node-items = abap_true.
        if guard = abap_true.
          fail( 'Use exists instead of implicitly traversing a table' ).
        endif.
        result = '{"items":' && result && '}'.
      else.
        data(required) = cond string( when guard = abap_true and node-doc-required = abap_false
          then '"required":[' && quote( node-name ) && '],' ).
        result = '{' && required && '"properties":{' && quote( node-name ) && ':' && result && '}}'.
      endif.
      current = node-parent.
    endwhile.
  endmethod.

  method compile.
    data rules type string_table.
    loop at occurrences into data(target) where doc-available_when is not initial or doc-required_when is not initial.
      component = cond #( when target-parent > 0 then occurrences[ target-parent ]-type_key && '.' ) && target-name.
      if target-parent = 0 or target-items = abap_true.
        fail( 'Conditional annotations must be on structure components' ).
      endif.
      data(conditions) = value string_table( ( target-doc-available_when ) ( target-doc-required_when ) ).
      delete conditions where table_line is initial.
      delete adjacent duplicates from conditions.
      loop at conditions into data(condition).
        append compile_rule( target = target condition = condition ) to rules.
      endloop.
    endloop.
    if rules is not initial.
      result = '"allOf":[' && concat_lines_of( table = rules sep = ',' ) && ']'.
    endif.
  endmethod.

  method compile_rule.
    context = target-parent.
    find first occurrence of pcre
          `^\s*(\{\s*@link[^{}]+\})\s*(!?=)\s*(\{\s*@link[^{}]+\}|'(?:''|[^'])*'|true|false|-?(?:0|[1-9][0-9]*)(?:\.[0-9]+)?)\s*$`
      in condition submatches data(field_link) data(operator) data(value_token).
    if sy-subrc <> 0.
      fail( 'Only a single ABAP field link = or != literal or enum constant link is supported' ).
    endif.
    data(field) = resolve( field_link ).
    data(node) = occurrences[ field ].
    if node-json_type = 'array' or node-json_type = 'object'.
      fail( 'Only scalar fields can be compared' ).
    endif.
    data(value) = literal( field = field token = value_token ).
    " The rule is built where both fields are reachable and then nested into the root schema.
    data(scope) = common_scope( first = target-parent second = node-parent ).

    data(presence) = cond string( when node-doc-required = abap_false and node-default_json <> value
      then '"required":[' && quote( node-name ) && '],' ).
    data(test) = wrap( target = node-parent scope = scope guard = abap_true
      json = '{' && presence && '"properties":{' && quote( node-name ) && ':{"const":' && value && '}}}' ).
    data(required) = cond string( when condition = target-doc-required_when then
      wrap( target = target-parent scope = scope json = '{"required":[' && quote( target-name ) && ']}' ) ).
    data(forbidden) = cond string( when condition = target-doc-available_when then
      wrap( target = target-parent scope = scope json = '{"properties":{' && quote( target-name ) && ':false}}' ) ).

    " The if schema tests equality, so != swaps then and else.
    data(negated) = xsdbool( operator = '!=' ).
    data(then_schema) = cond string( when negated = abap_true then forbidden else required ).
    data(else_schema) = cond string( when negated = abap_true then required else forbidden ).
    data(rule) = '{"if":' && test
      && cond string( when then_schema is not initial then ',"then":' && then_schema )
      && cond string( when else_schema is not initial then ',"else":' && else_schema ) && '}'.
    result = wrap( target = scope scope = 1 json = rule ).
  endmethod.
endclass.


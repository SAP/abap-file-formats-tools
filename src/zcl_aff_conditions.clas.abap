CLASS zcl_aff_conditions DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF enum_member,
        link TYPE string,
        json TYPE string,
        doc TYPE zcl_aff_abap_doc_parser=>abap_doc,
        st_value TYPE string,
      END OF enum_member,
      enum_members TYPE STANDARD TABLE OF enum_member WITH EMPTY KEY,
      BEGIN OF node,
        parent TYPE i,
        name TYPE string,
        type_key TYPE string,
        items TYPE abap_bool,
        kind TYPE abap_typekind,
        json_type TYPE string,
        doc TYPE zcl_aff_abap_doc_parser=>abap_doc,
        default_json TYPE string,
        members TYPE enum_members,
        st_ref TYPE string,
      END OF node,
      nodes TYPE STANDARD TABLE OF node WITH EMPTY KEY.
    TYPES:
      BEGIN OF serialization_check,
        target TYPE i,
        expression TYPE string,
      END OF serialization_check,
      serialization_checks TYPE STANDARD TABLE OF serialization_check WITH EMPTY KEY.
    METHODS constructor IMPORTING occurrences TYPE nodes.
    "! Returns the "allOf" member for the root schema, or nothing if there are no conditions
    METHODS compile RETURNING VALUE(result) TYPE string RAISING zcx_aff_tools.
    METHODS compile_serialization RETURNING VALUE(result) TYPE serialization_checks RAISING zcx_aff_tools.
  PRIVATE SECTION.
    DATA occurrences TYPE nodes.
    DATA context TYPE i.
    DATA component TYPE string.
    METHODS compile_rule
      IMPORTING target        TYPE node
                condition     TYPE string
                target_index  TYPE i OPTIONAL
                member        TYPE enum_member OPTIONAL
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_aff_tools.
    METHODS restrict_enum
      IMPORTING target        TYPE node
                member        TYPE enum_member
      RETURNING VALUE(result) TYPE string.
    METHODS resolve IMPORTING link TYPE string RETURNING VALUE(result) TYPE i RAISING zcx_aff_tools.
    METHODS descend
      IMPORTING start         TYPE i
                names         TYPE string_table
      RETURNING VALUE(result) TYPE i.
    METHODS literal
      IMPORTING field         TYPE i
                token         TYPE string
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_aff_tools.
    METHODS normalize_link
      IMPORTING link          TYPE string
                constant_link TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_aff_tools.
    METHODS common_scope
      IMPORTING first         TYPE i
                second        TYPE i
      RETURNING VALUE(result) TYPE i.
    METHODS wrap
      IMPORTING target        TYPE i
                scope         TYPE i
                json          TYPE string
                guard         TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_aff_tools.
    METHODS quote IMPORTING text TYPE string RETURNING VALUE(result) TYPE string.
    METHODS fail IMPORTING text TYPE string RAISING zcx_aff_tools.
ENDCLASS.

CLASS zcl_aff_conditions IMPLEMENTATION.
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
    result = to_upper( result ).
    IF constant_link = abap_true.
      IF NOT matches( val = result pcre = '[A-Z0-9_/]+\.DATA:[A-Z0-9_]+(\.[A-Z0-9_]+)+' ).
        fail( 'Constant links must use class.data:constant.member' ).
      ENDIF.
      REPLACE FIRST OCCURRENCE OF '.DATA:' IN result WITH '.'.
    ELSEIF NOT matches( val = result pcre = '[A-Z0-9_/]+\.[A-Z0-9_]+(\.[A-Z0-9_]+)+' ).
      fail( 'Field links must use class.type.component without .data:' ).
    ENDIF.
  ENDMETHOD.

  METHOD descend.
    result = start.
    LOOP AT names INTO DATA(name).
      WHILE occurrences[ result ]-json_type = 'array'.
        result = line_index( occurrences[ parent = result items = abap_true ] ).
        IF result = 0.
          RETURN.
        ENDIF.
      ENDWHILE.
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
      DATA(key) = normalize_link( link = token constant_link = abap_true ).
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
    LOOP AT occurrences INTO target.
      DATA(target_index) = sy-tabix.
      LOOP AT target-members INTO DATA(member) WHERE doc-available_when IS NOT INITIAL OR doc-required_when IS NOT INITIAL.
        component = member-link.
        IF member-doc-required_when IS NOT INITIAL.
          fail( '$requiredWhen is not supported on enum constants' ).
        ENDIF.
        IF target-parent = 0.
          fail( 'Conditional enum values require an enclosing structure' ).
        ENDIF.
        APPEND compile_rule( target = target condition = member-doc-available_when
          target_index = target_index member = member ) TO rules.
      ENDLOOP.
    ENDLOOP.
    IF rules IS NOT INITIAL.
      result = '"allOf":[' && concat_lines_of( table = rules sep = ',' ) && ']'.
    ENDIF.
  ENDMETHOD.

  METHOD restrict_enum.
    DATA(values) = VALUE string_table( ).
    LOOP AT target-members INTO DATA(candidate) WHERE json <> member-json.
      APPEND candidate-json TO values.
    ENDLOOP.
    " An empty enum is not a valid JSON Schema; false forbids every value.
    result = COND #( WHEN values IS INITIAL THEN 'false'
      ELSE '{"enum":[' && concat_lines_of( table = values sep = ',' ) && ']}' ).
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
    DATA(test) = wrap( target = node-parent scope = scope guard = abap_true
      json = '{' && presence && '"properties":{' && quote( node-name ) && ':{"const":' && value && '}}}' ).
    DATA(required) = COND string( WHEN member IS INITIAL AND condition = target-doc-required_when THEN
      wrap( target = target-parent scope = scope json = '{"required":[' && quote( target-name ) && ']}' ) ).
    DATA(forbidden) = COND string( WHEN condition = target-doc-available_when THEN
      wrap( target = target-parent scope = scope json = '{"properties":{' && quote( target-name ) && ':false}}' ) ).
    IF member IS NOT INITIAL.
      forbidden = wrap( target = target_index scope = scope json = restrict_enum( target = target member = member ) ).
    ENDIF.
    " The if schema tests equality, so != swaps then and else.
    DATA(negated) = xsdbool( operator = '!=' ).
    DATA(then_schema) = COND string( WHEN negated = abap_true THEN forbidden ELSE required ).
    DATA(else_schema) = COND string( WHEN negated = abap_true THEN required ELSE forbidden ).
    DATA(rule) = '{"if":' && test
      && COND string( WHEN then_schema IS NOT INITIAL THEN ',"then":' && then_schema )
      && COND string( WHEN else_schema IS NOT INITIAL THEN ',"else":' && else_schema ) && '}'.
    result = wrap( target = scope scope = 1 json = rule ).
  ENDMETHOD.

  METHOD compile_serialization.
    LOOP AT occurrences INTO DATA(target) WHERE doc-required_when IS NOT INITIAL.
      DATA(target_index) = sy-tabix.
      context = target-parent.
      component = target-type_key && '.' && target-name.
      IF context = 0 OR target-items = abap_true.
        fail( 'Conditional annotations must be on structure components' ).
      ENDIF.
      FIND FIRST OCCURRENCE OF PCRE
        `^\s*(\{\s*@link[^{}]+\})\s*(!?=)\s*(\{\s*@link[^{}]+\}|'(?:''|[^'])*'|true|false|-?(?:0|[1-9][0-9]*)(?:\.[0-9]+)?)\s*$`
        IN target-doc-required_when SUBMATCHES DATA(field_link) DATA(operator) DATA(token).
      IF sy-subrc <> 0.
        fail( 'Only a single ABAP field link = or != literal or enum constant link is supported' ).
      ENDIF.
      DATA(field) = resolve( field_link ).
      DATA(node) = occurrences[ field ].
      IF node-json_type = 'array' OR node-json_type = 'object'.
        fail( 'Only scalar fields can be compared' ).
      ENDIF.
      literal( field = field token = token ).
      DATA(scope) = common_scope( first = target-parent second = node-parent ).
      wrap( target = node-parent scope = scope guard = abap_true json = '' ).
      DATA(value) = COND string( WHEN token = 'true' THEN `C('X')`
        WHEN token = 'false' THEN `C('')`
        WHEN token(1) = `'` THEN 'C(' && token && ')'
        ELSE token ).
      IF token(1) = '{'.
        DATA(key) = normalize_link( link = token constant_link = abap_true ).
        value = node-members[ link = key ]-st_value.
      ELSEIF node-json_type = 'numeric'.
        value = SWITCH #( node-kind
          WHEN cl_abap_typedescr=>typekind_int8 THEN |INT8({ token })|
          WHEN cl_abap_typedescr=>typekind_packed THEN |P({ token })|
          WHEN cl_abap_typedescr=>typekind_float THEN |F('{ token }')|
          WHEN cl_abap_typedescr=>typekind_decfloat16 THEN |DECFLOAT16('{ token }')|
          WHEN cl_abap_typedescr=>typekind_decfloat34 THEN |DECFLOAT34('{ token }')|
          ELSE |I({ token })| ).
      ENDIF.
      DATA(expression) = |ref('{ node-st_ref }'){ operator }{ value }|.
      APPEND VALUE #( target = target_index expression = expression ) TO result.
    ENDLOOP.
    SORT result BY target expression.
    DELETE ADJACENT DUPLICATES FROM result COMPARING target expression.
  ENDMETHOD.
ENDCLASS.

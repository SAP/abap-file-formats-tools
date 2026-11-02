# IN conditions for AFF annotations

Implemented on `feat/conditions-in`, based on merged `main` (`72df5d0`).
This extends the existing conditional implementation; it does not introduce the
broader proposal's compound predicates, paths, `NOT IN`, or `exists`.

## Implementation plan

1. Share predicate parsing between JSON Schema and serialization generation.
2. Accept a single field-link membership predicate with a nonempty,
   comma-separated list of scalar literals or enum constant links.
3. Compile membership to JSON Schema `enum`, checking whether the list includes
   the declared external default (or implicit boolean false).
4. Compile conditional requiredness to OR-connected equality checks using the
   existing serialization literal conversion.
5. Test real generator output, invalid definitions, defaults, optional ancestors,
   and reused row types; validate generated schemas independently.

## Syntax

AFF annotations use `$availableWhen` / `$requiredWhen`, not `@availableWhen` /
`@requiredWhen`. Field links follow the merged implementation's convention:
`class.type.component`. Enum constant links use `class.data:constant.member`.

```abap
begin of ty_main,
  kind type string,
  "! $availableWhen {@link zif_example.ty_main.kind} in ('static', 'dynamic')
  "! $requiredWhen {@link zif_example.ty_main.kind} in ('static', 'dynamic')
  details type ty_details,
end of ty_main.
```

`IN` is case-insensitive. Strings remain case-sensitive and use doubled quotes
for embedded apostrophes. Commas and parentheses inside quoted strings are data,
not list separators. Booleans and numbers use the same literal syntax and type
checking as equality. Every value for an AFF enum must be a link to a member of
that field's declared constant group; `$enumValue` overrides are honored.

Lists can span consecutive ABAP Doc lines:

```abap
"! $requiredWhen {@link zif_example.ty_main.kind} IN (
"!   {@link zif_example.data:co_kind.static},
"!   {@link zif_example.data:co_kind.dynamic})
details type ty_details,
```

Singleton lists are supported; duplicate external values are emitted only once.
Empty lists, missing commas, trailing commas/text, incompatible literal types,
and invalid enum references fail generation instead of dropping a constraint.

## Generated schema

For the first example, assuming `kind` is optional and has no default:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "properties": {
    "kind": { "type": "string" },
    "details": { "type": "object" }
  },
  "additionalProperties": false,
  "allOf": [
    {
      "if": {
        "required": ["kind"],
        "properties": {
          "kind": { "enum": ["static", "dynamic"] }
        }
      },
      "then": { "required": ["details"] },
      "else": { "properties": { "details": false } }
    }
  ]
}
```

The ordinary `details` definition above is abbreviated. Real generation retains
its properties and constraints.

- With availability alone, omit `then`: a matching field is optional.
- With requiredness alone, omit `else`: a nonmatching field remains optional.
- Identical expression text for both annotations produces one rule.
- An absent nondefaulted string/number does not match.
- If the set contains the discriminator's default, omit its `required` guard.
  An already unconditionally required discriminator also needs no extra guard.
- Missing intermediate objects still need presence guards: defaults do not
  instantiate optional structures.
- The merged implementation stores rules in root `allOf`, wrapping their tests
  and effects in `properties` / `items`. Reused table line types retain independent
  current-row evaluation. This change preserves that layout rather than adopting
  the draft proposal's alternative local-`allOf` layout.

For conditional requiredness, serialization emits a test such as
`(ref('.root.KIND')=C('static')) or (ref('.root.KIND')=C('dynamic'))`.
This extends existing logic that emits required initial/default values; it does
not add automatic removal of unavailable values or new deserialization behavior.

## Verification

```text
npm run lint
npx abap_transpile test/abap_transpile.json
node output/index.mjs
node test/conditions-in.mjs
python test/conditions-in.py
```

The Python test requires `jsonschema`. It checks both generated schemas against
draft 2020-12 and validates 31 documents without default insertion or removal of
forbidden values. ABAP tests assert schema structure and generated serialization
expressions, plus rejection in both compiler paths. Existing test exclusions are
unchanged. These are local transpiler tests, not SAP activation or execution of
the generated Simple Transformation in a live SAP system.

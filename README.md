# Tools for ABAP File Formats
[![REUSE status](https://api.reuse.software/badge/github.com/SAP/abap-file-formats-tools)](https://api.reuse.software/info/github.com/SAP/abap-file-formats-tools)

## Description
ABAP file formats rely heavily on JSON Schema that are generated out of ABAP types.
This repository provides the tooling to generate such JSON Schema.

## Contributing
Comments and suggestions for improvements are most welcome.

More details are found at [Contributing](./CONTRIBUTING.md).

## Licensing
Please see our [LICENSE](LICENSE) for copyright and license information.
Detailed information including third-party components and their licensing/copyright information is available via the [REUSE tool](https://api.reuse.software/info/github.com/SAP/abap-file-formats-tools).

## Download & Installation
Pull the source code with [abapGit](https://github.com/abapGit/abapGit/) into your ABAP system.
The latest abapGit build can be downloaded [here](https://raw.githubusercontent.com/abapGit/build/main/zabapgit_standalone.prog.abap).

The development of the ABAP file formats tools takes place in the `Z` namespace.
Coding policies are stated and verified by [abaplint](https://github.com/marketplace/abaplint), see configuration file [here](abaplint.jsonc), that run on every pull request.

## How to Use
This repository mainly consists of two programs:
* `z_generate_json_schema` generates JSON Schemas (minimal requirement is v7.54) from ABAP types
* `z_generate_repo` generates files and folder hierarchy for uploading and maintaining the [abap-file-formats](https://github.com/SAP/abap-file-formats) repository (requires ABAP file format framework)

To add more information to the JSON Schema than that provided by the ABAP type, ABAP Doc is parsed.
Different possibilities for adding more information via annotations are listed [here](https://github.com/SAP/abap-file-formats/blob/main/docs/json.md#writing-JSON-schema-with-ABAP-types).

Additionally to the ABAP-file-formats-related annotations, one can also use the following:
### Content Media Type
 To specify the content media type of the content of a string field, the annotation
 ```abap
 "! $contentMediaType 'mediaType'
 ```
 followed by the media type surrounded by single quotation marks is used.
 This annotation can only be used for characters like ABAP types, i.e., types that are mapped to JSON type `string`.

### Content Encoding
 The encoding used to store the content of a string field can be specified with the annotation
 ```abap
 "! $contentEncoding 'encoding'
 ```
 followed by the encoding surrounded by single quotation marks.
 Possible values for the encoding are `7bit`, `8bit`, `binary`, `quoted-printable`, `base16`, `base32` and `base64`.
 This annotation can also only be used for characters like ABAP types, i.e., types that are mapped to JSON type `string`.

### Conditional Enum Values
`$availableWhen` can be applied to individual constants in a structure referenced
by `$values`. The annotated enum value is allowed only when the condition holds:

```abap
"! $values {@link zif_aff_example_v1.data:co_canteen}
TYPES ty_canteen TYPE c LENGTH 5.

CONSTANTS:
  BEGIN OF co_canteen,
    wdf01 TYPE ty_canteen VALUE 'WDF01',
    "! $availableWhen {@link zif_aff_example_v1.data:ty_main.general_information.drinks_coffee} = true
    wdf55 TYPE ty_canteen VALUE 'WDF55',
  END OF co_canteen.
```

The generator retains the full base `enum` and adds `if`/`then`/`else` rules in
`allOf`, narrowing the enum when a value is unavailable. Multiple restrictions
apply together. `$enumValue` overrides are respected. If no values remain, the
generator emits `false` rather than an invalid empty `enum`.

Conditions use a single field link followed by `=` or `!=` and a literal or enum
constant link. Field links are resolved in each usage context, including table
rows; ambiguous or unreachable links are rejected. Conditional enum values need
an enclosing structure. `$requiredWhen` is not supported on enum constants.

These restrictions do not make the field required or change its default. Avoid
using a conditionally unavailable value as an unconditional default if consumers
apply defaults to missing fields.

## How to Obtain Support
Feel free to raise issues to ask questions or report bugs.

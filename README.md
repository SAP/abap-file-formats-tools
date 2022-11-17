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

The reports and their purpose
* `z_generate_json_schema` generates the ABAP file formats JSON Schema (minimal requirement is v7.54)
* `z_generate_repo` generates files and folder hierarchy for upload and maintain the [abap-file-formats](https://github.com/SAP/abap-file-formats) repository (requires ABAP file format framework)

The development of the ABAP file formats tools takes place in the `Z` namespace.
Coding policies are stated and verified by [abaplint](https://github.com/marketplace/abaplint), see configuration file [here](abaplint.jsonc), that run on every pull request.

## How to Obtain Support
Feel free to raise issues to ask questions or report bugs.

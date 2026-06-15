# ASN.1 / ACN Language Support (VSCode)

VSCode client for the **asn1scc language server**. It provides IDE features for
ASN.1 (`.asn`, `.asn1`) and ACN (`.acn`) files:

- live diagnostics (syntax & semantic errors),
- auto-completion,
- go-to-definition.

The actual language intelligence runs in the asn1scc language server (a small
.NET program that ships with asn1scc); this extension just registers the
languages and launches that server over stdio.

## Requirements

You need the asn1scc **language server** binary. Two forms are supported:

- **`Server.exe`** — a self-contained build that needs **no .NET installation**
  (recommended for locked-down machines), or
- **`Server.dll`** — run via an installed **.NET 10** runtime.

See the asn1scc docs (`q/details-docs/lsp-setup-guide.md`) for how to obtain/build it.

## Configuration

Set the path to the server in your VSCode settings:

```jsonc
{
    // Point at a self-contained Server.exe ...
    "asn1scc.languageServer.path": "C:\\Users\\you\\asn1-lsp\\Server.exe"
    // ... or at a Server.dll (launched via `dotnet`):
    // "asn1scc.languageServer.path": "C:\\Users\\you\\asn1-lsp\\Server.dll"
}
```

If the setting is empty, the extension looks for a bundled server at
`<extension>/server/Server.dll` (run via `dotnet`).

## Installing

Install the packaged `.vsix` via **Extensions → … → Install from VSIX…**, then
open any `.asn`, `.asn1` or `.acn` file.

## Building from source

```bash
npm install
npm run compile      # tsc -> out/extension.js
npm run package      # produces asn1scc-lsp-<version>.vsix
```

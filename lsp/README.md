# ASN.1 / ACN Language Server

This folder contains the **Language Server Protocol (LSP)** support for asn1scc. It lets
any LSP-capable editor (Qt Creator, VSCode, Neovim, …) provide IDE features for ASN.1
and ACN files:

- **live diagnostics** (syntax & semantic errors as you type),
- **auto-completion**,
- **go-to-definition**.

It works for `.asn`, `.asn1` and `.acn` files. The server speaks LSP over **stdio**
(standard input/output); the editor launches it as a child process. The actual language
intelligence runs inside the asn1scc front-end (the same parser/analysis the compiler
uses), so what the editor reports matches what the compiler sees.

### Folder layout

| Path | What it is |
|------|------------|
| `Server/` | The **C# language server** (the program you build and run). |
| `Client/` | A **VSCode extension** that launches the server and registers the ASN.1/ACN languages. |
| `Qt-creator-options.jpg` | Screenshot of the Qt Creator setup described below. |
| `workdir/` | Small sample `.asn`/`.acn` files for trying the server out. |
| `log/` | (legacy) old log files; the server no longer writes here — see [Logs](#logs). |

> More detailed developer documentation lives in `../q/details-docs/lsp-*.md`.

---

## 1. Prerequisites

- **.NET 10 SDK** — required to build and run the server.
  Download: <https://dotnet.microsoft.com/download/dotnet/10.0>
  (This is a standard open-source build-from-source setup: you install the SDK, clone the
  repository, and build. If installing the SDK is not possible on your machine, please
  follow the official .NET installation docs — that part is outside the scope of asn1scc.)
- **Git** — to clone the repository.
- **An editor with LSP support** — e.g. **Qt Creator** (no extra plugin needed) or
  **VSCode** (needs the small extension in `Client/`).
- **Node.js ≥ 18 and npm** — *only* if you want to build the VSCode extension. Not needed
  for Qt Creator. Download: <https://nodejs.org/>

Works on Windows, Linux and macOS. The examples below assume **Windows** and that you
cloned the repository to **`c:\asn1scc`**. Adjust the paths if you cloned elsewhere or use
another OS.

---

## 2. Build the language server

> **Prefer not to build?** A pre-built **self-contained** server (`Server.exe` for
> Windows, plus a Linux build) is published on the project's **GitHub Releases** page and
> as a build artifact on the **Actions** tab. It bundles its own runtime, so it needs no
> .NET installation — download it, unzip it, and skip to step 3 (use the path to the
> extracted `Server.exe`). The steps below are for building from source instead.

Clone the repository:

```bat
git clone https://github.com/esa/asn1scc.git c:\asn1scc
cd c:\asn1scc
```

asn1scc generates some of its source code at build time with two helper tools (`Antlr`
and `parseStg2`). They are project dependencies and are built automatically in the same
configuration as the server (here, `Release`):

```bat
dotnet build lsp\Server\Server\Server.csproj -c Release
```

> Tip: `dotnet build asn1scc.sln -c Release` also produces the server, but it builds the
> entire compiler, so it takes longer.

The build produces the server here:

```
c:\asn1scc\lsp\Server\Server\bin\Release\net10.0\Server.dll
```

You run it with the .NET runtime, passing no arguments:

```bat
dotnet c:\asn1scc\lsp\Server\Server\bin\Release\net10.0\Server.dll
```

It then waits for an editor to talk to it over stdio. You normally do **not** start it by
hand — the editor launches it for you (see below).

---

## 3. Use it from Qt Creator (no plugin required)

Qt Creator has a built-in generic LSP client, so you only need to point it at the server.

1. Open **Edit → Preferences…** (on some builds **Tools → Options…**).
2. Select **Language Client** in the left sidebar.
3. Click **Add** (choose the generic *StdIO* server entry if prompted).
4. Fill in the **General** tab:

   | Field | Value |
   |-------|-------|
   | **Name** | `asn1 language server` |
   | **MIME types** | click **Set MIME Types…** and enter: `*.asn1;*.asn;*.acn` |
   | **Startup behavior** | `Requires an Open File` |
   | **Initialization options** | *(leave empty)* |
   | **Executable** | `C:\Program Files\dotnet\dotnet.exe` |
   | **Arguments** | `c:\asn1scc\lsp\Server\Server\bin\Release\net10.0\Server.dll` |

5. Make sure the checkbox next to the entry is **ticked**, then click **Apply / OK**.

Now open an `.asn1`, `.asn` or `.acn` file (e.g. `c:\asn1scc\lsp\workdir\2\a.asn`). Qt
Creator starts the server automatically; you get diagnostics, completion (Ctrl+Space) and
go-to-definition.

> The screenshot `Qt-creator-options.jpg` shows this exact dialog (it points at an older
> .NET path — use the `net10.0` path shown above).
>
> If you downloaded the **self-contained** server instead, set **Executable** to the
> extracted `Server.exe` and leave **Arguments** empty.

---

## 4. Use it from VSCode (with the extension in `Client/`)

VSCode cannot launch an arbitrary LSP server from its settings alone — it needs a small
extension. You can either **download a pre-built extension** (no Node.js/npm required) or
build it yourself.

### 4.1 Get the extension (`.vsix`)

**Option A — download the pre-built `.vsix` (recommended; no npm needed).**
A packaged `asn1scc-lsp-<version>.vsix` is produced by CI and published on the project's
**GitHub Releases** page (and as a build artifact on the **Actions** tab). Download it —
that is all you need; a `.vsix` already bundles everything the extension requires.

**Option B — build it from source (needs Node.js ≥ 18 + npm).**

```bat
cd c:\asn1scc\lsp\Client
npm install
npm run compile
npm run package
```

This produces `c:\asn1scc\lsp\Client\asn1scc-lsp-0.1.0.vsix`.

### 4.2 Install it

In VSCode: open the **Extensions** view → click the **…** menu → **Install from VSIX…**
→ select the `asn1scc-lsp-<version>.vsix` file.

### 4.3 Tell the extension where the server is

Open VSCode **Settings (JSON)** and add:

```jsonc
{
    // built from source (run via the installed dotnet runtime):
    "asn1scc.languageServer.path": "c:\\asn1scc\\lsp\\Server\\Server\\bin\\Release\\net10.0\\Server.dll"
    // or the self-contained download (no .NET install needed):
    // "asn1scc.languageServer.path": "c:\\asn1scc\\lsp-server\\Server.exe"
}
```

(If the path ends in `.dll`, the extension runs it via the installed `dotnet` runtime;
any other path, e.g. `Server.exe`, is launched directly.)

Open any `.asn`, `.asn1` or `.acn` file and the features become available.

---

## 5. Verify it works

1. Open `c:\asn1scc\lsp\workdir\2\a.asn` (and keep `a.acn` next to it).
2. Type an obvious syntax error — a red diagnostic should appear.
3. Press **Ctrl+Space** inside a type or field — you should get completions.
4. **Go to definition** on a type name should jump to its declaration.

### Logs

The server writes a log to your temp folder: `%TEMP%\asn1scc-lsp\log.txt` on Windows
(`/tmp/asn1scc-lsp/log.txt` on Linux/macOS). Set the `ASN1SCC_LSP_LOG` environment
variable to log to a different directory.

---

## 6. Troubleshooting

| Symptom | Likely cause / fix |
|---------|--------------------|
| Server won't start | .NET 10 SDK/runtime not installed, or wrong path in the editor config. Run the `dotnet ...Server.dll` command from §2 in a terminal to check it launches. |
| No diagnostics or completion | Wrong server path, or the file extension isn't registered. Check the editor's LSP/Output log. In VSCode confirm the `asn1scc.languageServer.path` setting. |
| Completion seems to ignore ACN settings | The server auto-loads the matching `.asn`/`.acn` file next to the open one — keep both in the same folder. |
| `ChannelClosedException` on shutdown | Harmless teardown noise when the editor closes the connection on exit — not a real error. |

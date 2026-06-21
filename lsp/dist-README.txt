============================================================
 ASN.1 / ACN Language Server  -  editor support package
============================================================

This package gives you IDE features (live error checking, auto-
completion, go-to-definition) for ASN.1 (.asn, .asn1) and ACN
(.acn) files in any editor that supports the Language Server
Protocol (LSP), such as Qt Creator and Visual Studio Code.

Nothing needs to be installed or compiled: the language server
binaries are self-contained (they bundle their own runtime, so
you do NOT need to install .NET), and the VSCode extension is
already packaged (so you do NOT need Node.js / npm).


------------------------------------------------------------
 WHAT IS IN THIS PACKAGE
------------------------------------------------------------
  asn1scc-lsp-<version>.vsix          The VSCode extension.
  asn1scc-lsp-server-win-x64.zip      Language server, Windows x64.
  asn1scc-lsp-server-linux-x64.zip    Language server, Linux x64.
  README.txt                          This file.

Do Step 1 first, then follow the section for your editor.


------------------------------------------------------------
 STEP 1  -  UNPACK THE LANGUAGE SERVER
------------------------------------------------------------
Unzip the server archive for your operating system into a folder
you can write to, for example:

  Windows:  C:\Users\<you>\asn1-lsp\
            -> the server is  C:\Users\<you>\asn1-lsp\Server.exe
  Linux:    ~/asn1-lsp/
            -> the server is  ~/asn1-lsp/Server

Again: you do NOT need to install .NET - the server is
self-contained.


------------------------------------------------------------
 QT CREATOR  (no plugin required)
------------------------------------------------------------
1. Edit -> Preferences...        (or Tools -> Options...)
2. Select "Language Client" in the left sidebar.
3. Click "Add" (choose the generic StdIO server entry if asked).
4. Fill in:
     Name ............. asn1 language server
     MIME types ....... click "Set MIME Types..." and enter:
                        *.asn1;*.asn;*.acn
     Startup behavior . Requires an Open File
     Executable ....... the unpacked Server.exe from Step 1
     Arguments ........ (leave empty)
5. Make sure the entry is ticked, then click Apply / OK.
6. Open a .asn1 / .asn / .acn file - the server starts
   automatically.


------------------------------------------------------------
 VISUAL STUDIO CODE
------------------------------------------------------------
1. Open the Extensions view -> "..." menu ->
   "Install from VSIX..." -> select asn1scc-lsp-<version>.vsix
2. Open Settings (JSON) and add the path to the server from
   Step 1:

     {
       "asn1scc.languageServer.path": "C:\\Users\\<you>\\asn1-lsp\\Server.exe"
     }

   (On Linux, use the path to the "Server" file instead.)
3. Open a .asn1 / .asn / .acn file.


------------------------------------------------------------
 VERIFY IT WORKS
------------------------------------------------------------
Open an ASN.1/ACN file:
  - a syntax error should show a red underline,
  - Ctrl+Space should offer completions,
  - "Go to Definition" should jump to a type's declaration.


------------------------------------------------------------
 TROUBLESHOOTING
------------------------------------------------------------
- No diagnostics/completion: check that the Executable / path
  setting points exactly at the unpacked Server.exe (Server on
  Linux), and that you opened a file with a .asn/.asn1/.acn
  extension.
- Completion seems to ignore ACN settings: keep the matching
  .asn and .acn files in the same folder.
- A "ChannelClosedException" message when the editor closes is
  harmless shutdown noise, not an error.


------------------------------------------------------------
 FULL DOCUMENTATION / SOURCE
------------------------------------------------------------
Full guide (including building everything from source):

  https://github.com/esa/asn1scc/blob/master/lsp/README.md

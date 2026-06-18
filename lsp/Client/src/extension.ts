/* --------------------------------------------------------------------------------------------
 * ASN.1 / ACN Language Support — VSCode client for the asn1scc language server.
 * Launches the (C#/OmniSharp) language server over stdio and wires it to .asn/.asn1/.acn files.
 * ------------------------------------------------------------------------------------------ */
'use strict';

import * as path from 'path';
import { workspace, ExtensionContext, window } from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    Executable
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext) {
    // Where is the language server? Prefer the user setting; otherwise fall back
    // to a server bundled inside the extension at <extension>/server/Server.dll.
    const configured = workspace
        .getConfiguration('asn1scc')
        .get<string>('languageServer.path')
        ?.trim();

    const serverPath = configured && configured.length > 0
        ? configured
        : context.asAbsolutePath(path.join('server', 'Server.dll'));

    // A .dll is run through the installed `dotnet` runtime; anything else
    // (e.g. a self-contained Server.exe / native binary) is launched directly.
    const executable: Executable = serverPath.toLowerCase().endsWith('.dll')
        ? { command: 'dotnet', args: [serverPath] }
        : { command: serverPath, args: [] };

    const serverOptions: ServerOptions = {
        run: executable,
        debug: executable
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'asn1' },
            { scheme: 'file', language: 'acn' }
        ],
        synchronize: {
            // Keep the server informed about edits to any ASN.1/ACN file in the workspace.
            fileEvents: workspace.createFileSystemWatcher('**/*.{asn,asn1,acn}')
        }
    };

    client = new LanguageClient(
        'asn1scc',
        'ASN.1/ACN Language Server',
        serverOptions,
        clientOptions
    );

    client.start().catch((err) => {
        window.showErrorMessage(
            `ASN.1/ACN language server failed to start (command: ${executable.command}). ` +
            `Check the "asn1scc.languageServer.path" setting. Details: ${err}`
        );
    });
}

export function deactivate(): Thenable<void> | undefined {
    return client?.stop();
}

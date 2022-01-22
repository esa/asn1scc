/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
// tslint:disable
'use strict';

import { workspace, Disposable, ExtensionContext } from 'vscode';
import { LanguageClient, LanguageClientOptions, SettingMonitor, ServerOptions, TransportKind, InitializeParams } from 'vscode-languageclient';
import { Trace } from 'vscode-jsonrpc';

export function activate(context: ExtensionContext) {

    // The server is implemented in node
    let serverExe = 'dotnet';
    console.log("Current directory is :" + process.cwd())

    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    let serverOptions: ServerOptions = {
        run: { command: serverExe, args: ['C:/prj/GitHub/asn1scc/lsp/Server/Server/bin/Debug/net6.0/Server.dll'] },
        debug: { command: 'C:/prj/GitHub/asn1scc/lsp/Server/Server/bin/Debug/net6.0/Server.exe', args: [] }
    }

    // Options to control the language client
    let clientOptions: LanguageClientOptions = {
        // Register the server for plain text documents
        
        documentSelector: [
            {pattern: '**/*.acn',},
            {pattern: '**/*.asn1',},
            {pattern: '**/*.asn',}
        ],
        synchronize: {
            // Synchronize the setting section 'languageServerExample' to the server
            configurationSection: 'languageServerExample',
            fileEvents: workspace.createFileSystemWatcher('**/*.acn')
        },
    }

    // Create the language client and start the client.
    const client = new LanguageClient('languageServerExample', 'Language Server Example', serverOptions, clientOptions);
    client.trace = Trace.Verbose;
    let disposable = client.start();

    // Push the disposable to the context's subscriptions so that the
    // client can be deactivated on extension deactivation
    context.subscriptions.push(disposable);
}
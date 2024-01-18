using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using MediatR;
using Microsoft.Extensions.Logging;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Progress;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Server.WorkDone;
using OmniSharp.Extensions.LanguageServer.Protocol.Workspace;

namespace LspServer
{
    public class DocumentVersions
    {
        private readonly ConcurrentDictionary<DocumentUri, int> _documentVersions = new ConcurrentDictionary<DocumentUri, int>();

        public int? GetVersion(DocumentUri documentUri)
        {
            if (_documentVersions.TryGetValue(documentUri, out var version))
            {
                return version;
            }

            return null;
        }

        public void Update(VersionedTextDocumentIdentifier identifier)
        {
            _documentVersions.AddOrUpdate(identifier.Uri, identifier.Version, (uri, i) => identifier.Version);
        }

        public void Update(OptionalVersionedTextDocumentIdentifier identifier)
        {
            _documentVersions.AddOrUpdate(identifier.Uri, identifier.Version ?? 0, (uri, i) => identifier.Version ?? 0);
        }

        public void Reset(TextDocumentIdentifier identifier)
        {
            _documentVersions.AddOrUpdate(identifier.Uri, 0, (uri, i) => 0);
        }

        public void Remove(TextDocumentIdentifier identifier)
        {
            _documentVersions.TryRemove(identifier.Uri, out _);
        }
    }


    internal class MyWorkspaceSymbolsHandler : WorkspaceSymbolsHandlerBase
    {
        private readonly ILogger<MyWorkspaceSymbolsHandler> _logger;
        private readonly Asn1SccService _asn1SccService;
        public MyWorkspaceSymbolsHandler(ILogger<MyWorkspaceSymbolsHandler> logger, Asn1SccService asn1SccService)
        {
            _logger = logger;
            _asn1SccService = asn1SccService;
        }


        public override Task<Container<SymbolInformation>> Handle(WorkspaceSymbolParams request, CancellationToken cancellationToken)
        {
            var symbols = new List<SymbolInformation>();
            _logger.LogInformation("MyWorkspaceSymbolsHandler WorkspaceSymbolParams {0}", request);
            _logger.LogInformation("MyWorkspaceSymbolsHandler WorkspaceSymbolParams {0}", request.Query);


            return Task.FromResult(new Container<SymbolInformation>(symbols));
        }

        protected override WorkspaceSymbolRegistrationOptions CreateRegistrationOptions(WorkspaceSymbolCapability capability, ClientCapabilities clientCapabilities)
        {
            return new WorkspaceSymbolRegistrationOptions { };
        }
    }

    internal class MyDefinitionHandler : DefinitionHandlerBase
    {
        private readonly ILogger<MyDefinitionHandler> _logger;
        private readonly Asn1SccService _asn1SccService;
        private readonly DocumentSelector _documentSelector = new DocumentSelector(
            new DocumentFilter { Pattern = "**/*.asn" },
            new DocumentFilter { Pattern = "**/*.asn1" },
            new DocumentFilter { Pattern = "**/*.acn" }
        );

        public MyDefinitionHandler(ILogger<MyDefinitionHandler> logger, Asn1SccService asn1SccService)
        {
            _logger = logger;
            _asn1SccService = asn1SccService;
        }


        public override async Task<LocationOrLocationLinks> Handle(DefinitionParams request, CancellationToken cancellationToken)
        {
            await Task.Yield();
            return _asn1SccService.getDefinitions(request.TextDocument.Uri, request.Position.Line, request.Position.Character);
        }

        protected override DefinitionRegistrationOptions CreateRegistrationOptions(DefinitionCapability capability, ClientCapabilities clientCapabilities)
        {
            return new DefinitionRegistrationOptions()
            {
                DocumentSelector = _documentSelector
            };
        }
    }

    internal class MyCompletionHandler : CompletionHandlerBase
    {
        private readonly ILogger<MyCompletionHandler> _logger;
        private readonly Asn1SccService _asn1SccService;
        private readonly DocumentSelector _documentSelector = new DocumentSelector(
            new DocumentFilter { Pattern = "**/*.asn" },
            new DocumentFilter { Pattern = "**/*.asn1" },
            new DocumentFilter { Pattern = "**/*.acn" }
        );

        public MyCompletionHandler(ILogger<MyCompletionHandler> logger, Asn1SccService asn1SccService)
        {
            _logger = logger;
            _asn1SccService = asn1SccService;
        }

        public override async Task<CompletionItem> Handle(CompletionItem request, CancellationToken cancellationToken)
        {
            await Task.Yield();
            var locations = new List<LocationOrLocationLink>();

            _logger.LogInformation("gmamais MyCompletionHandler(CompletionItem {0}), toString = {1}", request.Label, request.ToString());

            return request;
        }

        public override async Task<CompletionList> Handle(CompletionParams req, CancellationToken cancellationToken)
        {
            await Task.Yield();
            var db1 = req?.PartialResultToken?.ToString();
            var db2 = req?.ToString();
            var db3 = req?.Context?.ToString();
            var lst = _asn1SccService.getCompletionItems(req.TextDocument.Uri, req.Position.Line, req.Position.Character);
            return new CompletionList(lst, true);
        }

        protected override CompletionRegistrationOptions CreateRegistrationOptions(CompletionCapability capability, ClientCapabilities clientCapabilities)
        {
            return new CompletionRegistrationOptions()
            {
                DocumentSelector = _documentSelector,
                //TriggerCharacters = new[] { " " },
                WorkDoneProgress = false
            };
        }
    }

    internal class MyPublishDiagnosticsHandler : PublishDiagnosticsHandlerBase
    {

        private readonly ILogger<MyPublishDiagnosticsHandler> _logger;
        private readonly Asn1SccService _asn1SccService;
        private readonly DocumentSelector _documentSelector = new DocumentSelector(
            new DocumentFilter { Pattern = "**/*.asn" },
            new DocumentFilter { Pattern = "**/*.asn1" },
            new DocumentFilter { Pattern = "**/*.acn" }
        );

        public MyPublishDiagnosticsHandler(ILogger<MyPublishDiagnosticsHandler> logger, Asn1SccService asn1SccService)
        {
            _logger = logger;
            _asn1SccService = asn1SccService;
        }


        public override async Task<Unit> Handle(PublishDiagnosticsParams request, CancellationToken cancellationToken)
        {
            await Task.Yield();
            _logger.LogInformation("gmamais MyPublishDiagnosticsHandler(PublishDiagnosticsParams {0})", request.ToString());
            return Unit.Value;
        }



    }

    /**
     * Called whenever an ASN.1 or ACN file is opened or modified.
     *
     */

    internal class TextDocumentHandler : TextDocumentSyncHandlerBase
    {
        private readonly ILogger<TextDocumentHandler> _logger;
        private readonly ILanguageServerConfiguration _configuration;
        private readonly ILanguageServerFacade _server;
        private readonly Asn1SccService _asn1SccService;

        private readonly DocumentSelector _documentSelector = new DocumentSelector(
            new DocumentFilter { Pattern = "**/*.asn"},
            new DocumentFilter { Pattern = "**/*.asn1" },

            new DocumentFilter { Pattern = "**/*.acn"}
        );

        public TextDocumentHandler(ILogger<TextDocumentHandler> logger, Asn1SccService asn1SccService, ILanguageServerConfiguration configuration, ILanguageServerFacade server)
        {
            _logger = logger;
            _configuration = configuration;
            _asn1SccService = asn1SccService;
            _server = server;
        }

        public TextDocumentSyncKind Change { get; } = TextDocumentSyncKind.Full;

        public override Task<Unit> Handle(DidChangeTextDocumentParams notification, CancellationToken token)
        {
            string content;

            if (Change == TextDocumentSyncKind.Incremental)
            {
                List<string> lines = new List<string>(_asn1SccService.getDocumentLines(notification.TextDocument.Uri));

                foreach (TextDocumentContentChangeEvent textChange in notification.ContentChanges)
                {
                    if (textChange == null || textChange.Range == null)
                        continue;
                    lines = Server.TextSync.ApplyChange(lines, GetFileChangeDetails(textChange.Range, textChange.Text));
                }
                content = string.Join(System.Environment.NewLine, lines);
            }
            else
            {

                content = "";
                foreach (var c in notification.ContentChanges)
                {
                    content += c.Text;
                }
            }

            _asn1SccService.onDocumentChange(notification.TextDocument.Uri, content);
            List<Diagnostic> dia = _asn1SccService.getDiagnostics(notification.TextDocument.Uri);
            _server.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams()
            {
                Uri = notification.TextDocument.Uri,
                Version = notification.TextDocument.Version,
                Diagnostics = dia
            });
            return Unit.Task;
        }

        public override async Task<Unit> Handle(DidOpenTextDocumentParams notification, CancellationToken token)
        {
            await Task.Yield();
            _asn1SccService.onOpenDocument(notification.TextDocument.Uri, notification.TextDocument.Text);
            List<Diagnostic> dia = _asn1SccService.getDiagnostics(notification.TextDocument.Uri);
            _server.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams()
            {
                Uri = notification.TextDocument.Uri,
                Version = notification.TextDocument.Version,
                Diagnostics = dia
            });
            return Unit.Value;
        }

        public override Task<Unit> Handle(DidCloseTextDocumentParams notification, CancellationToken token)
        {
            if (_configuration.TryGetScopedConfiguration(notification.TextDocument.Uri, out var disposable))
            {
                disposable.Dispose();
            }

            return Unit.Task;
        }

        public override Task<Unit> Handle(DidSaveTextDocumentParams notification, CancellationToken token)
        {
            _asn1SccService.onDocumentSave(notification.TextDocument.Uri, notification.Text);
            List<Diagnostic> dia = _asn1SccService.getDiagnostics(notification.TextDocument.Uri);
            _server.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams()
            {
                Uri = notification.TextDocument.Uri,
                Diagnostics = dia
            });
            return Unit.Task;
        }
        protected override TextDocumentSyncRegistrationOptions CreateRegistrationOptions(SynchronizationCapability capability, ClientCapabilities clientCapabilities) => new TextDocumentSyncRegistrationOptions()
        {
            DocumentSelector = _documentSelector,
            Change = Change,
            //Save = new SaveOptions() { IncludeText = true }
            Save = new BooleanOr<SaveOptions>(true)


        };

        public override TextDocumentAttributes GetTextDocumentAttributes(DocumentUri uri) => new TextDocumentAttributes(uri, "acn");


        private static FileChange GetFileChangeDetails(Range changeRange, string insertString)
        {
            // The protocol's positions are zero-based so add 1 to all offsets

            if (changeRange == null) return new FileChange { InsertString = insertString, IsReload = true };

            return new FileChange
            {
                InsertString = insertString,
                Line = changeRange.Start.Line + 1,
                Offset = changeRange.Start.Character + 1,
                EndLine = changeRange.End.Line + 1,
                EndOffset = changeRange.End.Character + 1,
                IsReload = false
            };
        }


    }




    public sealed class FileChange
    {
        /// <summary>
        /// The string which is to be inserted in the file.
        /// </summary>
        public string InsertString { get; set; }

        /// <summary>
        /// The 1-based line number where the change starts.
        /// </summary>
        public int Line { get; set; }

        /// <summary>
        /// The 1-based column offset where the change starts.
        /// </summary>
        public int Offset { get; set; }

        /// <summary>
        /// The 1-based line number where the change ends.
        /// </summary>
        public int EndLine { get; set; }

        /// <summary>
        /// The 1-based column offset where the change ends.
        /// </summary>
        public int EndOffset { get; set; }

        /// <summary>
        /// Indicates that the InsertString is an overwrite
        /// of the content, and all stale content and metadata
        /// should be discarded.
        /// </summary>
        public bool IsReload { get; set; }
    }





}
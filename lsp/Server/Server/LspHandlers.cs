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



    internal class MyDefinitionHandler : DefinitionHandlerBase
    {
        private readonly ILogger<MyDefinitionHandler> _logger;
        private readonly DocumentSelector _documentSelector = new DocumentSelector(
            new DocumentFilter
            {
                Pattern = "**/*.acn"
            }
        );

        public MyDefinitionHandler(ILogger<MyDefinitionHandler> logger)
        {
            _logger = logger;
        }


        public override async Task<LocationOrLocationLinks> Handle(DefinitionParams request, CancellationToken cancellationToken)
        {
            await Task.Yield();
            var locations = new List<LocationOrLocationLink>();
            _logger.LogInformation("gmamais MyDefinitionHandler({0})", request.Position.ToString());


            locations.Add(
                new LocationOrLocationLink(new Location()
                {
                    Uri = request.TextDocument.Uri,
                    Range = new Range(2, 2, 2, 2)
                })
                );

            return locations;
        }

        protected override DefinitionRegistrationOptions CreateRegistrationOptions(DefinitionCapability capability, ClientCapabilities clientCapabilities)
        {
            return new DefinitionRegistrationOptions()
            {
                DocumentSelector = _documentSelector,
                WorkDoneProgress = false
            };
        }
    }

    internal class MyCompletionHandler : CompletionHandlerBase
    {
        private readonly ILogger<MyCompletionHandler> _logger;
        private readonly DocumentSelector _documentSelector = new DocumentSelector(
            new DocumentFilter
            {
                Pattern = "**/*.acn"
            }
        );

        public MyCompletionHandler(ILogger<MyCompletionHandler> logger)
        {
            _logger = logger;
        }

        public override async Task<CompletionItem> Handle(CompletionItem request, CancellationToken cancellationToken)
        {
            await Task.Yield();
            var locations = new List<LocationOrLocationLink>();
            _logger.LogInformation("gmamais MyCompletionHandler(CompletionItem {0})", request.Label);

            return request;
        }

        public override async Task<CompletionList> Handle(CompletionParams request, CancellationToken cancellationToken)
        {
            _logger.LogInformation("gmamais MyCompletionHandler(CompletionParams {0})", request.Position.ToString());
            await Task.Yield();
            var lst = new List<CompletionItem>();
            var seq = new CompletionItem()
            {
                Label = "SEQUENCE",
                Kind = CompletionItemKind.Variable,
                Detail = "This detail for sequence"

            };
            lst.Add(seq);
            var ch = new CompletionItem()
            {
                Label = "CHOICE",
                Kind = CompletionItemKind.Variable,
                Detail = "This detail for CHOICE"
            };
            lst.Add(ch);

            return new CompletionList(lst, true);
        }

        protected override CompletionRegistrationOptions CreateRegistrationOptions(CompletionCapability capability, ClientCapabilities clientCapabilities)
        {
            return new CompletionRegistrationOptions()
            {
                DocumentSelector = _documentSelector,
                WorkDoneProgress = false
            };
        }
    }

    internal class MyPublishDiagnosticsHandler : PublishDiagnosticsHandlerBase
    {

        private readonly ILogger<MyPublishDiagnosticsHandler> _logger;
        private readonly DocumentSelector _documentSelector = new DocumentSelector(
            new DocumentFilter
            {
                Pattern = "**/*.acn"
            }
        );

        public MyPublishDiagnosticsHandler(ILogger<MyPublishDiagnosticsHandler> logger)
        {

            _logger = logger;
        }


        public override async Task<Unit> Handle(PublishDiagnosticsParams request, CancellationToken cancellationToken)
        {
            await Task.Yield();
            //throw new System.NotImplementedException();
            _logger.LogInformation("gmamais MyPublishDiagnosticsHandler(PublishDiagnosticsParams {0})", request.ToString());



            return Unit.Value;
        }



    }



    internal class TextDocumentHandler : TextDocumentSyncHandlerBase
    {
        private readonly ILogger<TextDocumentHandler> _logger;
        private readonly ILanguageServerConfiguration _configuration;
        private readonly ILanguageServerFacade _server;

        private readonly DocumentSelector _documentSelector = new DocumentSelector(
            new DocumentFilter
            {
                Pattern = "**/*.acn"
            }
        );

        public TextDocumentHandler(ILogger<TextDocumentHandler> logger, Foo foo, ILanguageServerConfiguration configuration, ILanguageServerFacade server)
        {
            _logger = logger;
            _configuration = configuration;
            foo.SayFoo();
            _server = server;
        }

        public TextDocumentSyncKind Change { get; } = TextDocumentSyncKind.Full;

        public override Task<Unit> Handle(DidChangeTextDocumentParams notification, CancellationToken token)
        {
            //_logger.LogCritical("Critical");
            //_logger.LogDebug("Debug");
            //_logger.LogTrace("Trace");
            _logger.LogInformation("gmamais DidChangeTextDocumentParams({0})",
                notification.TextDocument.Uri.ToString());
            /*
            _logger.LogInformation("---- chaneges start ---");
            foreach(var c in notification.ContentChanges)
            {
                _logger.LogInformation("Range is :{0}", c.Range != null ? c.Range.ToString() : "null");
                _logger.LogInformation("Text is :{0}", c.Text);
                _logger.LogInformation("**** end of range ***    ");
            }
            _logger.LogInformation("---- chaneges end ---");
            */
            return Unit.Task;
        }

        public override async Task<Unit> Handle(DidOpenTextDocumentParams notification, CancellationToken token)
        {
            await Task.Yield();
            _logger.LogInformation("DidOpenTextDocumentParams {0}", notification.TextDocument.Uri.ToString());
            _logger.LogInformation("----- Content is -------------");
            _logger.LogInformation(notification.TextDocument.Text);
            _logger.LogInformation("----- End of Content -------------");
            await _configuration.GetScopedConfiguration(notification.TextDocument.Uri, token);

            List<Diagnostic> dia = new List<Diagnostic>();
            dia.Add(new Diagnostic()
            {
                Message = "Aman",
                Range = new Range(0,0,0,4),
                Severity = DiagnosticSeverity.Error,
                // TODO: We need to forward this type though if we add something like Vb Support
                Source = "acn"
            });
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

        public override Task<Unit> Handle(DidSaveTextDocumentParams notification, CancellationToken token) => Unit.Task;

        protected override TextDocumentSyncRegistrationOptions CreateRegistrationOptions(SynchronizationCapability capability, ClientCapabilities clientCapabilities) => new TextDocumentSyncRegistrationOptions()
        {
            DocumentSelector = _documentSelector,
            Change = Change,
            Save = new SaveOptions() { IncludeText = true }
        };

        public override TextDocumentAttributes GetTextDocumentAttributes(DocumentUri uri) => new TextDocumentAttributes(uri, "acn");
    }




}
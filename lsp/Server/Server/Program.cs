using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Antlr.Asn1;
using Antlr.Runtime;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Newtonsoft.Json.Linq;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Server;
using Serilog;

namespace LspServer
{
    internal class Program
    {
        private static void Main(string[] args) => MainAsync(args).Wait();

        private static async Task MainAsync(string[] args)
        {
            // Debugger.Launch();
            // while (!Debugger.IsAttached)
            // {
            //     await Task.Delay(100);
            // }

            Log.Logger = new LoggerConfiguration()
                        .Enrich.FromLogContext()
                        .WriteTo.File("C:\\prj\\GitHub\\asn1scc\\lsp\\log\\log.txt", rollingInterval: RollingInterval.Day)
                        .MinimumLevel.Verbose()
                        .CreateLogger();

            Log.Logger.Information("This only goes file...");

            IObserver<WorkDoneProgressReport> workDone = null!;

            var server = await LanguageServer.From(
                options =>
                    options
                       .WithInput(Console.OpenStandardInput())
                       .WithOutput(Console.OpenStandardOutput())
                       .ConfigureLogging(
                            x => x
                                .AddSerilog(Log.Logger)
                                .AddLanguageProtocolLogging()
                                .SetMinimumLevel(LogLevel.Debug)
                        )
                       .WithHandler<MyWorkspaceSymbolsHandler>()
                       .WithHandler<MyDefinitionHandler>()
                       .WithHandler<MyCompletionHandler>()
                       .WithHandler<TextDocumentHandler>()
                       .WithHandler<MyPublishDiagnosticsHandler>()
                       //.WithHandler<DidChangeWatchedFilesHandler>()
                       //.WithHandler<FoldingRangeHandler>()
                       //.WithHandler<MyDocumentSymbolHandler>()
                       //.WithHandler<SemanticTokensHandler>()

                       .WithServices(x => x.AddLogging(b => b.SetMinimumLevel(LogLevel.Trace)))
                       .WithServices(
                            services => {
                                services.AddSingleton(
                                    provider => {
                                        var loggerFactory = provider.GetService<ILoggerFactory>();
                                        var logger = loggerFactory.CreateLogger<Asn1SccService>();

                                        logger.LogInformation("Configuring");

                                        return new Asn1SccService(logger);
                                    }
                                );
                                services.AddSingleton(
                                    new ConfigurationItem
                                    {
                                        Section = "typescript",
                                    }
                                ).AddSingleton(
                                    new ConfigurationItem
                                    {
                                        Section = "terminal",
                                    }
                                );
                            }
                        )
                       .OnInitialize(
                            async (server, request, token) => {
                                var manager = server.WorkDoneManager.For(
                                    request, new WorkDoneProgressBegin
                                    {
                                        Title = "Server is starting...",
                                        Percentage = 10,
                                    }
                                );
                                workDone = manager;

                                await Task.Delay(2000);

                                manager.OnNext(
                                    new WorkDoneProgressReport
                                    {
                                        Percentage = 20,
                                        Message = "loading in progress"
                                    }
                                );
                            }
                        )
                       .OnInitialized(
                            async (server, request, response, token) => {
                                workDone.OnNext(
                                    new WorkDoneProgressReport
                                    {
                                        Percentage = 40,
                                        Message = "loading almost done",
                                    }
                                );

                                await Task.Delay(2000);

                                workDone.OnNext(
                                    new WorkDoneProgressReport
                                    {
                                        Message = "loading done",
                                        Percentage = 100,
                                    }
                                );
                                workDone.OnCompleted();
                            }
                        )
                       .OnStarted(
                            async (languageServer, token) => {
                                using var manager = await languageServer.WorkDoneManager.Create(new WorkDoneProgressBegin { Title = "Doing some work..." });

                                manager.OnNext(new WorkDoneProgressReport { Message = "doing things..." });
                                await Task.Delay(10000);
                                manager.OnNext(new WorkDoneProgressReport { Message = "doing things... 1234" });
                                await Task.Delay(10000);
                                manager.OnNext(new WorkDoneProgressReport { Message = "doing things... 56789" });

                                var logger = languageServer.Services.GetService<ILogger<Asn1SccService>>();
                                var configuration = await languageServer.Configuration.GetConfiguration(
                                    new ConfigurationItem
                                    {
                                        Section = "typescript",
                                    }, new ConfigurationItem
                                    {
                                        Section = "terminal",
                                    }
                                );

                                var baseConfig = new JObject();
                                foreach (var config in languageServer.Configuration.AsEnumerable())
                                {
                                    baseConfig.Add(config.Key, config.Value);
                                }

                                logger.LogInformation("Base Config: {Config}", baseConfig);

                                var scopedConfig = new JObject();
                                foreach (var config in configuration.AsEnumerable())
                                {
                                    scopedConfig.Add(config.Key, config.Value);
                                }

                                logger.LogInformation("Scoped Config: {Config}", scopedConfig);
                            }
                        )
            );

            await server.WaitForExit;
        }
    }

    internal class Asn1SccService
    {
        private readonly ILogger<Asn1SccService> _logger;
        private FrontEntMain.LspWorkSpace ws;
        //private readonly Dictionary<String, FrontEntMain.ParsedFile> parsedFiles = new Dictionary<String, FrontEntMain.ParsedFile>();

        public static Diagnostic lspError2Diagnostic(FrontEntMain.LspError e)
        {
            return new Diagnostic()
            {
                Message = e.msg,
                Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(e.line0, e.charPosInline, e.line0, e.charPosInline + 10),
                Severity = DiagnosticSeverity.Error,
                // TODO: We need to forward this type though if we add something like Vb Support
                Source = "asn1scc"

            };

        }

        public List<Diagnostic> getDiagnostics(DocumentUri docUri)
        {
            //_logger.LogInformation("***** ws is {0}", ws);
            List<Diagnostic> dia = new List<Diagnostic>();
            var parserErrors = 
                ws.files.Where(z => z.fileName == docUri.ToUri().LocalPath).
                SelectMany(z => z.parseErrors).
                Select(z => lspError2Diagnostic(z));

            var semanticErrors =
                ws.files.Where(z => z.fileName == docUri.ToUri().LocalPath).
                SelectMany(z => z.semanticErrors).
                Select(z => lspError2Diagnostic(z));

            dia.AddRange(parserErrors);
            dia.AddRange(semanticErrors);
            /*
            _logger.LogInformation("*****--------------");
            foreach (var e in parserErrors)
            {
                _logger.LogInformation("Error is line{0} msg is {1}", e.Range.Start.Line, e.Message);
            }
            _logger.LogInformation("*****--------------");
            foreach (var e in semanticErrors)
            {
                _logger.LogInformation("Error is line{0} msg is {1}", e.Range.Start.Line, e.Message);
            }

            _logger.LogInformation("*****--------------***********************************");
            */
            return dia;
        }

        public Asn1SccService(ILogger<Asn1SccService> logger)
        {
            ws = FrontEntMain.lspEmptyWs;
            _logger = logger;
        }

        public void onOpenDocument(DocumentUri docUri, string content)
        {
            ws = FrontEntMain.lspOnFileOpened(ws, docUri.ToUri().LocalPath, content);
        }

        public void onDocumentChange(DocumentUri docUri, string content)
        {
            ws = FrontEntMain.lspOnFileChanged(ws, docUri.ToUri().LocalPath, content);
        }

        public List<CompletionItem> getCompletionItems(DocumentUri docUri, int line0, int charPos)
        {
            return
                FrontEntMain.lspAutoComplete(ws, docUri.ToUri().LocalPath, line0, charPos).
                    Select(s => new CompletionItem()
                    {
                        Label = s,
                        Kind = CompletionItemKind.Keyword
                    }).ToList();
        }

        public List<LocationOrLocationLink> getDefinitions (DocumentUri docUri, int line0, int charPos)
        {
            return
                FrontEntMain.lspGoToDefinition(ws, docUri.ToUri().LocalPath, line0, charPos).
                    Select(z =>
                        new LocationOrLocationLink(new Location()
                        {
                            Uri = DocumentUri.From(new Uri(z.Item1).AbsolutePath)  /*file*/,
                            Range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                                z.Item2.line0, z.Item2.charPos, z.Item2.line0, z.Item2.charPos + z.Item2.name.Length)
                        })
                    ).ToList();

        }




        /*

        public void parseFiles(DocumentUri fileUri, String fileContent)
        {
            var fileName = fileUri.ToUri().LocalPath;
            var dir = Path.GetDirectoryName(fileName);
            if (dir == null)
                return;
            var files = Directory.GetFiles(dir);

            var lspFiles = 
                files.Select(f => 
                    new FrontEntMain.FileLsp(f, 
                        f == fileName ? 
                            Microsoft.FSharp.Core.FSharpOption<String>.Some(fileContent) : 
                            Microsoft.FSharp.Core.FSharpOption<String>.None) );
            
            FrontEntMain.parseFilesLsp(lspFiles);

        }


        public void saveFileResults(String fileUri, FrontEntMain.ParsedFile res)
        {
            _logger.LogInformation("START OF saveFileResults file Uri is {0}", fileUri);
            foreach(var s in res.completionItems)
            {
                _logger.LogInformation(" ===> Completion item is {0}", s);
            }

            if (parsedFiles.ContainsKey(fileUri))
            {
                _logger.LogInformation("dictionary  replace");
                parsedFiles[fileUri] = res;
            }
            else
            {
                _logger.LogInformation("dictionary add");
                parsedFiles.Add(fileUri, res);
            }
            _logger.LogInformation("END  OF saveFileResults file Uri is {0}", fileUri);
        }

        public FrontEntMain.ParsedFile getFileResults (String fileUri)
        {
            _logger.LogInformation("START OF getFileResults file Uri is {0}", fileUri);
            _logger.LogInformation("dictionary contains returns {0}", parsedFiles.ContainsKey(fileUri));

            return
            parsedFiles.ContainsKey(fileUri) ?
                parsedFiles[fileUri] :
                new FrontEntMain.ParsedFile(new asn1Parser.AntlrError[] { }, new string[] { }, new FrontEntMain.TypeAssignmentLSP[] { } , new IToken[] { } );
            //_logger.LogInformation("END OF getFileResults file Uri is {0}", fileUri);
        }

        public (String?, FrontEntMain.TypeAssignmentLSP?) getTasDefinition(String fileUri, int line, int charPos)
        {
            var r = getFileResults(fileUri);

            var token = 
                r.tokens.Where(a => a.Line == line && a.CharPositionInLine <= charPos && charPos <= a.CharPositionInLine + a.Text.Length).FirstOrDefault();
            if (token == null)
                return (null, null);
            foreach (var k in parsedFiles)
            {
                var tas = k.Value.tasList.Where(z => z.name == token.Text).FirstOrDefault();
                if (tas != null)
                {
                    return (k.Key, tas);
                }

            }

            return (null, null);
        }
        */


    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Antlr.Asn1;
using Antlr.Runtime;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Newtonsoft.Json.Linq;
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
                       .WithHandler<MyDefinitionHandler>()
                       .WithHandler<MyCompletionHandler>()
                       .WithHandler<TextDocumentHandler>()
                       .WithHandler<MyPublishDiagnosticsHandler>()
                       //.WithHandler<DidChangeWatchedFilesHandler>()
                       //.WithHandler<FoldingRangeHandler>()
                       //.WithHandler<MyWorkspaceSymbolsHandler>()
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
        private readonly Dictionary<String, FrontEntMain.ParsedFile> parsedFiles = new Dictionary<String, FrontEntMain.ParsedFile>();
        public Asn1SccService(ILogger<Asn1SccService> logger)
        {
            _logger = logger;
        }

        public void SayFoo() => _logger.LogInformation("Fooooo!");

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
            _logger.LogInformation("END OF getFileResults file Uri is {0}", fileUri);
        }

        public (String, FrontEntMain.TypeAssignmentLSP) getTasDefinition(String fileUri, int line, int charPos)
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


    }
}

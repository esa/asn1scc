﻿using Daemon.Utils;
using Service.Implementation;
using System;

namespace Daemon
{
    class Program
    {
        static int Main(string[] args)
        {
            var options = new Options();
            if (!CommandLine.Parser.Default.ParseArguments(args, options))
            {
                return 1;
            }

            Console.WriteLine("Asn1Scc Daemon listening on: " + options.Uri);

            var bindings = new Asn1ServiceBindings(new Asn1Service());

            var server = new HttpServer(options.Uri);

            bindings.BindTo(server);

            server.Serve();
            // TODO nice closing of service?
            return 0;
        }
    }
}

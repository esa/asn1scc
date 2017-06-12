namespace Daemon

open CommandLine

type Options = {

    [<Option('u', "uri", HelpText = "URI on which server should listen.", Default = "http://localhost:9749/")>]
    Uri: string

    [<Option('w', "watchdog", HelpText = "Enables watchog - if service /stayAlive will not be called for given amount of milliseconds, server will stop. Value -1 disables watchod.", Default = -1)>]
    WatchdogMs: int
} with
    member this.IsWatchdogEnabled = this.WatchdogMs > 0

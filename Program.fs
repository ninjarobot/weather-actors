namespace WeatherActors
open Suave                 // always open suave
open Suave.Successful      // for OK-result
open Suave.Web             // for config
open Suave.Filters         // path, pathScan
open Suave.Operators       // >=>
open Suave.Writers
open FSharp.Data
open Akka.Actor
open Akka.FSharp

module Program =

    let homeDir = 
        match System.Environment.OSVersion.Platform with
        | System.PlatformID.Unix
        | System.PlatformID.MacOSX -> 
            System.Environment.GetEnvironmentVariable("HOME")
        | System.PlatformID.Win32NT
        | System.PlatformID.Win32S
        | System.PlatformID.Win32Windows
        | System.PlatformID.WinCE ->
            System.Environment.GetEnvironmentVariable("%HOMEDRIVE%%HOMEPATH%")
        | _ as platformId -> raise(System.NotSupportedException(sprintf "Unsupported platform %O" platformId))

    let weatherApiKey =
        System.IO.Path.Combine([|homeDir; ".openweathermap.key.txt"|])
        |> System.IO.File.ReadAllText |> fun s -> s.Trim()

    let weatherApiBaseUri = "http://api.openweathermap.org/data/2.5/"

    type Lat = Lat of float
    type Lon = Lon of float

    type WeatherReq = 
        | Zip of IActorRef * string
        | Coords of IActorRef * Lat * Lon

    type WeatherRes = 
        | Weather of IActorRef * string

    let handleWeatherReq (mailbox: Actor<'a>) msg =
        let uri = weatherApiBaseUri + "weather"
        match msg with
        | Zip (aref, z) -> 
            let q = ["zip", z; "APPID", weatherApiKey; "units", "imperial"]
            let res = Http.RequestString(uri, query=q)
            mailbox.Sender() <! Weather(mailbox.Self, res)
        | Coords (aref, Lat lat, Lon lon) -> 
            let q = ["lat", lat.ToString(); "lon", lon.ToString(); "APPID", weatherApiKey; "units", "imperial"]
            let res = Http.RequestString(uri, query=q)
            mailbox.Sender() <! Weather(mailbox.Self, res)


    [<EntryPoint>]
    let main argv =
        use system = System.create "my-system" (Configuration.load())

        let currentWeatherHere zip =
            fun (ctx:HttpContext) -> async {
                let aref = select "akka://my-system/user/weather-req" system
                let caller = system.ActorOf(Props.Empty)
                let! (task:WeatherRes) = (aref <? Zip(caller, (sprintf "%s,us" zip)))
                let res = 
                    match task with 
                    | Weather (_, r) -> r
                return! OK (res) ctx
            }

        let app =
            choose
                [ GET >=> pathScan "/current/%s" currentWeatherHere >=> setMimeType "application/json;charset=utf-8" ]

        let aref = spawn system "weather-req" (actorOf2 handleWeatherReq)
        startWebServer defaultConfig app
        0

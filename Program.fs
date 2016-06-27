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

    let baseQuery = ["APPID", weatherApiKey; "units", "imperial"]

    type Lat = Lat of float
    type Lon = Lon of float

    type WeatherReq = 
        | Zip of string
        | City of string
        | Coords of Lat * Lon

    type OpenWeatherMapCurrent = JsonProvider<"""{"coord":
        {"lon":145.77,"lat":-16.92},
        "weather":[{"id":803,"main":"Clouds","description":"broken clouds","icon":"04n"}],
        "base":"cmc stations",
        "main":{"temp":293.25,"pressure":1019,"humidity":83,"temp_min":289.82,"temp_max":295.37},
        "wind":{"speed":5.1,"deg":150},
        "clouds":{"all":75},
        "rain":{"3h":3},
        "dt":1435658272,
        "sys":{"type":1,"id":8166,"message":0.0166,"country":"AU","sunrise":1435610796,"sunset":1435650870},
        "id":2172797,
        "name":"Cairns",
        "cod":200,
        "message":"something"}""">

    type WeatherSummary = {
        Description : string
        Temperature : decimal
        Humidity : int
        City : string
    }

    let handleWeatherReq (mailbox: Actor<'a>) msg =
        printfn "Message handled by %O" mailbox.Context.Self.Path
        let uri = weatherApiBaseUri + "weather"
        let q = 
            match msg with
            | Zip z -> 
                ["zip", z]
            | City c -> 
                ["q", c]
            | Coords (Lat lat, Lon lon) -> 
                ["lat", lat.ToString(); "lon", lon.ToString()]
            |> List.append baseQuery 
        Http.AsyncRequestString(uri, query=q)
        |!> mailbox.Sender()

    let convertWeatherReq (mailbox: Actor<obj>) msg =
        match box msg with
        | :? string as currWeatherRes ->
            let res = sprintf "%s" currWeatherRes
            printfn "Converting %s" res
            let w = res |> OpenWeatherMapCurrent.Parse
            match w.Cod with
            | 200 ->
                match w.Weather with 
                | [|weather|] -> 
                    let d = weather.Description
                    let t = w.Main.Temp
                    let h = w.Main.Humidity
                    let city = w.Name
                    mailbox.Sender() <! (Newtonsoft.Json.JsonConvert.SerializeObject <| { Description=d; Temperature=t; Humidity=h; City=city })
                | _ -> mailbox.Unhandled()
            | 404 ->
                mailbox.Sender() <! w.Message
            | _ ->
                mailbox.Unhandled()
        | _ -> mailbox.Unhandled()

    let system = 
        let s = lazy(System.create "my-system" (Configuration.load()))
        s.Value


    [<EntryPoint>]
    let main argv =

        let currWeatherActor = select "akka://my-system/user/weather-req" system
        let convertWeatherActor = select "akka://my-system/user/convert-weather" system

        let currentWeatherZip zip =
            fun (ctx:HttpContext) -> async {
                let! (res:string) = currWeatherActor <? Zip(sprintf "%s,us" zip)
                let! (converted:string) = convertWeatherActor <? res
                return! OK (converted) ctx
            }

        let currentWeatherCity city =
            fun (ctx:HttpContext) -> async {
                let! (res:string) = currWeatherActor <? City(sprintf "%s,us" city)
                let! (converted:string) = convertWeatherActor <? res
                return! OK (converted) ctx
            }

        let app =
            choose
                [ GET >=>
                    choose
                        [ 
                            pathScan "/current/zip/%s" currentWeatherZip >=> setMimeType "application/json;charset=utf-8"
                            pathScan "/current/city/%s" currentWeatherCity >=> setMimeType "application/json;charset=utf-8"
                        ]
                ]

        let aref1 = spawnOpt system "weather-req" (actorOf2 handleWeatherReq) [SpawnOption.Router(Akka.Routing.RoundRobinPool(5))]
        let aref2 = spawn system "convert-weather" (actorOf2 convertWeatherReq)
        startWebServer defaultConfig app
        0
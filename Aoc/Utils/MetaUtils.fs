module MetaUtils

open System
open System.Net
open System.Net.Http
open System.IO

let todayFilename day = @$"c:\Training\Aoc2024files\day{day}.txt"
let [<Literal>] keyFilename =  @"c:\Training\Aoc2024files\key.txt"
let getTodayInput day =
    
    let filename = todayFilename day

    if not (File.Exists(filename)) then
        task {
            use file = File.OpenWrite(filename)

            //find the Session cookie and paste the value into key.txt - it will look something like 53616c7465645f5f3532a446d565a187... 
            let key = (File.ReadAllLines keyFilename)[0]
            let cookieContainer = CookieContainer()
            cookieContainer.Add (Cookie(Name = "session", Value = key, Domain = ".adventofcode.com"))
            use handler = new HttpClientHandler (CookieContainer = cookieContainer)
            use client = new HttpClient(handler)
            client.BaseAddress <- Uri("https://adventofcode.com/")

            let! response = client.GetStreamAsync($"/2024/day/{day}/input")
            do! response.CopyToAsync(file)
        }
        |> Async.AwaitTask
        |> Async.RunSynchronously
        printfn $"File %s{filename} created"
    else
        printfn $"File %s{filename} already exists"    

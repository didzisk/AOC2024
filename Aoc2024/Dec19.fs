module Dec19

open System
open System.Collections.Generic
open System.IO
open Microsoft.FSharp.Collections
open StringUtils

let day = 19
MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

let Del1 () =
    //inputString |> printfn "%s"

    let patternsTowels = inputString.Split("\n\n")
    let patterns = patternsTowels[0].Split(", ") |> Set.ofArray
    let maxPattern = patterns |> Seq.map String.length |> Seq.max 
    let towels = patternsTowels[1].Split("\n", StringSplitOptions.RemoveEmptyEntries) //|> Array.filter (fun x->String.IsNullOrWhiteSpace x |> not)
    //towels|> Array.iter (printfn "%s")
    let cache = Dictionary<string, int64>()
    
    let rec makePattern (patterns: Set<string>) acc rem =
        if cache.ContainsKey rem then
            cache[rem]
        else
            match rem with
            | "" -> acc + 1L
            | str ->
                let sum =
                    [ 1 .. min maxPattern str.Length ]
                    |> Seq.map (fun len -> str.Substring(0, len))
                    |> Seq.filter (fun p -> Set.contains p patterns)
                    |> Seq.sumBy (fun p -> makePattern patterns acc (rem.Substring p.Length))

                cache.Add(rem, sum) |> ignore
                sum
    let allPaths = towels |> Array.map (makePattern patterns 0L)
    let part1 = allPaths |> Array.filter (fun s -> s > 0L) |> Array.length
    let part2 = allPaths |> Array.sum
    part1 |> printfn "Part 1: %d"
    part2 |> printfn "Part 2: %d"
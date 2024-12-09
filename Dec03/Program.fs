open System
open System.IO
open System.Text.RegularExpressions

let day = 3

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day
let text = File.ReadAllText filename
let getCommands line =
    Regex.Matches(line, "mul\(\d+\,\d+\)|don't\(\)|do\(\)") 
    |> Seq.map (_.Value)
        
let getProduct line =
    Regex.Matches(line, "([0-9]+)")
    |> Seq.map (_.Value)
    |> Seq.map int
    |> Seq.reduce (*)

let folder (isPart1:bool) (accumulatedSum, enabled) item =
    match item with 
    | "do()" -> (accumulatedSum, true)
    | "don't()" -> (accumulatedSum, isPart1)
    | _ -> if enabled then
               (accumulatedSum + (getProduct item), enabled)
           else
               (accumulatedSum, enabled)
               
let calc partNr line =
    line
    |> getCommands 
    |> Seq.fold (folder (partNr=1)) (0,true)
    
calc 1 text |> fst |> printfn "Part1: %A"
calc 2 text |> fst |> printfn "Part2: %A"
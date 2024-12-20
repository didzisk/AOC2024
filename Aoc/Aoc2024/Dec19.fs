module Dec19

open System
open System.IO
open StringUtils

let day = 19
MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

let Del1 () =
    inputString |> printfn "%s"

    let patternsTowels = inputString.Split("\n\n")
    let patterns = patternsTowels[0].Split(", ")
    let towels = patternsTowels[1].Split("\n")
    towels|> Array.iter (printfn "%s")
    
    

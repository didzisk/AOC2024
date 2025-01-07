module Dec21

open System
open System.IO

let day = 21
MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

let parse (text:string) =
    let lines = text.Split("\n", StringSplitOptions.RemoveEmptyEntries + StringSplitOptions.TrimEntries)
    lines 

let Del1 () =
    parse inputString
    |> Seq.iter (printfn "%s")
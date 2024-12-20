module Dec18

open System
open System.IO
open StringUtils

let day = 18
MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

let Del1 () =
    inputString |> printfn "%s"

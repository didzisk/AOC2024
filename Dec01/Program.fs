open System
open System.IO

let day = 1

MetaUtils.getTodayInput day

let filename = MetaUtils.todayFilename day

let lines = File.ReadAllLines filename
let originalArray = 
    lines
    |> Array.map (fun x->
        x.Split(' ')
        |> Array.filter (fun xx-> not (xx.Equals(String.Empty))))

let leftArray =
    originalArray
    |> Array.map (fun x-> x.[0] |> int64)
    |> Array.sort
    
let rightArray =
    originalArray
    |> Array.map (fun x-> x.[1] |> int64)
    |> Array.sort
 
Array.zip leftArray rightArray
|> Array.map (fun (x,y) -> (x,y,Math.Abs(x-y)))
|> Array.iter (printfn "%A")

Array.zip leftArray rightArray
|> Array.map (fun (x,y) -> Math.Abs(x-y))
|> Array.sum
|> printfn "Part1: %A"

leftArray
|> Array.map (fun x->
    let count =
        rightArray
        |> Array.filter (fun y-> y=x)
        |> Array.length
        |> int64
    count * x
    )
|> Array.sum
|> printfn "Part2: %A"



open System
open System.IO

let day = 2

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day
let lines = File.ReadAllLines filename
let originalArray = 
    lines
    |> Array.map (fun x->
        x.Split(' ')
        |> Array.filter (fun xx-> not (xx.Equals(String.Empty)))
        |> Array.map int32
    )

let isAscending = Array.forall (fun (a,b)-> a-b < 0 && a-b > -4)
let isDescending = Array.forall (fun (a,b)-> a-b < 4 && a-b > 0)

let reportIsGood origLine =
    let arr = Array.pairwise origLine
    arr |> isAscending || arr |> isDescending
    
let calc1 () =
    originalArray
    |>Array.filter reportIsGood
    |>Array.length
    |> printfn "Part1: %A"

let excludeNth (arr:int array) (n:int) =
    [|
            yield! arr[0..n-1]
            yield! arr[n+1..arr.Length-1]
    |]

let arrayPermutationsWithCheck checkFun (arr:int array) =
    [
        for i in [0..arr.Length-1] do
            yield excludeNth arr i |> checkFun
    ]

let checkLineWithPermutations checkFun (arr:int array) =
    arrayPermutationsWithCheck checkFun arr
    |> List.contains true
    
let checkLinePart2 = checkLineWithPermutations reportIsGood

let calc2 () =
    originalArray
    |>Array.filter checkLinePart2
    |>Array.length
    |> printfn "Part2: %A"
    
calc1 ()
calc2 ()
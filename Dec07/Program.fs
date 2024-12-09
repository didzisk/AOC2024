open System.IO
open StringUtils
let day = 7

//MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day
let text =
    File.ReadAllLines filename
    
let parse line =
    let arr = 
        line
        |> split ": "
        |> Array.map int64
    (arr[0], arr[1..] |> Array.toList)
    
let addNewLevel (elements:'a list) (input: 'a list list) =
    [
        for a in elements do
            for b in input do
                yield a::b
    ]

let mul = (*)
let add = (+)
let addPlusMul =
    addNewLevel [mul;add]
    
let prependIdentity (all:(int64->int64->int64) list) =
    (fun (_:int64) (b:int64) -> b)::all
        
let functionCombinations (n:int) =
    let mutable combinations = [[]]
    for i in 1..n do
        combinations <- addPlusMul combinations
    combinations

let processOneCombination (line: (int64 * (int64 -> int64 -> int64)) list) =
//    printfn "%A" line
    let result =
        line |> List.fold
            (
            fun x (value, thisFunc)->
                thisFunc x value
            ) 0L
//    printfn "%A" result
    result
    
let processOneLine funcs s =
    printfn "%A" s
    let (expected, m) = parse s
    funcs (List.length m-1)
    |> List.map prependIdentity
    |> List.map (fun xs->
        List.take (List.length m) xs
        |> List.zip m)
    |> List.map processOneCombination
    |> List.contains expected

let calc funcs text =
    text
    |> Array.filter (processOneLine funcs)
    |> Array.map (fun s->
        let (x,a)=parse s
        x)
    |> Array.sum

    |> (printfn "%A")

    
let con (a:int64) (b:int64) =
    a.ToString()+b.ToString() |> int64
let addPlusMulCon =
    addNewLevel [mul;add;con]
    
let functionCombinations2 (n:int) =
    let mutable combinations = [[]]
    for i in 1..n do
        combinations <- addPlusMulCon combinations
    combinations

let ex=
    @"190: 10 19
    3267: 81 40 27
    83: 17 5
    156: 15 6
    7290: 6 8 6 15
    161011: 16 10 13
    192: 17 8 14
    21037: 9 7 18 13
    292: 11 6 16 20"
    |> split "\r\n"
text 
|> (calc functionCombinations)

printfn "Del2ex:"
ex 
|> (calc functionCombinations2)

printfn "Del2:"
text 
|> (calc functionCombinations2)

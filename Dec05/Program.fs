open System
open System.IO
open StringUtils
let day = 5

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day
let text = File.ReadAllLines filename
    
let parseRule s =
    split "|" s
    |> (fun a -> int a[0], int a[1])
    
let rulesEndAt text =
    Array.indexed text
    |> Array.find (fun (i,a)-> String.IsNullOrEmpty a)
    |> fst

let readRules text =
    let re = rulesEndAt text
    Array.take re text
    |> Array.map parseRule
    
let allAfterMe (rules: (int * int) array) (i:int) =
    rules
    |> Array.filter (fun a-> fst a=i)
    |> Array.map snd
    
let parseUpdate (s:string) =
    split "," s
    |> Array.map int
    
let readUpdates text =
    let re = (rulesEndAt text)+1
    Array.skip re text
    |> Array.map parseUpdate

let brokenRule (line:int array) idx rules =
    let a = allAfterMe rules line[idx]
    line
    |> Array.take idx
    |> Array.exists (fun e -> Array.contains e a)
    
let breaksARule rules (line:int array) =
    line
    |> Array.mapi (fun idx _ -> brokenRule line idx rules)
    |> Array.exists id
    
let compliesToAllRules rules line = not (breaksARule rules line)
    
let middle (arr:int array) =
    arr[(Array.length arr)/2]
    
let calc1 text =
    let rules = readRules text
    text
    |> readUpdates 
    |> Array.filter (compliesToAllRules rules)
    |> Array.map middle
    |> Array.sum
    |> printfn "%A"

calc1 text

let compareEntries rules n1 n2 =
    let a1 = allAfterMe rules n1
    let a2 = allAfterMe rules n2
    if a1 |> Array.contains n2  then
        -1
    else
        if  a2 |> Array.contains n1 then
            1
        else
            0

let sortTheLine rules (arr:int array) =
    arr
    |> Array.sortWith (compareEntries rules)
    
let calc2 text = 
    let rules = readRules text
    text
    |> readUpdates 
    |> Array.filter (breaksARule rules)
    |> Array.map (sortTheLine rules)
    |> Array.map middle
    |> Array.sum
    |> printfn "%A"

calc2 text
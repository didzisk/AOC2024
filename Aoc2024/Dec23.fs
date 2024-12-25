module Dec23

open System
open System.IO
open Microsoft.FSharp.Core
open StringUtils

let day = 23
MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

let parseInput (text:string) =
    let conn =
        text
        |> split "\r\n"
        |> Array.map (fun x ->
            let arr = split "-" x |> Array.sort
            arr[0], arr[1])
        |> Set.ofArray
    conn
        
let sortTriplet a b c =
    let arr = [|a;b;c|]
    arr |> Array.sortInPlace
    arr[0], arr[1], arr[2]
    
let makeTriplets arr = 
    seq {
        for first, second in arr do
            for _,b in (arr |> Set.filter (fun (x,_)-> x = second)) do
                let c,d,e = sortTriplet first second b
                if Set.contains (c,e) arr then
                    yield (c,d,e) 
    }
    
//returns all others connected directly to this one
let groupConn (conn:Set<string * string>) =
    let reversed =
        conn
        |> Set.map (fun (a,b)->b,a)
    Set.union conn reversed
    |> Seq.groupBy fst
    |> Seq.map (fun (a, s) ->
        a, s |> Seq.map snd
        )
    |> Map.ofSeq
    
let addOneLevel (conn:Set<string * string>) (cur:string array array) =
    let connGrouped = groupConn conn
    [|
    for line in cur do
        for a in connGrouped.Keys do
            if line |> Array.forall(fun x-> connGrouped[a] |> Seq.contains x) then
                yield (line |> Array.append [|a|] |> Array.sort)
    |]
    |> Array.distinct

let parse2 text =
    text
        |> split "\r\n"
        |> Array.map (split "-")

let calc1 (text:string) =
    let conn = parseInput text
    conn
    |> makeTriplets
    |> Seq.filter (fun (a,b,c)-> a.StartsWith("t") || b.StartsWith("t") || c.StartsWith("t"))
    //|> Seq.iter (printfn "%A")
    |> Seq.length
    |> (printfn "%A")
let Del1() =
    calc1 inputString

let calc12 text =
    let conn = parseInput text
    let lev2 = parse2 text
    let lev3 = addOneLevel conn lev2
    let arr =
        lev3
        |> Array.filter (fun a-> a[0].StartsWith("t") || a[1].StartsWith("t") || a[2].StartsWith("t"))
    arr |> Seq.iter (printfn "%A")
    arr    |> Array.length

let rec genNext conn curr =
    let next = addOneLevel conn curr
    if Array.length next = 1 then
        next
    else
        genNext conn next
        
let calc2 text =
    let conn = parseInput text
    let lev2 = parse2 text
    let final = genNext conn lev2
    final |> Seq.iter (printfn "%A")
    String.Join(",", final[0])
    
let ex = @"kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn"

let Del1ex() =
    calc1 ex
    
let Del12() =
    calc12 inputString |> printfn "%d"
    
let Del2ex() =
    calc2 ex |> printfn "%s"
    
let Del2() =
    calc2 inputString |> printfn "%s"
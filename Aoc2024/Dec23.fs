module Dec23

open System
open System.IO
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
    //conn |> Seq.iter (printfn "%A")
    conn
        
let sortTriplet a b c =
    let arr = [|a;b;c|]
    arr |> Array.sortInPlace
    arr[0], arr[1], arr[2]
    
let calc1 (text:string) =
    let arr = parseInput text
    seq {
        for (first, second) in arr do
            for (a,b) in (arr |> Set.filter (fun (x,y)-> x = second)) do
                let (c,d,e)=sortTriplet first second b
                if Set.contains (c,e) arr then
                    yield (c,d,e) 
    }
    |> Seq.filter (fun (a,b,c)-> a.StartsWith("t") || b.StartsWith("t") || c.StartsWith("t"))
    //|> Seq.iter (printfn "%A")
    |> Seq.length
    |> (printfn "%A")
let Del1() =
    calc1 inputString
    
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
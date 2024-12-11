open System.IO
open StringUtils
let day = 11

//MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let getInput() =
    File.ReadAllText filename
    |> trim
    |> split " "

let rules (s, count) =
    if s="0" then
        seq{ "1", count }
    else
        if s.Length % 2 = 0 then
            let num = s.Length / 2
            seq{
              s.Substring(0,num), count
              (s.Substring(num) |> int64 |> _.ToString(), count)
              }
        else
            let i = int64 s
            seq { (i*2024L).ToString(), count }
            
let nextStep line =
    seq {
        for s in line do
            yield! rules s
    }
    |> Seq.groupBy fst
    |> Seq.map (fun (a,x)->a, x|>Seq.map snd|>Seq.sum)

let rec blink n =
    if n=1 then
        nextStep
    else
        nextStep >> blink (n-1)
        
let calc n =
    Seq.countBy id
    >> Seq.map (fun (a,x) -> a, int64 x)
    >> (blink n)
    >> Seq.sumBy snd

getInput() |> calc 25 |> printfn "Del1 %A "
getInput() |> calc 75 |> printfn "Del2 %A "
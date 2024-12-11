open System.IO
open StringUtils
let day = 11

//MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let getWorld() =
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
        
let calc blinkFunc s =
    s
    |> Seq.countBy id
    |> Seq.map (fun (a,x) -> a, int64 x)
    |> blinkFunc
    |> Seq.sumBy snd
    
let calc1 = calc (blink 25) 

let calc2 = calc (blink 75)
    
seq{"125"; "17"} |> calc1|> printfn "example Del1 %A "

calc1 (getWorld()) |> printfn "Del1 %A "
calc2 (getWorld()) |> printfn "Del2 %A "
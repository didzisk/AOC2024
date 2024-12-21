module Dec13

open System
open System.IO
open System.Text.RegularExpressions
open StringUtils

let day = 13

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day
let inputString = File.ReadAllText filename
let getCommands line =
    Regex.Matches(line, "\d+") 
    |> Seq.map (_.Value)
    |> Seq.map int
    |> Seq.toArray

let calc1 (text:string) =
    let lines = text.Split("\n", StringSplitOptions.RemoveEmptyEntries + StringSplitOptions.TrimEntries)
    let t =
        seq{
            for i in 0..(lines.Length / 3 - 1) do
               yield lines[3*i]+lines[3*i+1]+lines[3*i+2] 
            }
            
    let mt =
        t
        |> Seq.map getCommands
    
    mt |> Seq.iter (printfn "%A")    
    
    let mi =
        mt
        |> Seq.map (fun arr->
            let xa=int arr[0]
            let ya=int arr[1]
            let xb=int arr[2]
            let yb=int arr[3]
            let xp=int arr[4]
            let yp=int arr[5]
            let k = (xa*yp-ya*xp)/(xa*yb-ya*xb)
            let n = (xp-xb*k)/xa
            let calcX = xa * n + xb * k
            let calcY = ya * n + yb * k
            n,k,xp,calcX,yp,calcY
            )
    mi
    |> Seq.iter (printfn "%A")    

    let mcalc =
        mi
        |>Seq.map (fun (n,k,xp,calcX,yp,calcY)->
            if n>= 0 && n<=100 && k>=0 && k<=100 && xp=calcX && yp=calcY then
                n*3 + k
            else
                0
            )
        |> Seq.sum
    mcalc
    |>  (printfn "%A")

let calc2 (text:string) =
    let lines = text.Split("\n", StringSplitOptions.RemoveEmptyEntries + StringSplitOptions.TrimEntries)
    let t =
        seq{
            for i in 0..(lines.Length / 3 - 1) do
               yield lines[3*i]+lines[3*i+1]+lines[3*i+2] 
            }
            
    let mt =
        t
        |> Seq.map getCommands
    
    mt |> Seq.iter (printfn "%A")    
    
    let mi =
        mt
        |> Seq.map (fun arr->
            let xa=int64 arr[0]
            let ya=int64 arr[1]
            let xb=int64 arr[2]
            let yb=int64 arr[3]
            let xp=int64 arr[4] + 10000000000000L
            let yp=int64 arr[5] + 10000000000000L
            let k = (xa*yp-ya*xp)/(xa*yb-ya*xb)
            let n = (xp-xb*k)/xa
            let calcX = xa * n + xb * k
            let calcY = ya * n + yb * k
            n,k,xp,calcX,yp,calcY
            )
    mi
    |> Seq.iter (printfn "%A")    

    let mcalc =
        mi
        |>Seq.map (fun (n,k,xp,calcX,yp,calcY)->
            if n>= 0 && k>=0 && xp=calcX && yp=calcY then
                n*3L + k
            else
                0
            )
        |> Seq.sum
    mcalc
    |>  (printfn "%A")

let Del1 () = calc1 inputString

let ex = @"Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"

let Del1ex () = calc1 ex
let Del2ex () = calc2 ex

let Del2 () = calc2 inputString

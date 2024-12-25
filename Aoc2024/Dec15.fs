module Dec15

open System
open System.IO
open StringUtils

let day = 15
//MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

let parseInput (text:string) =
    let lines = text.Split [|'\n'|]
    let br = lines |> Array.indexed |> Array.find (fun (_,x)-> String.IsNullOrWhiteSpace x) |> fst
    let world =
        lines
        |> Array.take br
        |> Array.map _.ToCharArray()
    let startPos =
        world |> ArrayUtils.findWhere (fun x-> x='@')
    
    let tasks =
        lines
        |> Array.skip br
        |> Array.map _.Trim()
        |> (fun x-> String.Join ("", x))
    world, tasks, startPos
    
let directions = [|'^',(-1,0);'>',(0,1); 'v',(1,0);'<',(0,-1)|] |> Map.ofArray

let rec checkDirection (r0,c0) (d:char) (world:char array array) (dist0:int) =
    let dir = directions[d]
    let r=r0 + fst dir
    let c=c0 + snd dir
    let dist = dist0 + 1
    if world[r][c] = '#' then
        None
    else
        if world[r][c] = '.' then
            Some (r,c, dist)
        else
            checkDirection (r,c) d world dist

let pushDirection  (r0,c0) (d:char) (world:char array array) =
    match checkDirection (r0, c0) d world 0 with
    | None ->(r0, c0)
    | Some (r,c, dist) ->
        let (dirR, dirC) = directions[d]
        world[r0][c0]<-'.'
        world[r0+dirR][c0+dirC]<-'@'
        for i in 2..dist do
            world[r0+dirR*i][c0+dirC*i]<-'O'
        (r0+dirR, c0+dirC)

let calc1Score (world:char array array) =
    seq {
        for r in 0..(world.Length-1) do
            for c in 0..(world[0].Length-1) do
                if world[r][c] = 'O' then
                    yield 100*r+c
         }
    |> Seq.sum
    
let calc1 text =
    let world, tasks, startPos = parseInput text
    
    let folder (r0, c0) d =
        pushDirection (r0, c0) d world 
        
    tasks |> Seq.fold folder startPos |> printfn "Final pos: %A"
    
    calc1Score world |> printfn "Del 1: %d"
              
let Del1 () =
    inputString
    |> parseInput
    |> (fun (w,t, s) ->
        printfn "%A, %s" w t
        printfn "StartPos: %A" s )
    
    calc1 inputString
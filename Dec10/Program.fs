open System.IO
open StringUtils
let day = 10

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let getWorld() =
    File.ReadAllLines filename
    |> Array.map(fun x-> x.ToCharArray() |> Array.map c2i)

let directions = [(0,1);(1,0);(0,-1);(-1,0)]

let canGo (world: int array array) (pos: int * int) (dir: int*int) =
    let r,c = pos
    let dr, dc = dir
    let r1 = r+dr
    let c1 = c+dc
    let validRow = r1>=0 && r1<world.Length
    let validCol = c1>=0 && c1<world[0].Length
    if not (validRow && validCol) then
        None
    else
        let a = world[r][c]
        let a1 = world[r1][c1]
        if a1 = a+1 then Some ((r1, c1), a1)
        else None
    
let rec allTopsFromOnePosition (world: int array array) (existingTops:(int * int) list) (pos: int * int) =
        directions
        |> List.collect (fun dir-> 
            match canGo world pos dir with
            | Some (a, 9) -> a :: existingTops 
            | Some (a, _) -> allTopsFromOnePosition world existingTops a
            | None -> existingTops
            )
let scoreOnePosition (world: int array array) filter (pos: int * int) =
    match pos with
    | r,c when world[r][c] = 0 ->
        allTopsFromOnePosition world [] pos
        |> filter
        |> Seq.length
    | _ -> 0
    
let calcScores filter (world: int array array) =
    seq{
        for r in 0..world.Length-1 do
            for c in 0..world[0].Length-1 do
                yield scoreOnePosition world filter (r,c)
    }
    |> Seq.sum

getWorld()
|> calcScores Seq.distinct 
|> printfn "Del1: %A"

getWorld()
|> calcScores id 
|> printfn "Del2: %A"
module Dec12

open System
open System.Collections.Generic
open System.IO

let day = 12
//MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

let parseInput (text:string) =
    let lines = text.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries + StringSplitOptions.TrimEntries)
    let world =
        lines
        |> Array.map _.ToCharArray()
    world    
    
[<NoComparison; CustomEquality>]
type Position =
    {r:int;c:int}
    interface IEquatable<Position> with
        member this.Equals (other: Position): bool = 
            (this.c = other.c) && (this.r = other.r)

    override this.Equals(obj) = failwith "todo"
    override this.GetHashCode() =
        this.r*1000+this.c

let DIRS = [|(-1,0);(0,1);(1,0);(0,-1)|] // up right down left


let calc1 (text:string) =
    let world = parseInput text
    let seen = Array2D.create world.Length world[0].Length false //HashSet<Position>()
    //let blocks = Dictionary<char, int * int>()
    
    let countPerimeter r0 c0 =
        DIRS
        |> Array.map (fun (dr,dc) ->
            if r0+dr>=0 && r0+dr<world.Length && c0+dc>=0 && c0+dc<world[0].Length then
                if  world[r0+dr][c0+dc]=world[r0][c0] then
                    0
                else
                    1
            else
                1
            )
        |> Array.sum
             
    let rec fillFromPoint r0 c0 =
        if seen[r0,c0] then
            (0,0)
        else
            seen[r0,c0]<-true
            let p0 = countPerimeter r0 c0
            let arr =
                DIRS
                |> Array.map (fun (dr,dc) ->
                    if r0+dr>=0 && r0+dr<world.Length && c0+dc>=0 && c0+dc<world[0].Length then
                        if  world[r0+dr][c0+dc]=world[r0][c0] then
                            fillFromPoint (r0+dr) (c0+dc)
                        else
                            (0,0)
                    else
                        (0,0)
                    )
            let p = p0 + (arr |> Array.sumBy(snd))
            let n = 1 + (arr |> Array.sumBy(fst))
            (n,p)

            
    [
    for r in 0..world.Length-1 do
        for c in 0..world[0].Length-1 do
            if not (seen[r,c]) then
                let code = world[r][c]
                let (n,p)=fillFromPoint r c
                printfn $"{code} {n} * {p} = {n*p}"
                yield n*p
    ]
    |> List.sum
                // let p = countPerimeter r c
                // if blocks.ContainsKey(code) then
                //     let n0, p0 = blocks[code]
                //     blocks[code]<-(n0+1, p0+p)
                // else
                //     blocks.Add(code, (1, p))
    //blocks
    // |> Seq.map (fun kvp->
    //         let n,p = kvp.Value
    //         n * p
    //         )
    // |> Seq.sum
            // if seen.Add({Position.r=r;c=c}) then
            //     let seenHere = HashSet<Position>()
            //     let code = world[r][c]
            //     let rec checkHeighbors r0 c0 =
            //         for d in DIRS do
                        
                
    
let Del1 () = inputString |> calc1 |> printfn "%A"

let test1 () =
    @"RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"
    |> calc1
    |> (printfn "%A")
    //|> Seq.iter (printfn "%A")
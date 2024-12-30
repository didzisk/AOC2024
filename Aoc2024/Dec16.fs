module Dec16

open System
open System.Collections.Generic
open System.IO
open StringUtils

let day = 16
MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

let parseInput (text:string) =
    let lines = text |> split "\n"
    let world = lines |> Array.map _.Trim() |> Array.map _.ToCharArray()
    //lines |> Seq.iter (printfn "%s")
    let s = world |> ArrayUtils.findWhere (fun x-> x='S')
    let e = world |> ArrayUtils.findWhere (fun x-> x='E')
    world, s, e
    
[<NoComparison; CustomEquality>]
type CurrentState =
    {r:int;c:int;dir:int}
    interface IEquatable<CurrentState> with
        member this.Equals (other: CurrentState): bool = 
            let x = this
            let y = other
            (x.c = y.c) && (x.r = y.r) && (x.dir = y.dir)

    override this.Equals(obj) = failwith "todo"
    override this.GetHashCode() =
        this.dir*1000_000+this.r*1000+this.c

let DIRS = [|(-1,0);(0,1);(1,0);(0,-1)|] // up right down left

let dijkstra (world:char array array) rs cs re ce  dir= 
    let DIST = Dictionary<CurrentState, int>() //current pos with direction - and distance
    let Q = PriorityQueue<CurrentState, int>()//(world.Length * world[0].Length, KVPComparer<int, CurrentState>(KeyComparer<int>.Default, KeyComparer<CurrentState>.Default)); //Distance and current pos with direction, sorted by distance
    let SEEN = HashSet<CurrentState>()
    let mutable best = -1
    let mutable curr = {r=rs; c=cs; dir = dir}
    let mutable arrivalDir = 0
    let mutable d = 0
    Q.Enqueue( curr, 0)
    while Q.TryDequeue(&curr, &d) do
        if not (DIST.ContainsKey(curr)) then
            DIST.Add(curr, d)
        if curr.r = re && curr.c = ce && best = -1 then
            arrivalDir <- curr.dir
            best <- d
        if not(SEEN.Contains curr) then
            SEEN.Add(curr) |> ignore
            let dr,dc = DIRS[curr.dir]
            let rr,cc = curr.r+dr,curr.c+dc
            if cc>=0 && cc<world[0].Length && rr>=0 && rr<world.Length && world[rr][cc] <> '#' then
                Q.Enqueue({r = rr; c = cc; dir = curr.dir}, d+1)
            let dirPlus90 = (curr.dir+1) % 4
            Q.Enqueue({curr with dir = dirPlus90}, d+1000)
            let dirPlus270 = (curr.dir+3) % 4
            Q.Enqueue({curr with dir = dirPlus270}, d+1000)
    best, arrivalDir

let calc1 (text:string) =
    let world, (rs,cs), (re,ce)  = parseInput text
    dijkstra world rs cs re ce 1

let calc2  (text:string) =
    let world, (rs,cs), (re,ce)  = parseInput text
    let best, _ = dijkstra world rs cs re ce 1
    let s = 
        seq{
        for r in 1..world.Length-2 do
            printfn "Row %d" r
            for c in 1..world[0].Length-2 do
                printf "."
                let dist1, dir1 = dijkstra world rs cs r c 1
                let (dist2,_) = dijkstra world r c re ce dir1
                if dist1+dist2 = best then
                    yield r,c
        }
        |> Seq.distinct
        |> List.ofSeq
    (Seq.length s), s, world
    
    
let Del1 () =
    calc1 inputString
    |> fst
    |> printfn "Part 1: %d" 
  
let ex2 = @"#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################"
//cost 11048
//type 

let Del1ex () =
    calc1 ex2
    |> fst
    |> printfn "Part 1ex: %d"
    
let drawWorld (world:char array array) (path:(int * int) seq) =
    let p = path |> Set.ofSeq
    for r in 0..world.Length-1 do
        printfn ""
        for c in 0..world[0].Length-1 do
            if p.Contains(r,c) then
                printf "O"
            else
                printf "%c" (world[r][c])
                
let Del2ex () =
    let l, s, w = calc2 ex2
    l |> printfn "Part 2ex: %d"
    drawWorld w s
    
let Del2 () =
    let l, s, w = calc2 inputString
    l |> printfn "Part 2: %d"
    //drawWorld w s
    

                
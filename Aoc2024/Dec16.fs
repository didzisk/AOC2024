module Dec16

open System
open System.Collections.Generic
open System.IO
open Spreads
open Spreads.Collections
open StringUtils

let day = 16
MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

let parseInput (text:string) =
    let lines = text |> split "\n"
    let world = lines |> Array.map _.ToCharArray()
    //lines |> Seq.iter (printfn "%s")
    let s = world |> ArrayUtils.findWhere (fun x-> x='S')
    let e = world |> ArrayUtils.findWhere (fun x-> x='E')
    world, s, e
    
[<CustomComparison; CustomEquality>]
type CurrentState =
    {r:int;c:int;dir:int}
    interface IComparable with 
        member this.CompareTo (obj:obj) =
            match obj with
            | null                 -> 1
            | :? CurrentState as y ->
                //(this :> IComparable<_>).CompareTo other
                let x = this
                if (x.c < y.c) || (x.r < y.r) || (x.dir < y.r) then -1
                else
                    if (x.c = y.c) || (x.r = y.r) || (x.dir = y.r) then 0
                    else 1
            | _                    -> invalidArg "obj" "not a CurrentState"
    interface IEquatable<CurrentState> with
        member this.Equals (other: CurrentState): bool = 
            let x = this
            let y = other
            (x.c = y.c) && (x.r = y.r) && (x.dir = y.dir)

    override this.Equals(obj) = failwith "todo"
    override this.GetHashCode() =
        this.dir*1000_000+this.r*1000+this.c

let calc1 (text:string) =
    let world, s, e  = parseInput text
    let DIRS = [|(-1,0);(0,1);(1,0);(0,-1)|] // up right down left
    let DIST = Dictionary<CurrentState, int>() //current pos with direction - and distance
    let Q = PriorityQueue<CurrentState, int>()//(world.Length * world[0].Length, KVPComparer<int, CurrentState>(KeyComparer<int>.Default, KeyComparer<CurrentState>.Default)); //Distance and current pos with direction, sorted by distance
    let SEEN = HashSet<CurrentState>()
    let mutable best = -1
    let mutable curr = {r=fst s; c=snd s; dir = 1}
    let mutable d = 0
    Q.Enqueue( curr, 0)
    while Q.TryDequeue(&curr, &d) do
        if not (DIST.ContainsKey(curr)) then
            DIST.Add(curr, d)
        if curr.r = fst e && curr.c = snd e && best = -1 then
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
    printfn "Part 1: %d" best
            
            
// while Q:
//     d,r,c,dir = heapq.heappop(Q)
//     if (r,c,dir) not in DIST:
//         DIST[(r,c,dir)] = d
//     if r==er and c==ec and best is None:
//         best = d

//     if (r,c,dir) in SEEN:
//         continue
//     SEEN.add((r,c,dir))
//     dr,dc = DIRS[dir]
//     rr,cc = r+dr,c+dc
//     if 0<=cc<C and 0<=rr<R and G[rr][cc] != '#':
//         heapq.heappush(Q, (d+1, rr,cc,dir))
//     heapq.heappush(Q, (d+1000, r,c,(dir+1)%4))
//     heapq.heappush(Q, (d+1000, r,c,(dir+3)%4))

    
    
    
let Del1 () =
    calc1 inputString
    
  
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
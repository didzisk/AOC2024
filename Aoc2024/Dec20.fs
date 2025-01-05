module Dec20

open System
open System.Collections.Generic
open System.Drawing
open System.IO
open GridForm
open StringUtils

let day = 20
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
    {r:int;c:int}
    interface IEquatable<CurrentState> with
        member this.Equals (other: CurrentState): bool = 
            (this.c = other.c) && (this.r = other.r)

    override this.Equals(obj) = failwith "todo"
    override this.GetHashCode() =
        this.r*1000+this.c

let DIRS = [|(-1,0);(0,1);(1,0);(0,-1)|] // up right down left

let unravelDist (dist : Dictionary<CurrentState, int>) =
    dist
    |> Seq.sortBy (_.Value)
    |> Seq.map(fun kvp -> kvp.Key.r, kvp.Key.c, kvp.Value)

let dijkstra (world:char array array) rs cs re ce = 
    let DIST = Dictionary<CurrentState, int>() 
    let Q = PriorityQueue<CurrentState, int>()
    let SEEN = HashSet<CurrentState>()
    let mutable best = -1
    let mutable curr = {r=rs; c=cs}
    let mutable d = 0
    Q.Enqueue( curr, 0)
    while Q.TryDequeue(&curr, &d) do
        if not (DIST.ContainsKey(curr)) then
            DIST.Add(curr, d)
        if curr.r = re && curr.c = ce && best = -1 then
            best <- d
        if not(SEEN.Contains curr) then
            SEEN.Add(curr) |> ignore
            
            for (dr,dc) in DIRS do
                let rr,cc = curr.r+dr,curr.c+dc
                if cc>=0 && cc<world[0].Length && rr>=0 && rr<world.Length && world[rr][cc] <> '#' then
                    Q.Enqueue({r = rr; c = cc}, d+1)
    best, DIST 

               
let SCALE = 4.0

let paintWorld (world:char array array) (path:(int * int * int) seq) =
    for r in 0..world.Length-1 do
        printfn ""
        for c in 0..world[0].Length-1 do
            match world[r][c] with
            | '#' -> PointRC mf SCALE Color.Gray r c
            | 'S' -> PointRC mf SCALE Color.Green r c
            | 'E' -> PointRC mf SCALE Color.Black r c
            | _ -> ()
    let maxDist = path |> Seq.map (fun (_,_,x)->x) |> Seq.max |> float
    let halfdist = maxDist / 2.0
    path
    |> Seq.iter (fun (r,c,d)->
        let color =
            if float d < halfdist then
                let p1 = (float d) / halfdist
                let green =(p1 * 255.0) |> int
                let blue = (255.0 - p1 * 255.0) |> int
                Color.FromArgb(0, green, blue)
            else
                let p1 = (float d - halfdist) / halfdist
                let red = (p1 * 255.0) |> int
                let green = (255.0 - p1 * 255.0) |> int
                Color.FromArgb(red, green, 0)
        PointRC mf SCALE color r c
        )
        

let test1 (text:string) =
    let world, (rs,cs), (re,ce)  = parseInput text
    let best, path = dijkstra world rs cs re ce
    paintWorld world (unravelDist path)
    Console.WriteLine($"Current best: {best} Press Enter to continue")
    Console.ReadLine() |> ignore
    world[95][52]<-'.'
    let best1, path1 = dijkstra world rs cs re ce
    paintWorld world (unravelDist path1)
    Console.WriteLine($"Current best: {best1} Press Enter to exit")

let calc1 (text:string) =
    let world, (rs,cs), (re,ce)  = parseInput text
    let best, path = dijkstra world rs cs re ce
    Console.WriteLine($"Current best: {best}")
    let mutable counter = 0
    for r in 1..world.Length-2 do
        printfn ""
        printfn $"row {r}"
        for c in 1..world[0].Length-2 do
            printf "."
            let world1, _, _ = parseInput text
            if world1[r][c] = '#' then
                world1[r][c] <- '.'
                let best1,_ = dijkstra world1 rs cs re ce
                if best - best1 > 99 then
                    counter<-counter+1
    printfn $"Part 1: %d{counter}"

let calc2 (text:string) =
    let world, (rs,cs), (re,ce)  = parseInput text
    let best, path = dijkstra world rs cs re ce
    let mutable counter = 0
    for r in 1..world.Length-2 do
        //printfn ""
        //printfn $"row {r}"
        for c in 1..world[0].Length-2 do
            //printf "."
            let curr = {CurrentState.r=r; c=c}
            if path.ContainsKey(curr) then
                let currDist = path[curr]
                for dr = -20 to 20 do
                    for dc = -20 to 20 do
                        let mdist = abs(dr)+abs(dc)
                        if mdist<=20 then
                            let t = {CurrentState.r=r+dr; c=c+dc}
                            if path.ContainsKey(t) then
                                let newD = path[t]+mdist
                                if currDist - newD >= 100 then
                                    counter <- counter+1
    printfn $"Part 2: %d{counter}"
                            
    
let Del1() =
    calc1 inputString
    
let Del2() =
    calc2 inputString
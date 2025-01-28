module Dec15

open System
open System.IO
open System.Reflection.PortableExecutable
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
    
[<NoComparison; CustomEquality>]
type Position =
    {r:int;c:int}
    interface IEquatable<Position> with
        member this.Equals (other: Position): bool = 
            (this.c = other.c) && (this.r = other.r)

    override this.Equals(obj) = failwith "todo"
    override this.GetHashCode() =
        this.r*1000+this.c

let doubleIt (world:char array array) =
    let obstacles = 
        seq {
            for r in [0..world.Length-1] do
                for c in [0..world[0].Length-1] do
                    if world[r][c] = '#' then
                        yield (r, c*2)
                        yield (r, c*2+1)
            }
        |> Set.ofSeq
    let boxes =
        seq {
            for r in [0..world.Length-1] do
                for c in [0..world[0].Length-1] do
                    if world[r][c] = 'O' then
                        yield (r, c*2)
        }
        |> Array.ofSeq
    obstacles, boxes

let printPart2 obstacles boxes (r0,c0)=
    // Console.Clear()
    // for (r,c) in obstacles do
    //     Console.CursorTop<-r
    //     Console.CursorLeft<-c
    //     Console.Write "#"
    // for (r,c) in boxes do
    //     Console.CursorTop<-r
    //     Console.CursorLeft<-c
    //     Console.Write "[]"
    // Console.CursorTop<-r0
    // Console.CursorLeft<-c0
    // Console.Write "@"
    ()

let rec tryPushBoxHorizontally (obstacles:Set<int*int>) (boxes:(int*int) array) (r0,c0) (direction:int) =
    let thisBox = boxes |> Array.tryFindIndex(fun (r,c)-> r=r0 && c=c0)
    match thisBox with
    | None -> failwith "this box doesn't exist"
    | Some i0 ->
        let hasWall =
            if direction < 0 then
                obstacles.Contains(r0,c0 - 1)
            else
                obstacles.Contains(r0,c0 + 2)
        if hasWall then
            false
        else
            let boxToPush = boxes |> Array.tryFindIndex(fun (r,c)-> r=r0 && c=c0 + 2 * direction)
            match boxToPush with
            | None ->
                boxes[i0]<-(r0,c0 + direction)
                true
            | Some i ->
                if tryPushBoxHorizontally obstacles boxes (r0,c0 + 2 * direction) direction then
                    boxes[i0]<-(r0,c0 + direction)
                    true
                else
                    false
    
let tryGoHorizontally (obstacles:Set<int*int>) (boxes:(int*int) array) (r0,c0) (direction:int) =
    let hasWall = obstacles.Contains(r0,c0 + direction)
    if hasWall then
        (r0,c0)
    else
        let col = if direction<0 then c0-2 else c0+1 
        let boxToPush = boxes |> Array.tryFindIndex(fun (r,c)-> r=r0 && c=col)
        match boxToPush with
        | None ->
            (r0,c0 + direction)
        | Some _ ->
            if tryPushBoxHorizontally obstacles boxes (r0,col) direction then
                (r0,c0 + direction)
            else
                (r0,c0)
                
let rec traverseVertically (obstacles:Set<int*int>) (boxes:(int*int) array) (r0,c0) (d:int) (testOnly)=
    let thisBox = boxes |> Array.tryFindIndex(fun (r,c)-> r=r0 && c=c0)
    match thisBox with
    | None -> failwith "this box doesn't exist"
    | Some i0 ->
        let hasWall = obstacles.Contains(r0+d,c0) || obstacles.Contains(r0+d,c0+1)
        if hasWall then
             false
        else
            let canPushOrIsEmpty (rt,ct) =
                let targetBox = boxes |> Array.tryFindIndex(fun (r,c)-> r=rt && c=ct)
                match targetBox with
                | None -> true
                | Some _ -> traverseVertically obstacles boxes (rt, ct) d testOnly
            
            let topLeftAllows = canPushOrIsEmpty (r0+d, c0-1)
            let topAllows = canPushOrIsEmpty (r0+d, c0)
            let topRightAllows = canPushOrIsEmpty (r0+d, c0+1)
            if topLeftAllows && topAllows && topRightAllows then
                if not testOnly then
                    boxes[i0]<-(r0+d,c0)
                true
            else
                false
                
let canPushVertically obstacles boxes pos direction =
    traverseVertically obstacles boxes pos direction true
    
let doPushVertically obstacles boxes pos direction=
    traverseVertically obstacles boxes pos direction false
    
let tryGoVertically  (obstacles:Set<int*int>) (boxes:(int*int) array) (r0,c0) direction=
    let hasWall = obstacles.Contains(r0+direction,c0)
    if hasWall then
        (r0,c0)
    else
        let boxToPush =
            let candidate1 = boxes |> Array.tryFind(fun (r,c)-> r=r0+direction && c=c0-1)
            match candidate1 with
            | None ->
                let candidate2 =  boxes |> Array.tryFind(fun (r,c)-> r=r0+direction && c=c0)
                match candidate2 with
                | None -> None
                | _ -> candidate2
            | _ -> candidate1
        match boxToPush with
        | None -> (r0+direction,c0)
        | Some (r,c) ->
            if canPushVertically obstacles boxes (r,c) direction then
                doPushVertically obstacles boxes (r,c) direction |> ignore
                (r0+direction,c0)
            else
                (r0, c0)
                
    
let tryGo (obstacles:Set<int*int>) (boxes:(int*int) array) (r,c) dir=
    printPart2 obstacles boxes (r,c)
    // Console.CursorTop <-r
    // Console.CursorLeft <-c
    // Console.CursorVisible<-false
    printf $"{dir}"
    match dir with
    | '<' -> tryGoHorizontally obstacles boxes (r,c) -1
    | '>' -> tryGoHorizontally obstacles boxes (r,c) 1 
    | '^' -> tryGoVertically obstacles boxes (r,c)  -1
    | 'v' -> tryGoVertically obstacles boxes (r,c)  1
    | _ -> failwith "unexpected direction"
    
let calc2 text =
    text
    |> parseInput
    |> (fun (w,t, (r0,c0)) ->
        let obstacles, boxes = doubleIt w
        let start = (r0,c0*2)

        printPart2 obstacles boxes start
        
        let folder2 (r0, c0) d =
            tryGo obstacles boxes (r0, c0) d 
            
        let finalPos = t |> List.ofSeq |> List.fold folder2 start
        finalPos |> printfn "Final pos: %A"
        boxes
        |> Seq.map (fun (r,c)-> r*100 + c)
        |> Seq.sum
        |> printfn "Part2: %d"
        
        printPart2 obstacles boxes finalPos
        
        )
    
let Del2 () =
    inputString |> calc2

let test2small () =
    printfn "test2 small"
    let expected = 105+207+306
    printfn $"Expected: %d{expected}"
    let map = @"#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^"
    map |> calc2
    
let test2big () = 
    printfn "test2 big"
    let expected = 105+207+306
    printfn $"Expected: %d{expected}"
    let map = @"##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
    map |> calc2

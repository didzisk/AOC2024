module Dec18

open System
open System.IO

let day = 18
MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

let parse (text:string) =
    let lines = text.Split("\n", StringSplitOptions.RemoveEmptyEntries + StringSplitOptions.TrimEntries)
    lines
    |> Seq.map(fun x->
        let arr = x.Split ','
        int arr[0], int arr[1]
        )
    
let directions = [|(-1,0); (0,1); (1,0);(0,-1)|]

[<TailCall>]
let rec shortBruteForceSearch (world:int array2d) (isObstacle: int * int -> bool) (x:int,y:int) (current:int) (tx:int, ty:int)=
    for d in directions do
        let candX = x + fst d
        let candY = y + snd d
        if not (isObstacle (candX, candY)) then
            if world[candX,candY] > current then
                world[candX,candY] <- current
                if candX = tx && candY = ty then
                    printfn "%d" current
                    ()
                else
                    shortBruteForceSearch world isObstacle (candX,candY) (current+1) (tx,ty)
        
let calc1 text w h num=
    let world = Array2D.create w h Int32.MaxValue
    let obstacles = 
        parse text
        |> Seq.take num
        |> Set.ofSeq
    let isObstacle (x,y) =
        x < 0 || x >= w || y<0 || y >= h
        || obstacles.Contains (x,y)
    world[0,0]<-0
    shortBruteForceSearch world isObstacle (0,0) 1 (70,70)
    world[70,70]
    
    
let Del1 () =
    let w = 70
    let h = 70
    calc1 inputString (w+1) (h+1) 1024
    |> (printfn "%A")

[<TailCall>]
let rec quickBruteForceSearch (world:int array2d) (isObstacle: int * int -> bool) (x:int,y:int) (current:int) (tx:int, ty:int)=
    if world[tx,ty]<Int32.MaxValue then
        ()
    else
        for d in directions do
            let candX = x + fst d
            let candY = y + snd d
            if not (isObstacle (candX, candY)) then
                if world[candX,candY] > current then
                    world[candX,candY] <- current
                    if candX = tx && candY = ty then
                        printfn "%d" current
                        ()
                    else
                        quickBruteForceSearch world isObstacle (candX,candY) (current+1) (tx,ty)

let pathIsOpen text w h num=
    let world = Array2D.create w h Int32.MaxValue
    let s =
        parse text
        |> Seq.take num
    let obstacles =
        s |> Set.ofSeq
    let isObstacle (x,y) =
        x < 0 || x >= w || y<0 || y >= h
        || obstacles.Contains (x,y)
    world[0,0]<-0
    quickBruteForceSearch world isObstacle (0,0) 1 (70,70)
    world[70,70] <  Int32.MaxValue, s |> Seq.last
    
[<TailCall>]
let rec calcNextInterval text w h (i1, p1) (i2, p2) =
    if i2-i1 = 1 then
        i2, p2
    else
        let middle = (i1+i2) / 2
        let middleIsOpen, pair = pathIsOpen text w h middle
        let first, second = 
            if middleIsOpen then
                (middle, pair), (i2, p2)
            else
                (i1,p1), (middle, pair)
        calcNextInterval text w h first second 

let Del2 () =
    let w = 70
    let h = 70
    calcNextInterval inputString (w+1) (h+1) (1024, (0,0)) (3500, (0,0))
    |> snd |> (printfn "%A")

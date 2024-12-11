open System.IO
open StringUtils

let day = 8

//MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day
let text =
    File.ReadAllLines filename
    |> Array.map(_.ToCharArray())

let getAntinode (world:'a array array) (rfrom:int, cfrom:int) (rto:int, cto:int) =
    (rto+rto-rfrom, cto+cto-cfrom)
    
let getNodes (world:char array array) =
    seq{
        for r in 0..world.Length-1 do
            for c in 0..world[0].Length-1 do
                if world[r][c] <> '.' then
                    yield world[r][c], (r,c)
    }
    |> Seq.groupBy fst
    
let showNodes world =
    getNodes world
    |> Seq.iter (printfn "%A")

//showNodes text

let getAntinodes world =
    seq {
        for s in getNodes world do
            let node = snd s
            for first in node do
                for second in node do
                    if (first|>snd|>fst <> (second|>snd|>fst)) || (first|>snd|>snd <> (second|>snd|>snd))then
                        let a = getAntinode world (snd first) (snd second)
                        if ArrayUtils.validPos world a then 
                            yield getAntinode world (snd first) (snd second)
    }
    |> Seq.distinct
    |> Seq.length
    
getAntinodes text|> (printfn "Del1 %A")

let ex = @"............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"
ex |> split "\r\n"
|> Array.map(_.ToCharArray())
|> getAntinodes
|> (printfn "%A")

let isOnLine (r0: int, c0: int) (r1: int, c1: int) (r:int,c:int) =
    (r-r0)*(c-c1)=(c-c0)*(r-r1)
    
isOnLine (0,0) (1,1) (3,3) |> printfn "isOnLine (0,0) (1,1) (3,3) %A"
isOnLine (1,3) (2,6) (3,9) |> printfn "isOnLine (0,0) (1,4) (2,8) %A"

let getAntinodes2 world =
    seq {
        for s in getNodes world do
            let node = snd s
            for first in node do
                for second in node do
                    if (first|>snd|>fst <> (second|>snd|>fst)) || (first|>snd|>snd <> (second|>snd|>snd))then
                        let pairsAntinodes =
                            world
                            |> ArrayUtils.allWhereRC (
                                fun a ->
                                    isOnLine (snd first) (snd second) a && ArrayUtils.validPos world a)
                        yield! pairsAntinodes
    }
    |> Seq.distinct
    |> Seq.length

ex |> split "\r\n"
|> Array.map(_.ToCharArray())
|> getAntinodes2
|> (printfn "%A")

getAntinodes2 text|> (printfn "Del2 %A")


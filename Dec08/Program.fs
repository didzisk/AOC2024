open System.IO
open StringUtils

let day = 8

//MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day
let text =
    File.ReadAllLines filename
    |> Array.map(_.ToCharArray())

let getAntinode (world:'a array array) (rfrom, cfrom) (rto, cto) =
    (rto+rto-rfrom, cto+cto+cfrom)
    
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

showNodes text

let getAntinodes world =
    seq {
    getNodes world
    |> Seq.iter (fun (_,s) ->
        s
        |> Seq.iter (fun (_,first)->
            s
            |> Seq.iter (fun (_,second)->
                ()
                )
            )
    }
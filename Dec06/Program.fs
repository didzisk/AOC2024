open System.IO
let day = 6

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day
let getWorld() =
    File.ReadAllLines filename
    |> Array.map(_.ToCharArray())

let startPos (world:char array array)=
    world |> ArrayUtils.findWhere (fun a -> a='^')
let world = getWorld()
let startpos = startPos world 
let directions = [|(-1,0); (0,1); (1,0);(0,-1)|]
let turnRight dir =
    if dir = 3 then 0 else dir+1
let go (r,c, dir) =
    let step = directions[dir]
    let r1=r+fst step
    let c1=c+snd step
    r1,c1
    
let rec next (world:char array array) (r,c, dir, count) =
    world[r][c]<-'X'
    let lim = world.Length-1
    let cand = go (r,c,dir)
    if fst cand < 0 || fst cand > lim || snd cand < 0 || snd cand > lim || count>=15000 then
        (r,c,dir,count)
    else
        if world[fst cand ][snd cand]='#' then
            let newDir = turnRight dir
            next world (r,c,newDir,count)
        else            
            next world (fst cand, snd cand, dir, count+1)
            
let calc1 world =
    next world (fst startpos, snd startpos, 0, 0)
    |> ignore
    world
    |> Array.map(fun a->
        a
        |> Array.filter(fun b->b='X')
        |> Array.length
    )
    |> Array.sum
    
calc1 world
|> printfn "%A"
let obstacledWorlds (world:char array array)=
    seq {
    for r in 0..world.Length-1 do
        for c in 0..world[r].Length-1 do
            if world[r][c]<>'^' && world[r][c]<>'#'then
                let newW = getWorld ()
                newW[r][c]<-'#'
                yield newW
    }
    
let hasLoop world =
    next world (fst startpos, snd startpos, 0, 0)
    |> fun (_,_,_,x)-> x=15000
    
obstacledWorlds world
|> Seq.filter hasLoop
|> Seq.length
|> printfn "Part2: %A"

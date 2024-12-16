module Dec14

open StringUtils

type RowCol = {row:int64; col:int64}

type Particle = {pos:RowCol; v:RowCol}

let parseLine (line:string) =
    let arr = line |> split " "
    let valuesP = arr[0].Substring(2) |> split ","
    let valuesV = arr[1].Substring(2) |> split ","
    let particle = {pos = {col=int64 valuesP[0]; row= int64 valuesP[1]}
                    v =  {col=int64 valuesV[0]; row= int64 valuesV[1]}}
    particle
let parseText text =
    let lines = text |> split "\r\n"
    lines
    |> Array.map parseLine

let modMul (m:int64) (a:int64) (b:int64) =
    ((a % m) * (b % m)) % m

let modAdd (m:int64) (a:int64) (b:int64) =
    let c = ((a % m) + (b % m)) % m
    if c<0L then
        m+c
    else c

let calc1 text rows cols times =
    let s = parseText text
    s
    |> Seq.map (fun p->
        let r = modAdd rows p.pos.row (modMul rows p.v.row times)
        let c = modAdd cols p.pos.col (modMul cols p.v.col times)
        let finalP = {row = r; col = c}
        printfn "%A" finalP
        finalP
        )
    |> Seq.map (fun p->
        let r=p.row
        let c=p.col
        let centerRow = rows / 2L
        let centerCol = cols / 2L
        let q =
            if r=centerRow || c=centerCol then
                0
            else
                if r<centerRow then
                    if c<centerCol then 1 else 2
                else
                    if c>centerCol then 3 else 4
        q
        )
    |> Seq.countBy id
    |> Seq.filter (fun (a,_)-> a<>0)
    |> Seq.map snd
    |> Array.ofSeq
    |> (fun arr-> arr[0] * arr[1] * arr[2] * arr[3])
let ex = @"p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"

let calc1ex =
    calc1 ex 7 11 100
    |> (printfn "%A")
    

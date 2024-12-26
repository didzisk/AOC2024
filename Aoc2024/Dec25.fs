module Dec25

open System.IO
open StringUtils

let day = 25
//MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

let rec parse81 (lines:string array,keys:int array list,locks:int array list)=
    if lines.Length < 7 then
        keys, locks
    else
        let arr =
            lines
            |> Array.take 7
        let isLock = arr[0]="#####"
        let code = Array.create 5 0
        for c = 0 to 4 do
            for r = 1 to arr.Length-2 do
                if arr[r][c]='#' then
                    code[c]<-code[c]+1
        let newLines = lines |> Array.skip 8
        let newKeys, newLocks =
            if isLock then
                keys, (code :: locks)
            else
                (code :: keys), locks
        parse81 (newLines, newKeys, newLocks)
    
let parseInput (text:string) =
    let lines = text.Split("\n") |> Array.map trim
    let keys, locks = parse81 (lines,[],[])
    keys, locks

let keyFitsLock1 (key:int array) (lock:int array)=
    let arr = Array.zip key lock
    arr |> Array.forall (fun (k,l)-> k+l < 6)
    
let calc1 text =
    let keys,locks = parseInput text
    seq {
    for l in locks do
        for k in keys do
            if keyFitsLock1 k l then
                yield 1
    }
    |> Seq.length
    
    
let Del1() = 
    calc1 inputString |> printfn "Part 1: %d"
let ex = @"#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####
"

let Del1ex () =
    calc1 ex |> printfn "Part 1 ex: %d"
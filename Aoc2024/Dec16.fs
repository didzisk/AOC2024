module Dec16

open System
open System.IO
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
    world
    
let Del1 () =
    parseInput inputString
    
  
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
//let Q = SortedDeque() 
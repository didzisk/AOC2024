module Dec24

open System.IO

let day = 24
MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

// let parseInput (text:string) =
//     let top = text |>  
//     let conn =
//         text
//         |> split "\r\n"
//         |> Array.map (fun x ->
//             let arr = split "-" x |> Array.sort
//             arr[0], arr[1])

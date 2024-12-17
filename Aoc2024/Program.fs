open System.IO

let day = 14

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

// Dec14.calc1ex
//
// Dec14.calc1 inputString 103 101 100 |> (printfn "%A")
// Dec14.calc1 "p=2,4 v=2,-3" 7 11 1
//     |> Seq.iter ignore// (printfn "%A")
//
// Dec14.calc1 "p=2,4 v=2,-3" 7 11 2
//     |> Seq.iter ignore// (printfn "%A")

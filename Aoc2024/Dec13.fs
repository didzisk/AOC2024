module Dec13

open System.IO

let day = 13

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day
let inputString = File.ReadAllText filename



open System.IO
open StringUtils
open MathNet.Numerics.LinearAlgebra
let day = 8

//MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day
let text =
    File.ReadAllLines filename
    |> Array.map(_.ToCharArray())
    

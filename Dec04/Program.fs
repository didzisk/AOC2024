open System.IO
open Microsoft.FSharp.Collections

let day = 4

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day
let text =
    File.ReadAllLines filename
    |> Array.map(_.ToCharArray())

type ScanState = {Count:int; Current:string}

let testCandidate state candidate =
    match candidate with
    | "XMAS" -> Some {Current = ""; Count = state.Count+1 }
    | _ when "XMAS".StartsWith(candidate) -> Some {state with Current = candidate}
    | _ -> None
        
let folder state a =
    let candidate = state.Current + new string([|a|])
    let s1 = testCandidate state candidate
    match s1 with
    | Some goodState -> goodState
    | None ->
        let s2 = testCandidate state (new string([|a|]))
        match s2 with
        | Some goodState -> goodState
        | None -> {state with Current = ""}

let scanOne lineNum line  =
    line
    |> Seq.fold folder {Count = 0; Current = ""}
    
let reverseEachLine arr =
    arr |> Array.map (Array.rev) 
    
let diagonals1 (arr:char array array) = 
    let maxCol = Array.length arr.[0]-1
    [|
        for startCol = maxCol downto 0 do
            yield
                [|
                for col = startCol to maxCol do
                    let row = col - startCol
                    yield arr[row][col]
                |]
        for startRow = 1 to maxCol do
            yield
                [|
                for col = 0 to maxCol-startRow do
                    let row = startRow+col
                    yield arr[row][col]
                |]
    |]
    
let diagonals2 =
    reverseEachLine
    >> diagonals1
    
let diagonals3 =
    diagonals1
    >> reverseEachLine

let diagonals4 = 
    Array.transpose
    >> reverseEachLine
    >> diagonals1
    
let states text =
    [|
        yield! text
        yield! reverseEachLine text
        yield! Array.transpose text
        yield! Array.transpose text |> reverseEachLine
        yield! diagonals1 text
        yield! diagonals2 text
        yield! diagonals3 text
        yield! diagonals4 text
    |]
    |> Array.mapi scanOne

let calc1 text =
    states text
        |> Array.map (_.Count)
        |> Array.sum
        |> (printfn "%A")

calc1 text

let hasCrossMas (arr:char array array) row col =
    let nw = ArrayUtils.NW arr row col
    let ne = ArrayUtils.NE arr row col
    let sw = ArrayUtils.SW arr row col
    let se = ArrayUtils.SE arr row col
    let c = arr[row][col]
    let d1 = new string([|nw;c;se|])
    let d2 = new string([|ne;c;sw|])
    if (d1="MAS" || d1 = "SAM") && (d2="MAS" || d2 = "SAM") then 1 else 0

let calc2 (arr:char array array) =
    let indices = [1..arr[0].Length-2]
    List.allPairs indices indices
    |> List.sumBy (fun (row, col) -> hasCrossMas arr row col)
    |> (printfn "%A")

calc2 text

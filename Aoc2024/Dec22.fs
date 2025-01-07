module Dec22

open System
open System.Collections.Generic
open System.IO

let day = 22
//MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

let parseInput (text:string) =
    let lines = text.Split("\n", StringSplitOptions.RemoveEmptyEntries + StringSplitOptions.TrimEntries)
    lines
    |> Array.map int64

let prune (a:int64) =
    a &&& 0xFFFFFFL

let mul64mixprune (a:int64) =
    (a <<< 6) ^^^ a |> prune
    
let div32mixprune (a:int64) =
    (a >>> 5) ^^^ a |> prune
    
let mul2048mixprune (a:int64) =
    (a <<< 11) ^^^ a |> prune
    
let nextSecret (a:int64) =
    a
    |> mul64mixprune
    |> div32mixprune
    |> mul2048mixprune

let gen (a:int64,i:int) =
    if i = 2001 then
            None
        else
            let next = nextSecret a 
            Some (a, (next,i+1))
            
let calcOneList (a:int64) =
    Array.unfold gen (a,0)

let Calc1 text =
    parseInput text
    |> Seq.map calcOneList
    |> Seq.map Seq.last
    |> Seq.sum
    |> (printfn "%d")

let Del1 () =
    Calc1 inputString

[<CustomEquality; NoComparison>]
type theKey =
    | K of (int*int*int*int)
    override this.Equals(obj) =
        match this with
        | K (a1,b1,c1,d1) ->
            match obj with
            | :? theKey as K (a,b,c,d) -> a1=a && b1=b && c1=c && d1 = d
            | _ -> false
        
    override this.GetHashCode() =
        match this with
        | K (a,b,c,d) -> a*1_000+b*100+c*10+d
    
let getLastDigit a=
    a % 10L |> int
    
let convertOneList (scores:Dictionary<theKey,int>) arrLength idx (a:int64 array) =
    let arr = a |> Array.map getLastDigit
    printfn $"%d{idx}/%d{arrLength-1}"
    let seen  = HashSet<theKey>()
    for i in 4..arr.Length-1 do
        let key = K (arr[i-3]-arr[i-4], arr[i-2]-arr[i-3], arr[i-1]-arr[i-2], arr[i] - arr[i-1])
        if seen.Add(key) then
            let score = arr[i]
            if scores.ContainsKey(key) then
                scores[key]<-scores[key]+score
            else
                scores.Add(key,score)
    
let Calc2 text =
    let scores = Dictionary<theKey,int>()

    let arr = 
        parseInput text
        |> Array.map calcOneList
    arr |> Seq.iteri (convertOneList scores arr.Length)

    scores
    |> Seq.map (_.Value)
    |> Seq.max
    |> printfn "Del 2: %d"
    
    scores
    |> Seq.sortByDescending(_.Value)
    |> printfn "%A"
    
    scores
    |> Seq.maxBy(fun kvp-> kvp.Value)
    |> printfn "Del 2: %A"
    
let Del2 () =
    Calc2 inputString
    
let Del2ex() =
    Calc2 @"1
2
3
2024"
    
    
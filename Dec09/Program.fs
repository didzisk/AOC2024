open System.IO

let day = 9

//MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day
let text = File.ReadAllText filename
    
let generateHdd (text:char array) =
    [|
        for i in 0..text.Length/2 do
            let idx = i * 2
            if (idx<text.Length) then
                let c = (int32 text[idx])-48
                for _ in 1..c do 
                    yield i
                if (idx<text.Length-1) then
                    for _ in 1..(int32 text[idx+1])-48 do
                        yield -1
    |]

let visualizeHdd a =
    a
    |> Array.iter (fun a->
        if a>=0 then
            printf $"{a}"
        else
            printf ".")
    printfn ""
    

let defragmentOneItem1 (text:int array) (reverseText:int array) (fileId:int)=
    let target = text |>Array.findIndex (fun x->x = -1)
    let reverseSource =
        reverseText
        |> Array.findIndex (fun x->x = fileId)
    let source = text.Length-1-reverseSource
    if target<source then
        text[target] <- fileId
        text[source] <- -1
        reverseText[reverseSource] <- -1
    
let defragmentOneFile1 (text:int array) (reverseText:int array) (fileId:int)=
    let numIterations =
        text
        |> Array.filter (fun x -> x=fileId)
        |> Array.length
    for _ in 1..numIterations do
        defragmentOneItem1 text reverseText fileId
        
let defrag1 (text:int array) =
    let reverseText = Array.rev text 
    let startId = text |> Array.max
    for id = startId downto 1 do
        defragmentOneFile1 text reverseText id
    
let checksum (text:int array) =
    text
    |> Array.mapi (
        fun i x ->
            if x>0 then
                (int64 i) * (int64 x)
            else
                0
        )
    |> Array.sum
    
let calc1 (text:string) =
    let arr = 
        text
        |> (_.ToCharArray())
        |> generateHdd
    defrag1 arr
    checksum arr |> printfn "%A"
    
let findFreeSpaces (arr:int array) (spaces:int array) (count:int)=
    {spaces[count]..arr.Length-count}
    |> Seq.tryFind (fun i ->
        arr
        |> Array.skip i
        |> Array.take count
        |> Array.forall (fun x-> x < 0)
        )
    
let defragmentOneFile2 (arr:int array) (reverseText:int array) spaces (fileId:int)=
    let count =
        arr
        |> Array.filter (fun x -> x=fileId)
        |> Array.length
    let reverseSource =
        reverseText
        |> Array.findIndex (fun x->x = fileId)
    let source = arr.Length-1-reverseSource
    let targetOpt = findFreeSpaces arr spaces count
    match targetOpt with
    | Some target when target<source ->
        spaces[count] <- target
        for i in 0..count-1 do
            arr[target+i] <- fileId
            arr[source-i] <- -1
    | _ -> ()
    
    

let defrag2 (text:int array) =
    let reverseText = Array.rev text 
    let startId = text |> Array.max
    let spaces = Array.create 10 0
    for id = startId downto 1 do
        printfn $"{id}"
        defragmentOneFile2 text reverseText spaces id
    

let calc2 (text:string) =
    let arr = 
        text
        |> (_.ToCharArray())
        |> generateHdd
    defrag2 arr
    checksum arr |> printfn "%A"
    
calc1 "2333133121414131402"
calc1 text
calc2 "2333133121414131402"
calc2 text
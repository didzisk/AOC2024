module Dec24

open System
open System.Collections.Generic
open System.IO
open StringUtils

let day = 24
//MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

let xorF a b = a<>b
let andF a b = a && b
let orF a b = a || b

type OneCommand = 
    {
        Source1 : string
        Source2 : string
        Command : string
        Target : string
    }

    override this.ToString() = $"{this.Source1} {this.Command} {this.Source2} -> {this.Target}"  

let commandFunc (a:string) =
    match a with
    | "XOR" -> xorF
    | "OR" -> orF
    | "AND" -> andF
    | _ -> failwith "unexpected"
    
let parseOneLine (line:string) =
    let arr = split " " line
    {Source1 = arr[0]; Command = arr[1]; Source2 = arr[2]; Target = arr[4]}

let parseInput (text:string) =
    let lines =
        text.Split "\n"
        |> Array.map _.Trim()
    let top =
        lines
        |> Array.findIndex (fun x-> x="")
    
    let inputs =
        lines
        |> Array.take top
    let state =
        inputs
        |> Seq.map (fun x->
            let arr = split ": " x
            KeyValuePair(arr[0], (arr[1]="1"))
            )
        |> Dictionary
      
    let commands =
        lines
        |> Array.skip (top+1)
        |> Array.filter(fun x->x<>"")
        |> Array.map parseOneLine
    state, commands

let doOneCommand (state:IDictionary<string, bool>) (commands:Queue<OneCommand>) =
    let hasWork, c = commands.TryDequeue()
    if hasWork then
        if state.ContainsKey(c.Source1)
           && state.ContainsKey(c.Source2) then
               let res = commandFunc(c.Command) state[c.Source1] state[c.Source2]
               //printfn $"{c.Source1} {c.Command} {c.Source2} -> {c.Target}"
               state.Add(c.Target, res)
        else
            commands.Enqueue(c)
    hasWork
    
let runMachine state commands =
    while doOneCommand state commands do
        ()
    
    state
    |>Seq.filter _.Key.StartsWith("z")
    |>Seq.map (fun x->
        let shift = int (x.Key.Substring(1))
        if x.Value then
            1UL <<< shift
        else
            0UL
        )
    |> Seq.sum

let stateToInt (state:Dictionary<string, bool>) =
    let mutable x,y = 0UL,0UL
    for i=0 to 44 do
        let codex = sprintf $"x%02d{i}"
        let codey = sprintf $"y%02d{i}"
        if state[codex] then
            x<-x+(1UL <<< i)
        if state[codey] then
            y<-y+(1UL <<< i)
    x,y

let Calc1 text =
    let (state, commands) = parseInput text
    
    let x,y = stateToInt state
    
    runMachine state (Queue commands)
    |> (printfn "x=%d, y=%d, result1 = %d" x y) 

let intToState (state:Dictionary<string, bool>) (a:uint64) (prefix:string) =
    for i=0 to 44 do
        let code = sprintf $"{prefix}%02d{i}"
        state.Add(code, (a &&& (1UL <<< i)) > 0UL)
        
    
let Del1() =
    Calc1 inputString
    
let testOneBit (commands : OneCommand array) n =
    for a in 0UL..1UL do
        for b in 0UL..1UL do
            let state = Dictionary<string, bool>()
            let x = a <<< n
            let y = b <<< n
            intToState state x "x"
            intToState state y "y"
            let z = runMachine state (Queue commands)
            if z<> (x+y) then
                z |> (printfn "bit nr. %d: %d+%d = %d " n x y)
            
let CalcTest2 text =
    let (_, commands) = parseInput text
    for n=0 to 44 do
    testOneBit commands n
    
let Test2 () =
    CalcTest2 inputString
    
let Test1 () = 
    let (_, commands) = parseInput inputString
    // let x=21547963238425UL
    // let y=22045422764601UL
    // let z=43559017878162UL 
    // let x=1547963238425UL
    // let y=2045422764601UL
    // let z=3559017878162UL 
    // let x=1500000000000UL
    // let y=2000000000000UL
    // let z=3559017878162UL 
    let x=1UL
    let y=0UL
    let z=3559017878162UL 
    let state = Dictionary<string, bool>()
    intToState state x "x"
    intToState state y "y"
    runMachine state (Queue commands)
    |> (printfn "%d+%d = %d" x y)
    printfn "expected : x=21547963238425, y=22045422764601, result1 = 43559017878162"
    
let hasCodesAndCommand (c:string) (a:string) (b:string) (x:OneCommand) =
    x.Command = c && ((x.Source1 = a && x.Source2 = b)
                   || (x.Source1 = b && x.Source2 = a))
    
let hasTarget (a:string) (x:OneCommand) =
    x.Target = a
    
let walkOneCommand (s:OneCommand array) (oldC:string) (n:int)=
// One bit looks like this:
// x02 XOR y02 -> vjh  ThisX xor thisY -> thisA
// vjh XOR rpn -> z02	ThisA xor OldC ->  ThisZ 
// y02 AND x02 -> hfd	ThisX and ThisY -> ThisB
// rpn AND vjh -> bmn  OldC  and ThisA -> ThisC
// hfd OR bmn -> qpp	ThisB or  ThisC -> OutC
    let codex = sprintf $"x%02d{n}"
    let codey = sprintf $"y%02d{n}"
    let codez = sprintf $"z%02d{n}"
    
    let savedCodes = Dictionary<string,string>()
    
    let AcmdCandidate = s |> Array.find (hasCodesAndCommand "XOR" codex codey)
    let codeAcandidate = AcmdCandidate.Target //this might be wrong, but the find will always succeed

    let optZcmd = s |> Array.tryFind (hasCodesAndCommand "XOR" codeAcandidate oldC)
    //if we didn't find it, then codeA wasn't correct. Now find correct Z command. 
    let ZcmdCandidate = 
        match optZcmd with
        | Some z -> z
        | None -> s |> Array.find (hasTarget codez) //if A was bad, then find by target. This will contain correct A then.
    let codeA =
        if ZcmdCandidate.Source1 = oldC then
            ZcmdCandidate.Source2
        else
            ZcmdCandidate.Source1
    if codeAcandidate <> codeA then
        savedCodes.Add(codeA, codeAcandidate)
    if ZcmdCandidate.Target <> codez then
        savedCodes.Add(codez, ZcmdCandidate.Target)
    let Acmd = {AcmdCandidate with Target = codeA}
    let Zcmd = {Source1 = oldC; Source2 = codeA; Command = "XOR"; Target = codez}
    
    let BcmdCandidate = s |> Array.find (hasCodesAndCommand "AND" codex codey) //always succeeds
    let codeBcandidate = BcmdCandidate.Target
    let codeB =
        if savedCodes.ContainsKey(codeBcandidate) then
            savedCodes[codeBcandidate]
        else
            codeBcandidate
    let Bcmd = {BcmdCandidate with Target = codeB}
    
    let CcmdCandidate = s |> Array.find (hasCodesAndCommand "AND" codeA oldC) //we have two correct codes, always succeeds
    let codeCcandidate = CcmdCandidate.Target
    let codeC =
        if savedCodes.ContainsKey(codeCcandidate) then
            savedCodes[codeCcandidate]
        else
            codeCcandidate
    let Ccmd = {CcmdCandidate with Target = codeC}

    let outCcmdCandidate = s |> Array.find (hasCodesAndCommand "OR" codeB codeC)
    let outCcode =
        if savedCodes.ContainsKey(outCcmdCandidate.Target) then
            savedCodes[outCcmdCandidate.Target]
        else
            outCcmdCandidate.Target
            
    let outCcmd = {outCcmdCandidate with Target = outCcode}
    
    let changePair =
        if savedCodes.Count >0 then
            savedCodes |> Seq.head |> (fun kvp -> Some (kvp.Key, kvp.Value))
        else
            None

    Acmd, Zcmd, Bcmd, Ccmd, outCcmd, changePair
let walkCommands (s:OneCommand array) =
    let changes = List<string>()
    let newCommands =
        [
            yield parseOneLine "x00 XOR y00 -> z00"
            let OldC00 = s |> Array.find (fun x -> x.Command = "AND" &&
                                                   ((x.Source1 = "x00" && x.Source2 = "y00")
                                                    || (x.Source1 = "y00" && x.Source2 = "x00")))
            yield OldC00
            let mutable oldC = OldC00.Target
            for n in 1..44 do
                let Acmd, Zcmd, Bcmd, Ccmd, outCcmd, changePair = walkOneCommand s oldC n
                oldC <- outCcmd.Target
                yield Acmd
                yield Zcmd
                yield Bcmd
                yield Ccmd
                yield outCcmd
                match changePair with
                | Some (a,b) ->
                    changes.Add a
                    changes.Add b
                | None -> ()
         ]
    newCommands, changes

let Del2() =
    let (_, commands) = parseInput inputString
    let newCommands, changes = walkCommands commands
    let s2 = changes |> Array.ofSeq |> Array.sort  
    let solution2 = String.Join(",",  s2)

    newCommands|> Seq.iter (fun x->printfn "%s" (x.ToString()))
    solution2 |> printfn "Part2: %s"
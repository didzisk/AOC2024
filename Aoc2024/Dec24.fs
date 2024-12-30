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

type OneCommand = {
    Source1 : string
    Source2 : string
    Command : bool -> bool -> bool
    Target : string
}

let parseOneCommand (line:string) =
    let arr = split " " line
    let command =
        match arr[1] with
        | "XOR" -> xorF
        | "OR" -> orF
        | "AND" -> andF
        | _ -> failwith "unexpected"
    {Source1 = arr[0]; Command = command; Source2 = arr[2]; Target = arr[4]}

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
        |> Array.map parseOneCommand
        |> Queue
    state, commands

let doOneCommand (state:IDictionary<string, bool>) (commands:Queue<OneCommand>) =
    let hasWork, c = commands.TryDequeue()
    if hasWork then
        if state.ContainsKey(c.Source1)
           && state.ContainsKey(c.Source2) then
               let res = c.Command state[c.Source1] state[c.Source2]
               state.Add(c.Target, res)
        else
            commands.Enqueue(c)
    hasWork
    
let Calc1 text =
    let (state, commands) = parseInput text
    while doOneCommand state commands do
        ()
    
    state
    |>Seq.filter _.Key.StartsWith("z")
    |>Seq.map (fun x->
        let shift = int (x.Key.Substring(1))
        if x.Value then
            1L <<< shift
        else
            0
        )
    |> Seq.sum
    |> (printfn "%A")

let Del1() =
    Calc1 inputString
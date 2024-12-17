module Dec17

open System.IO
open StringUtils

let day = 17

//MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

type MachineState =
    {
        A : int
        B : int
        C : int
        PC: int //Program Counter
        //Except for jump instructions, the instruction pointer increases by 2 after each instruction
        //is processed (to move past the instruction's opcode and its operand)
        M : byte array        
    }
    
let extractRegValue line =
    line |> split ":" |> Array.last |> trim |> int
    
let parseInput text =
    let arr = text |> split "\r\n"
    let a = extractRegValue arr[0] 
    let b = extractRegValue arr[1] 
    let c = extractRegValue arr[2]
    let chars = arr[4] |> split "," |> Array.map byte
    {A=a;B=b;C=c;PC=0;M=chars}
    
inputString |> parseInput |> printfn "%A"
    
    
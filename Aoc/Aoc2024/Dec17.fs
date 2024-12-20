module Dec17

open System
open System.IO
open StringUtils

let day = 17

//MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

type MachineState =
    {
        A : int64
        B : int64
        C : int64
        PC: int //Program Counter
        //Except for jump instructions, the instruction pointer increases by 2 after each instruction
        //is processed (to move past the instruction's opcode and its operand)
        M : int64 array        
    }
    
let extractRegValue line =
    line |> split ":" |> Array.last |> trim |> int64
    
let parseInput text =
    let arr = text |> split "\r\n"
    let a = extractRegValue arr[0] 
    let b = extractRegValue arr[1] 
    let c = extractRegValue arr[2]
    let chars = arr[3]
                |> split " ," |> Array.skip 1 |> Array.map int64
    {A=int64 a;B=b;C=c;PC=0;M=chars}

let calcNext (s:MachineState, outp:int list) =
    
(*Combo operands 0 through 3 represent literal values 0 through 3.
Combo operand 4 represents the value of register A.
Combo operand 5 represents the value of register B.
Combo operand 6 represents the value of register C.
Combo operand 7 is reserved and will not appear in valid programs.*)
    let literal =s.M[s.PC+1]
    let combo lit =
        match lit with
        | 0L | 1L | 2L | 3L -> int64 literal
        | 4L -> s.A
        | 5L -> s.B
        | 6L -> s.C
        | _ -> failwith "unexpected" //uint 0 //7 is reserved
    let newState =
        match s.M[s.PC] with
        (*The adv instruction (opcode 0) performs division. The numerator is the value in the A register. 
        The denominator is found by raising 2 to the power of the instruction's combo operand. 
        (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.)
        The result of the division operation is truncated to an integer and then written to the A register.*)
        | 0L -> {s with A = s.A >>> int (combo literal); PC = s.PC+2}
        (*The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's literal operand,
        then stores the result in register B.*)
        | 1L -> {s with B = s.B ^^^ (int64 literal); PC = s.PC+2}
        (*The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to the B register.*)
        | 2L -> {s with B = (combo literal) &&& 7L; PC = s.PC+2}
        (*The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A register is not zero, it jumps by setting the instruction pointer
         to the value of its literal operand; if this instruction jumps, the instruction pointer is not increased by 2 after this instruction.*)
        | 3L when s.A = 0L -> {s with PC = s.PC+2}
        | 3L when s.A <>0L -> {s with PC = int literal}
        (*The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C,
        then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)*)
        | 4L -> {s with B = s.B ^^^ s.C; PC = s.PC+2}
        (*The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value.
        (If a program outputs multiple values, they are separated by commas.)*)
        | 5L -> {s with PC = s.PC+2}
        (*The bdv instruction (opcode 6) works exactly like the adv instruction
        except that the result is stored in the B register.
        (The numerator is still read from the A register.)*)
        | 6L -> {s with B = s.A >>> int (combo literal); PC = s.PC+2}
        (*The cdv instruction (opcode 7) works exactly like the adv instruction
        except that the result is stored in the C register.*)
        | 7L -> {s with C = s.A >>> int (combo literal); PC = s.PC+2}

        | _ -> failwith $"unexpected {s.M[s.PC]} in position {s.PC}"
    let newOut = 
        if s.M[s.PC] = 5L then
            let outVal = ((combo literal) &&& 7L)
            outp @ [(int outVal)]  
        else
            outp
    newState, newOut
    
let rec next (s:MachineState, outp:int list) =
    let newState, newOutp = calcNext (s, outp)
    if newState.PC >= s.M.Length then
        newState, newOutp
    else
        next (newState, newOutp)

    
let calc1 text =
    ((text |> parseInput) , [])
    |> next

let ex1 = @"Register A: 0
Register B: 0
Register C: 9

Program: 2,6"
    
let test1 ()=
    let s, outp = ex1 |> calc1
    printfn $"B={s.B} {outp}"
    
let ex2 = @"Register A: 10
Register B: 0
Register C: 0

Program: 5,0,5,1,5,4"
let test2 ()= calc1 ex2

let ex3 = @"Register A: 2024
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"

let test3 ()= calc1 ex3

let ex4 = @"Register A: 0
Register B: 29
Register C: 0

Program: 1,7"
let test4 () =
    let s, outp= calc1 ex4
    printfn $"B={s.B}"

let ex5 = @"Register A: 0
Register B: 2024
Register C: 43690

Program: 4,0"
let test5 () =
    let s, outp= calc1 ex5
    printfn $"B={s.B}"


let ex6 = @"Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"

let test6 ()= calc1 ex6

let ex7 = @"Register A: 12345678
Register B: 0
Register C: 0

Program: 2,4,1,0,7,5,1,5,0,3,4,5,5,5,3,0"

let test7 ()= calc1 ex7

let ex8 = @"Register A: 12345678
Register B: 0
Register C: 0

Program: 2,4,1,3,7,5,0,3,1,4,4,4,5,5,3,0"
let test8 ()=
    let s,outp= calc1 ex8
    printfn $"%A{outp}"


let del1() =
    calc1 inputString
    |> snd
    |> List.map Convert.ToString
    |> (StringUtils.join ',')
    |> printfn "Part 1: %s"

let rec translated (A: int) =
    let mutable b = (A &&& 7) ^^^ 2 //2,4 //if second to last bit is set then 0 else 2
    let c = A >>> b //7,5  //either move or not 2 positions
    b <- b ^^^ 7 ^^^ c//1,7 //4,1 revert bits of B, xor with bits of c
    printf "%A," (b &&& 7)    //5,5 - out B last 3 bits

    let a = A >>> 3 //0,3  //(cut last 3 bits for next loop) 
    if a = 0 then () else translated a //3,0 - if A isn't zero, do this again
    
let calcB (A: int) =
    let b = (A &&& 7) ^^^ 2 //2,4 //if second to last bit is set then 0 else 2
    let c = A >>> b //7,5  //either move or not 2 positions
    (b &&& 7) ^^^ 7 ^^^ c //1,7 //4,1 revert bits of B, xor with bits of c


let rec translated1 (A: int64) =
    let b = calcB ((A &&& 31L) |> int)
    printf "%A," (b &&& 7)    //5,5 - out B last 3 bits

    let a = A >>> 3 //0,3  //(cut last 3 bits for next loop) 
    if a = 0 then () else translated1 a //3,0 - if A isn't zero, do this again
    
let del1Translated () =
    let s = inputString |> parseInput
    translated (int s.A)
    
let del1Translated1 () =
    let s = inputString |> parseInput
    translated1 (int s.A)

let rec fitsPos (arr:int64 array) (i:int) (cand:int) =
    let fits = (calcB cand) = int arr[i] &&
               ((i=arr.Length-1) || ([0..31] |> List.exists (fun x-> fitsPos arr (i+1) x)))
    if fits then
        printfn "%d %d" i cand
    fits
    

let fitsHere (s:MachineState) (i:int) (candidate:int64)=
    let _, outp = next ({s with A = s.A * 8L + candidate}, [])
    let expected = int s.M[s.M.Length-1-i]
    let actual = outp |> List.head
    let accept = expected = actual
    accept
    
    
let processOneLevel (s:MachineState) (goodStarts:int64 list) (i:int) =
    [
        for start in goodStarts do
            for c in 0L..7L do
                if fitsHere {s with A=start} i c then
                    yield start*8L+c            
    ]
    
let allRoutes (s:MachineState) =
    let folder = (processOneLevel s)
    [0..15] |> List.fold folder [0L]
    
let del2 () =
    let s = inputString |> parseInput
    let best =
        allRoutes s
        |> List.min
    Convert.ToString(best)   |> printfn "Part 2: %s"
    Convert.ToString(best,8) |> printfn "Octal: %s"

// Run it by doing this:
// Dec17.del1 () 
// Dec17.del2 () 

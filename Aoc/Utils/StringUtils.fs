module StringUtils

open System

let reverseString (x:string) =
    let charArray = x.ToCharArray()
    Array.Reverse charArray
    let s = new string(charArray)
    s

let split (chars: string) (s: string) = 
    s.Split (Seq.toArray chars, StringSplitOptions.RemoveEmptyEntries)
    
let c2i =
    System.Globalization.CharUnicodeInfo.GetDigitValue
    
let c2im (c:char) =
    match c with
    | x when x>='0' && x<='9' ->  System.Globalization.CharUnicodeInfo.GetDigitValue x
    | _ -> -1
    
let trim (s:string) =
    s.Trim()
    
let join (c:char) (s: string seq) =
    String.Join(c, s)
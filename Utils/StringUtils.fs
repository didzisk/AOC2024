module StringUtils

open System

let reverseString (x:string) =
    let charArray = x.ToCharArray()
    Array.Reverse charArray
    let s = new string(charArray)
    s

let split (chars: string) (s: string) = 
    s.Split (Seq.toArray chars, StringSplitOptions.RemoveEmptyEntries)
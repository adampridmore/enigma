module Helpers

let toUpper (s:string) = s.ToUpper()
let toCharArray (s:string) = s.ToCharArray()
let charToString (c:char) = c.ToString()
let split (s:string) = s.Split([|System.Environment.NewLine;"\n";"\r\n"|], System.StringSplitOptions.RemoveEmptyEntries)
    
let indexOf (toFind:char) (text:string) = 
    match text.IndexOf(toFind) with
    | -1 -> failwith (sprintf "Unknown char '%c'" toFind)
    | i -> i

let charFromString (text:string) index = text.[index]

let modular x m = 
    match x % m with
    | x when x < 0 -> x + m
    | _ -> x

let p v = v |> printfn "%A";v 

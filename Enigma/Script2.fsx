let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotor1Mapping = "BCDEFGHIJKLMNOPQRSTUVWXYZA"
let numberOfLetters = alphabet.Length

let toUpper (s:string) = s.ToUpper()
let toCharArray (s:string) = s.ToCharArray()
let charToString (c:char) = c.ToString()
let split (s:string) = s.Split([|System.Environment.NewLine;"\n";"\r\n"|], System.StringSplitOptions.RemoveEmptyEntries)
    
let indexOf (toFind:char) (text:string) = 
    match text.IndexOf(toFind) with
    | -1 -> failwith (sprintf "Unknown char '%c'" toFind)
    | i -> i

let charFromString (text:string) index = 
    let s = text.Substring(index, 1)
    s.Chars(0)

let subString (text:string) index =
    text.Substring(index, 1)
    


let rotorB1 (l:char) =
    alphabet 
    |> indexOf l 
    |> charFromString rotor1Mapping

let rotorB1Reverse (l:char) =
    rotor1Mapping
    |> indexOf l 
    |> charFromString alphabet

let reflector (l:char) = 
    ((alphabet |> indexOf l) + (numberOfLetters / 2)) % numberOfLetters 
    |> charFromString alphabet

let cipherChar = rotorB1 >> reflector >> rotorB1Reverse
// let cipherString text = text
    
alphabet.ToCharArray() 
|> Seq.map (fun c -> c |> rotorB1 |> reflector |> rotorB1Reverse)
|> Seq.iter (printfn "%c")

'B' |> rotorB1 |> reflector |> rotorB1Reverse

"Hello"
|> toUpper
|> toCharArray
|> Seq.map (fun c -> c |> rotorB1 |> reflector |> rotorB1Reverse)
|> Seq.map charToString
|> Seq.reduce (+)

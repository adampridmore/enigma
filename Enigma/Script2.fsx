let alphabet =      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotor1Mapping = "BCDEFGHIJKLMNOPQRSTUVWXYZA"
//                   00000000001111111111222222
//                   01234567890123456789012345
let numberOfLetters = alphabet.Length

let toUpper (s:string) = s.ToUpper()
let toCharArray (s:string) = s.ToCharArray()
let charToString (c:char) = c.ToString()
let split (s:string) = s.Split([|System.Environment.NewLine;"\n";"\r\n"|], System.StringSplitOptions.RemoveEmptyEntries)
    
let indexOf (toFind:char) (text:string) = 
    match text.IndexOf(toFind) with
    | -1 -> failwith (sprintf "Unknown char '%c'" toFind)
    | i -> i

let charFromString (text:string) index = text.[index]

let addAndMod a b = (a + b) % numberOfLetters

let modular x m = 
    match x % m with
    | x when x < 0 -> x + m
    | _ -> x

let rotori i fromMapping toMapping (l:char) = 
    let mappingIndex = fromMapping |> indexOf l
    addAndMod mappingIndex i
    |> charFromString toMapping
        
let rotoriB1 i (l:char) = 
    rotori i alphabet rotor1Mapping l

let rotoriB1Reverse i (l:char) = 
    rotori (modular (-i) numberOfLetters) rotor1Mapping alphabet l

let reflector (l:char) = 
    ((alphabet |> indexOf l) + (numberOfLetters / 2)) % numberOfLetters 
    |> charFromString alphabet

let p v = v |> printfn "%A";v 

let cipherChar i = p >> rotoriB1 i >> p >> reflector >> p >> rotoriB1Reverse i >> p
let decipherChar i = rotoriB1Reverse i >> p >> reflector >> p >> rotoriB1 i >> p

let cipherString = 
    toUpper
    >> toCharArray
    >> Seq.mapi (fun i c -> c |> cipherChar i)
    >> Seq.map charToString
    >> Seq.reduce (+)
        
//'B' |> rotorB1 |> reflector |> rotorB1Reverse
//'A' |> cipherChar 0 |> cipherChar 0
'A' |> cipherChar 1 |> cipherChar 1
'A' |> cipherChar 2 |> cipherChar 2
'E' |> cipherChar 2 //|> cipherChar 1

//'P' |> cipherChar 1 
//'P' |> rotoriB1 1 |> reflector |> rotoriB1Reverse 1
//'A' |> rotoriB1 1
//'L' |> cipherChar 2 |> cipherChar 2
//'L' |> cipherChar 3 |> cipherChar 3
//'O' |> cipherChar 4 |> cipherChar 4
//'O' |> rotori 1 alphabet rotor1Mapping

"Hello" |> cipherString |> cipherString 

//'A' |> rotori 2 alphabet rotor1Mapping

modular -8 20

let negativeMod x m = 
    match (-x) % numberOfLetters with
    | x when x < 0 -> x + m
    | _ -> x

negativeMod 8 20


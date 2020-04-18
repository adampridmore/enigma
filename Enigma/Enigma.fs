module EnigmaCipher

open Helpers

let alphabet =      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotor1Mapping = "BCDEFGHIJKLMNOPQRSTUVWXYZA"
//                   00000000001111111111222222
//                   01234567890123456789012345
let numberOfLetters = alphabet.Length

let addAndMod a b = (a + b) % numberOfLetters

let rotori i fromMapping toMapping (l:char) = 
    let mappingIndex = fromMapping |> indexOf l
    //modular (mappingIndex+i) % numberOfLetters
//    modular (mappingIndex + i) numberOfLetters
    addAndMod mappingIndex i
    |> charFromString toMapping
        
let rotoriB1 i (l:char) = 
    rotori i alphabet rotor1Mapping l

let rotoriB1Reverse i (l:char) = 
    rotori (modular (-i) numberOfLetters) rotor1Mapping alphabet l

let reflector (l:char) = 
    ((alphabet |> indexOf l) + (numberOfLetters / 2)) % numberOfLetters 
    |> charFromString alphabet

let cipherChar i = p >> rotoriB1 i >> p >> reflector >> p >> rotoriB1Reverse i >> p
let decipherChar i = rotoriB1Reverse i >> p >> reflector >> p >> rotoriB1 i >> p

let cipherString = 
    toUpper
    >> toCharArray
    >> Seq.mapi (fun i c -> c |> cipherChar i)
    >> Seq.map charToString
    >> Seq.reduce (+)

let numberOfLetters = 4 

let rotor1 position l =
    (position + l) % numberOfLetters
    |>  function 
        | 0 -> 1
        | 1 -> 2
        | 2 -> 3
        | 3 -> 0
        | _ -> failwith (sprintf "Letter outside range: %d" l)

let reflector l = (l + (numberOfLetters/2) ) % numberOfLetters
let plaintext = [2;1;3;1]
let plaintext2 = seq{for i = 1 to 100 do yield 1} |> Seq.toList

let encrypt = 
    Seq.mapi (fun i letter -> letter |> rotor1 i |> reflector |> rotor1Reverse i) 
    >> Seq.toList

//let cipherText = plaintext |> encrypt |> encrypt
let cipherText = plaintext2 |> encrypt 




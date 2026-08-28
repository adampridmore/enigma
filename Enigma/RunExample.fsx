// Example script demonstrating the real Enigma I cipher implementation.
// Run with: dotnet fsi Enigma/RunExample.fsx

#load "Helpers.fs"
#load "Enigma.fs"

open EnigmaCipher

let config = {
    Rotors = [ rotorI; rotorII; rotorIII ]
    Reflector = ukwB
    // Plugboard = makePlugboard [ "AM"; "FI"; "NV"; "PS"; "TU"; "WZ" ]
    Plugboard = makePlugboard []
    RingSettings = [ 1; 1; 1 ]
    Positions = [ 'A'; 'A'; 'A' ]
}

let plaintext = "ADAMPRIDMORE"
let ciphertext = plaintext |> cipherString config
// Re-run from the same starting positions to decrypt (Enigma is reciprocal).
let decrypted = ciphertext |> cipherString config 

printfn "Plaintext:  %s" plaintext
printfn "Ciphertext: %s" ciphertext
printfn "Decrypted:  %s" decrypted

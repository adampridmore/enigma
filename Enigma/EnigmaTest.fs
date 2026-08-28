module EnigmaCipherTest

open EnigmaCipher
open FsUnit.Xunit
open Xunit

[<Fact>]
let ``Test vector 1 - no plugboard canonical AAAAA`` () =
    let config = {
        Rotors = [rotorI; rotorII; rotorIII]
        Reflector = ukwB
        Plugboard = Map.empty
        RingSettings = [1; 1; 1]
        Positions = ['A'; 'A'; 'A']
    }
    cipherString config "AAAAA" |> should equal "BDZGO"

[<Fact>]
let ``Test vector 1 - symmetry reciprocal BDZGO`` () =
    let config = {
        Rotors = [rotorI; rotorII; rotorIII]
        Reflector = ukwB
        Plugboard = Map.empty
        RingSettings = [1; 1; 1]
        Positions = ['A'; 'A'; 'A']
    }
    cipherString config "BDZGO" |> should equal "AAAAA"

[<Fact>]
let ``Test vector 2 - with plugboard rotors VII V IV`` () =
    let config = {
        Rotors = [rotorVII; rotorV; rotorIV]
        Reflector = ukwB
        Plugboard = makePlugboard ["AD"; "FT"; "WH"; "JO"; "PN"]
        RingSettings = [2; 3; 4]     // mikepound ringSettings={1,2,3} 0-indexed → 1-indexed: 2,3,4
        Positions = ['K'; 'F'; 'M']  // mikepound rotorPositions={10,5,12} 0-indexed → K,F,M
    }
    cipherString config "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> should equal "UJFZBOKXBAQSGCLDNUTSNTASEF"

[<Fact>]
let ``Test vector 3 - default settings ADAMPRIDMORE, cross-checked against 101computing.net emulator`` () =
    let config = {
        Rotors = [rotorI; rotorII; rotorIII]
        Reflector = ukwB
        Plugboard = Map.empty
        RingSettings = [1; 1; 1]
        Positions = ['A'; 'A'; 'A']
    }
    cipherString config "ADAMPRIDMORE" |> should equal "BAZOJTPUOSVT"

module EnigmaCipher

open Helpers

type RotorSpec = { Wiring: string; Notches: char list }

type MachineConfig = {
    Rotors: RotorSpec list        // left → right, exactly 3
    Reflector: string             // 26-char substitution string
    Plugboard: Map<char, char>
    RingSettings: int list        // 1-indexed (1 = A), left → right
    Positions: char list          // current rotor positions, left → right
}

let rotorI    = { Wiring = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"; Notches = ['Q'] }
let rotorII   = { Wiring = "AJDKSIRUXBLHWTMCQGZNPYFVOE"; Notches = ['E'] }
let rotorIII  = { Wiring = "BDFHJLCPRTXVZNYEIWGAKMUSQO"; Notches = ['V'] }
let rotorIV   = { Wiring = "ESOVPZJAYQUIRHXLNFTGKDCMWB"; Notches = ['J'] }
let rotorV    = { Wiring = "VZBRGITYUPSDNHLXAWMJQOFECK"; Notches = ['Z'] }
let rotorVI   = { Wiring = "JPGVOUMFYQBENHZRDKASXLICTW"; Notches = ['Z'; 'M'] }
let rotorVII  = { Wiring = "NZJHGRCXMYSWBOUFAIVLPEKQDT"; Notches = ['Z'; 'M'] }
let rotorVIII = { Wiring = "FKQHTLXOCBJSPDZRAMEWNIUYGV"; Notches = ['Z'; 'M'] }

// Reflector (Umkehrwalze) wiring — index i maps letter i to its reciprocal pair
let ukwA = "EJMZALYXVBWFCRQUONTSPIKHGD"  // Reflector A
let ukwB = "YRUHQSLDPXNGOKMIEBFZCWVJAT"  // Reflector B (most common)
let ukwC = "FVPJIAOYEDRZXWGCTKUQSBNMHL"  // Reflector C

let makePlugboard (pairs: string list) : Map<char, char> =
    pairs
    |> List.collect (fun pair -> [(pair.[0], pair.[1]); (pair.[1], pair.[0])])
    |> Map.ofList

let private plugSwap (plugboard: Map<char, char>) (c: char) =
    match Map.tryFind c plugboard with
    | Some c' -> c'
    | None -> c

let private stepChar (c: char) = char ((int c - int 'A' + 1) % 26 + int 'A')

let private atNotch (rotor: RotorSpec) (pos: char) = List.contains pos rotor.Notches

let private stepPositions (rotors: RotorSpec list) (positions: char list) =
    let left, mid, right = positions.[0], positions.[1], positions.[2]
    let midAtNotch   = atNotch rotors.[1] mid
    let rightAtNotch = atNotch rotors.[2] right
    let newLeft  = if midAtNotch then stepChar left else left
    let newMid   = if midAtNotch || rightAtNotch then stepChar mid else mid
    let newRight = stepChar right
    [newLeft; newMid; newRight]

let private rotorForward (rotor: RotorSpec) (position: char) (ring: int) (c: char) =
    let p = int position - int 'A'
    let r = ring - 1
    let inputIdx  = int c - int 'A'
    let shifted   = (inputIdx + p - r + 26) % 26
    let wiringOut = int rotor.Wiring.[shifted] - int 'A'
    let output    = (wiringOut - p + r + 26) % 26
    char (output + int 'A')

let private rotorReverse (rotor: RotorSpec) (position: char) (ring: int) (c: char) =
    let p = int position - int 'A'
    let r = ring - 1
    let inputIdx = int c - int 'A'
    let shifted  = (inputIdx + p - r + 26) % 26
    let wiringIn = rotor.Wiring.IndexOf(char (shifted + int 'A'))
    let output   = (wiringIn - p + r + 26) % 26
    char (output + int 'A')

let private reflect (reflector: string) (c: char) =
    reflector.[int c - int 'A']

let private encryptChar (config: MachineConfig) (c: char) : MachineConfig * char =
    let newPositions = stepPositions config.Rotors config.Positions
    let cfg   = { config with Positions = newPositions }
    let pos   = cfg.Positions
    let rings = cfg.RingSettings
    let rots  = cfg.Rotors
    let c1 = plugSwap cfg.Plugboard c
    let c2 = rotorForward  rots.[2] pos.[2] rings.[2] c1
    let c3 = rotorForward  rots.[1] pos.[1] rings.[1] c2
    let c4 = rotorForward  rots.[0] pos.[0] rings.[0] c3
    let c5 = reflect cfg.Reflector c4
    let c6 = rotorReverse  rots.[0] pos.[0] rings.[0] c5
    let c7 = rotorReverse  rots.[1] pos.[1] rings.[1] c6
    let c8 = rotorReverse  rots.[2] pos.[2] rings.[2] c7
    let c9 = plugSwap cfg.Plugboard c8
    cfg, c9

let cipherString (config: MachineConfig) (input: string) : string =
    let _, result =
        (toUpper input).ToCharArray()
        |> Array.fold (fun (cfg, acc) c ->
            if c >= 'A' && c <= 'Z' then
                let newCfg, enc = encryptChar cfg c
                newCfg, acc + string enc
            else
                cfg, acc + string c
        ) (config, "")
    result

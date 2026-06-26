# Enigma Real Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the toy cipher in `Enigma.fs` with a correct Wehrmacht/Luftwaffe Enigma I implementation and verify it against two known test vectors.

**Architecture:** Modify `Enigma.fs` in-place to add a `RotorSpec` type and `MachineConfig` record, replace the simplified rotor/reflector logic with real wiring tables and correct stepping (including double-step anomaly), and add a plugboard. `cipherString` becomes a function that takes a `MachineConfig`. Replace all tests in `EnigmaTest.fs` with the three real-world test cases.

**Tech Stack:** F# on .NET 10, xUnit, FsUnit.xUnit

## Global Constraints

- All files in `Enigma/` — do not create new source files
- Module compile order must stay: Helpers.fs → Enigma.fs → EnigmaTest.fs → Library.fs
- `Helpers.fs` is not modified
- `dotnet test` must pass with 0 failures after Task 2

---

### Task 1: Rewrite `Enigma.fs` with real Enigma logic

**Files:**
- Modify: `Enigma/Enigma.fs`

**Interfaces:**
- Produces:
  - `type RotorSpec = { Wiring: string; Notches: char list }`
  - `type MachineConfig = { Rotors: RotorSpec list; Reflector: string; Plugboard: Map<char,char>; RingSettings: int list; Positions: char list }`
  - `rotorI`, `rotorII`, `rotorIII`, `rotorIV`, `rotorV`, `rotorVI`, `rotorVII`, `rotorVIII` : `RotorSpec`
  - `ukwB`, `ukwC` : `string`
  - `makePlugboard : string list -> Map<char,char>`
  - `cipherString : MachineConfig -> string -> string`

- [ ] **Step 1: Replace `Enigma.fs` with the new implementation**

Replace the entire contents of `Enigma/Enigma.fs` with:

```fsharp
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

let ukwB = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
let ukwC = "FVPJIAOYEDRZXWGCTKUQSBNMHL"

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
    let midAtNotch  = atNotch rotors.[1] mid
    let rightAtNotch = atNotch rotors.[2] right
    let newLeft  = if midAtNotch then stepChar left else left
    let newMid   = if midAtNotch || rightAtNotch then stepChar mid else mid
    let newRight = stepChar right
    [newLeft; newMid; newRight]

let private rotorForward (rotor: RotorSpec) (position: char) (ring: int) (c: char) =
    let p = int position - int 'A'
    let r = ring - 1
    let inputIdx = int c - int 'A'
    let shifted  = (inputIdx + p - r + 26) % 26
    let wiringOut = int rotor.Wiring.[shifted] - int 'A'
    let output = (wiringOut - p + r + 26) % 26
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
```

- [ ] **Step 2: Build to verify no compile errors**

Run: `dotnet build /Users/adampridmore/work/scratch/enigma/Enigma/Enigma.fsproj`
Expected: `Build succeeded` (tests will fail until Task 2)

---

### Task 2: Replace tests with real test vectors

**Files:**
- Modify: `Enigma/EnigmaTest.fs`

**Interfaces:**
- Consumes: `MachineConfig`, `rotorI`–`rotorVII`, `ukwB`, `makePlugboard`, `cipherString` from Task 1

- [ ] **Step 1: Replace `EnigmaTest.fs` with real test vectors**

Replace the entire contents of `Enigma/EnigmaTest.fs` with:

```fsharp
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
        RingSettings = [10; 5; 12]
        Positions = ['A'; 'B'; 'C']
    }
    cipherString config "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> should equal "UJFZBOKXBAQSGCLDNUTSNTASEF"
```

- [ ] **Step 2: Run tests**

Run: `dotnet test /Users/adampridmore/work/scratch/enigma/Enigma/Enigma.fsproj`
Expected: `Passed! - Failed: 0, Passed: 3`

- [ ] **Step 3: Commit**

```bash
git add Enigma/Enigma.fs Enigma/EnigmaTest.fs
git commit -m "Implement real Enigma I cipher with rotors, reflector, plugboard and stepping"
```

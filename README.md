# Enigma

An F# implementation of the Wehrmacht/Luftwaffe Enigma I cipher machine, verified against known historical test vectors.

## Features

- Rotors I–VIII with correct wirings and notch positions (VI, VII, VIII have double notches)
- Reflectors UKW-B and UKW-C
- Plugboard (Steckerbrett)
- Correct rotor stepping including the double-step anomaly
- Ring settings (Ringstellung)

## Usage

```fsharp
open EnigmaCipher

let config = {
    Rotors = [rotorI; rotorII; rotorIII]  // left → right
    Reflector = ukwB
    Plugboard = makePlugboard ["AB"; "CD"]  // letter pairs to swap
    RingSettings = [1; 1; 1]               // 1-indexed (1 = A)
    Positions = ['A'; 'A'; 'A']            // starting rotor positions
}

let ciphertext = cipherString config "HELLO"
let plaintext  = cipherString config ciphertext  // Enigma is symmetric
```

`makePlugboard` accepts a list of two-character pair strings. An empty plugboard is `Map.empty`.

Encryption is symmetric: encrypting the ciphertext with the same starting configuration returns the original plaintext.

## Commands

```bash
dotnet build   # Build
dotnet test    # Run tests
dotnet run     # Run entry point
```

Run a single test by name:
```bash
dotnet test --filter "FullyQualifiedName~<test name>"
```

## Test Vectors

| Rotors (L→R) | Reflector | Rings | Positions | Plugboard | Input | Output |
|---|---|---|---|---|---|---|
| I II III | UKW-B | 1 1 1 | A A A | none | `AAAAA` | `BDZGO` |
| VII V IV | UKW-B | 2 3 4 | K F M | AD FT WH JO PN | `ABCDEFGHIJKLMNOPQRSTUVWXYZ` | `UJFZBOKXBAQSGCLDNUTSNTASEF` |

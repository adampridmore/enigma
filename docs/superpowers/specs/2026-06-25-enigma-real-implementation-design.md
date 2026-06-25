# Enigma Real Implementation Design

**Date:** 2026-06-25  
**Scope:** Extend `Enigma.fs` to be fully compatible with the Wehrmacht/Luftwaffe Enigma I machine, verified against known test vectors.

---

## Approach

Modify and extend the existing `Enigma.fs` and `EnigmaTest.fs` in-place (Option A). No new files. Keep `Helpers.fs`, `Library.fs`, and `Program.fs` untouched except removing the `p` debug printer from the cipher pipeline.

---

## Data Model (`Enigma.fs`)

```fsharp
type RotorSpec = { Wiring: string; Notch: char }

type MachineConfig = {
    Rotors: RotorSpec list    // left → right, exactly 3 elements
    Reflector: string         // 26-char substitution string
    Plugboard: Map<char, char>
    RingSettings: int list    // 1-indexed (1 = A), left → right
    Positions: char list      // current rotor positions, left → right
}
```

Built-in rotor constants defined at module level:

| Name   | Wiring                     | Notch |
|--------|----------------------------|-------|
| rotorI | EKMFLGDQVZNTOWYHXUSPAIBRCJ | Q     |
| rotorII | AJDKSIRUXBLHWTMCQGZNPYFVOE | E    |
| rotorIII | BDFHJLCPRTXVZNYEIWGAKMUSQO | V   |
| rotorIV | ESOVPZJAYQUIRHXLNFTGKDCMWB | J    |
| rotorV | VZBRGITYUPSDNHLXAWMJQOFECK | Z     |

Built-in reflector constants:

| Name | Wiring                     |
|------|----------------------------|
| ukwB | YRUHQSLDPXNGOKMIEBFZCWVJAT |
| ukwC | FVPJIAOYEDRZXWGCTKUQSBNMHL |

Helper to build a plugboard `Map<char,char>` from a list of pair strings like `["AD"; "FT"]` — each pair adds both directions (A→D and D→A).

---

## Rotor Stepping

Applied **before** each character is encrypted. Given positions `[left; mid; right]` and notches:

1. If mid is at its notch → both mid and left step (double-step anomaly)
2. Else if right is at its notch → mid steps
3. Right always steps

Step = advance position by 1 letter (Z wraps to A).

---

## Per-Character Encryption

After stepping, for each character:

1. **Plugboard in** — look up char in `Plugboard` map; if present substitute, else pass through
2. **Right → Middle → Left rotor, forward pass** — each rotor applies its wiring with position+ring offset
3. **Reflector** — apply reflector wiring (no stepping)
4. **Left → Middle → Right rotor, reverse pass** — each rotor applies inverse wiring with same offset
5. **Plugboard out** — same substitution as step 1

**Rotor offset formula** (forward, 0-indexed position `p`, 0-indexed ring setting `r`):
- `offset = (p - r + 26) % 26`
- `encryptedIndex = (wiring[( inputIndex + offset + 26) % 26] - 'A' - offset + 26) % 26`

Reverse pass uses the inverse of the wiring (find the index where the wiring contains the target letter).

---

## `cipherString`

```fsharp
// Signature (conceptual)
cipherString : MachineConfig -> string -> string
```

Implemented as a fold over the characters of the uppercased input string. Each step produces `(updatedConfig, encryptedChar)`. Non-alpha characters are passed through unchanged.

The existing `cipherString` (no-argument, implicit state) is replaced by this config-taking version.

---

## Tests (`EnigmaTest.fs`)

All existing tests are replaced with:

### Test 1 — No plugboard (Wikipedia canonical)
- Rotors: I II III | Reflector: UKW-B | Rings: 1 1 1 | Positions: A A A | Plugboard: none
- Input: `"AAAAA"` → Expected: `"BDZGO"`

### Test 2 — With plugboard (mikepound/enigma)
- Rotors: VII V IV | Reflector: UKW-B | Rings: 10 5 12 | Positions: A B C | Plugboard: AD FT WH JO PN
- Input: `"ABCDEFGHIJKLMNOPQRSTUVWXYZ"` → Expected: `"UJFZBOKXBAQSGCLDNUTSNTASEF"`

### Test 3 — Symmetry / reciprocal property
- Same config as Test 1; encrypt the output of Test 1 (`"BDZGO"`) → Expected: `"AAAAA"`

---

## Files Changed

| File | Change |
|------|--------|
| `Enigma/Enigma.fs` | Replace cipher logic; add rotor/reflector constants, `MachineConfig` type, stepping, plugboard, new `cipherString` |
| `Enigma/EnigmaTest.fs` | Replace all tests with the three tests above |
| `Enigma/Helpers.fs` | No change (keep `p` in file but remove from cipher pipeline) |

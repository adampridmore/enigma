# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
dotnet build          # Build the project
dotnet test           # Run all tests
dotnet run            # Run the program entry point
```

Run a single test by name:
```bash
dotnet test --filter "FullyQualifiedName~<test name>"
```

## Architecture

F# implementation of an Enigma-style cipher machine targeting .NET Core 3.1. All source is in `Enigma/`.

**Module load order** (defined in `Enigma.fsproj`):
1. `Helpers.fs` — string utilities, `modular` (handles negative modulo correctly), and `p` (pass-through debug printer via `printfn`)
2. `Enigma.fs` — core cipher logic
3. `EnigmaTest.fs` — xUnit / FsUnit tests
4. `Library.fs` — placeholder stub

**Cipher design** (`Enigma.fs`):
- Each character at position `i` passes through: `rotoriB1 i` → `reflector` → `rotoriB1Reverse i`
- The rotor shifts the alphabet index by `i` using `rotor1Mapping` as a substitution table
- The reflector maps each letter 13 positions forward (ROT13-style on the 26-letter alphabet)
- `cipherString` uppercases input then applies `cipherChar i` per character (where `i` is the character's index) — making it symmetric: `cipherString >> cipherString = id`
- `p` (the debug printer from `Helpers.fs`) is currently active inside the `cipherChar` pipeline, so test runs produce verbose trace output

**Exploratory scripts** (not compiled into the project):
- `Script1.fsx` — V1 prototype using numeric letter indices
- `Script2.fsx` — V2 prototype using chars, which led to the current design

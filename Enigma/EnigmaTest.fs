module EnigmaCipherTest

open EnigmaCipher
open FsUnit.Xunit
open System
open Xunit

[<Fact>]
let ``My test`` () =
    Assert.True(true)

[<Fact>]
let ``Ciper and deciper string``() = 
   
    "Hello" |> cipherString |> cipherString |> should equal "HELLO"

[<Fact>]
let ``Ciper string``() = 
    "Hello" |> cipherString |> should equal "URYYB"

[<Fact>]
let ``Ciper character``() = 
    'A' |> cipherChar 0 |> should equal 'N'
    
[<Fact>]
let ``Ciper character with 1 rotation``() = 
    'A' |> cipherChar 1 |> should equal 'N'
    
[<Fact>]
let ``Ciper character with 10 rotation``() = 
    'A' |> cipherChar 10 |> should equal 'N'


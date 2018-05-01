module Tests

open Xunit
open CodinGame.PhoneNumbers
open System.IO
open Swensen.Unquote

[<Fact>]
let ``Test Case 1: one number`` () =
    let numbers = 
        [
            "0467123456"
        ]   

    test <@ countNodes numbers = 10 @>

[<Fact>]
let ``Test Case 2: numbers with different base`` () =
    let numbers = 
        [
            "0123456789"
            "1123456789"       
        ]   

    test <@ countNodes numbers = 20 @>

[<Fact>]
let ``Test Case 3: numbers included into another one`` () =
    let numbers = 
        [
            "0123456789"
            "0123"
        ]

    test <@ countNodes numbers = 10 @>

[<Fact>]
let ``Test Case 4: numbers with common part`` () =
    let numbers = 
        [
            "0412578440"
            "0412199803"
            "0468892011"
            "112"
            "15"
        ]
    
    test <@ countNodes numbers = 28 @>

[<Fact>]
let ``Test Case 5: big number list`` () =
    let numbers = 
        File.ReadAllLines("./PhoneNumbers/numbers.txt")
        |> List.ofArray

    test <@ countNodes numbers = 45512 @>
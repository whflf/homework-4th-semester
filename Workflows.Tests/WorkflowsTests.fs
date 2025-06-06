module Workflows.Tests

open NUnit.Framework
open FsUnit

open Workflows

[<Test>]
let ``rounding workflow should round number to 2 decimal places`` () =
    let result = rounding 2 { return 3.14159 }
    result |> should equal 3.14

[<Test>]
let ``rounding workflow should round sum of numbers to 1 decimal place`` () =
    let result = 
        rounding 1 {
            let! a = 2.34
            let! b = 1.25
            return a + b
        }
    result |> should equal 3.6

[<Test>]
let ``rounding workflow should round to tens when accuracy is negative`` () =
    let result = rounding -1 { return 123.456 }
    result |> should equal 120.0

[<Test>]
let ``rounding workflow should round very small number to 4 decimal places`` () =
    let result = rounding 4 { return 0.0000123456789 }
    result |> should equal 0.0

[<Test>]
let ``rounding workflow should leave zero unchanged`` () =
    let result = rounding 3 { return 0.0 }
    result |> should equal 0.0


[<Test>]
let ``calculate workflow should parse and add two valid numbers`` () =
    let result = 
        calculate () {
            let! a = "1.5"
            let! b = "2.5"
            return a + b
        }
    result |> should equal (Some 4.0f)

[<Test>]
let ``calculate workflow should return None when input is invalid`` () =
    let result = 
        calculate () {
            let! a = "1.5"
            let! b = "not number"
            return a + b
        }
    result |> should equal None

[<Test>]
let ``calculate workflow should return value directly without parsing`` () =
    let result = calculate () { return 10.0f }
    result |> should equal (Some 10.0f)

[<Test>]
let ``calculate workflow should return None for empty string input`` () =
    let result = calculate () { let! a = "" in return a }
    result |> should equal None


[<Test>]
let ``combined workflow should parse string and round result`` () =
    let result = 
        calculate () {
            let! a = "3.14159"
            let rounded = rounding 2 { return float a }
            return rounded
        }
    result |> should equal (Some 3.14f)

[<Test>]
let ``combined workflow should return None if parsing fails before rounding`` () =
    let result = 
        calculate () {
            let! a = "abc"
            let rounded = rounding 2 { return float a }
            return rounded
        }
    result |> should equal None

[<Test>]
let ``combined workflow should compute sum and round to 1 decimal place`` () =
    let result = 
        calculate () {
            let! a = "1.234"
            let! b = "5.678"
            let sum = a + b
            let rounded = rounding 1 { return float sum }
            return rounded
        }
    result |> should equal (Some 6.9f)

[<Test>]
let ``combined workflow should parse number and round to tens`` () =
    let result = 
        calculate () {
            let! a = "123.456"
            let rounded = rounding -1 { return float a }
            return rounded
        }
    result |> should equal (Some 120.0f)

[<Test>]
let ``combined workflow should return None if any step fails`` () =
    let result = 
        calculate () {
            let! a = "12.34"
            let! b = "not number"
            let sum = a + b
            let rounded = rounding 2 { return float sum }
            return rounded
        }
    result |> should equal None

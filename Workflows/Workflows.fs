module Workflows

open System

let roundWithAccuracy accuracy number = 
    round (number * (10.0 ** accuracy)) / (10.0 ** accuracy)

// RoundingBuilder is a computation expression builder that rounds numbers to a specified accuracy.
type RoundingBuilder(accuracy: int) =
    member this.Bind(x, f) =
        f x
    member this.Return(x) =
        roundWithAccuracy accuracy x

let rounding accuracy = RoundingBuilder(accuracy)


// CalculateBuilder is a computation expression builder that parses strings
// to single precision floats and allows for arithmetic operations.
type CalculateBuilder() =
    member this.Bind(x: string, f) =
        let newX = x.Replace('.', ',')
        match Single.TryParse(newX) with
        | true, value -> f value
        | false, _ -> None
    member this.Return(x) =
        Some x

let calculate () = CalculateBuilder()

let result = 
        calculate () {
            let! a = "1.5"
            let! b = "2.5"
            return a + b
        }

printfn "Result: %A" result

module LambdaParser.Tests

open NUnit.Framework
open FsUnit
open LambdaInterpreter
open LambdaParser
open FParsec


[<Test>]
let ``S K K result should be \z.z`` () =
    let testInput = "let S = \x y z.x z (y z)
                    let K = \x y.x
                    S K K"
    let result = testInput |> run program

    let reducted =
        match result with
        | Success(result, _, _) -> toString (betaReduction (buildAST result))
        | Failure(errorMessage, _, _) -> failwith errorMessage

    reducted |> should equal "\z.z"

[<Test>]
let ``succ (succ (plus one zero)) result should be \f.\x.f (f (f x))`` () =
    let testInput = "let succ = \\n f x.f (n f x)
                    let plus = \\n m f x. n f (m f x)
                    let one = \\f x. f x
                    let zero = \\f x.x
                    succ (succ (plus one zero))"
    let result = testInput |> run program

    let reducted =
        match result with
        | Success(result, _, _) -> toString (betaReduction (buildAST result))
        | Failure(errorMessage, _, _) -> failwith errorMessage

    reducted |> should equal "\\f.\x.f (f (f x))"

[<Test>]
let ``S result should be \x.\y.\z.x z (y z)`` () =
    let testInput = "let S = \x y z.x z (y z)
                    S"
    let result = testInput |> run program

    let reducted =
        match result with
        | Success(result, _, _) -> toString (betaReduction (buildAST result))
        | Failure(errorMessage, _, _) -> failwith errorMessage

    reducted |> should equal "\x.\y.\z.x z (y z)"

[<Test>]
let ``K a b result should be a`` () =
    let testInput = "let K = \x y.x
                    K a b"
    let result = testInput |> run program

    let reducted =
        match result with
        | Success(result, _, _) -> toString (betaReduction (buildAST result))
        | Failure(errorMessage, _, _) -> failwith errorMessage

    reducted |> should equal "a"


[<Test>]
let ``S a b c result should be a c (b c)`` () =
    let testInput = "let S = \x y z.x z (y z)
                    S a b c"
    let result = testInput |> run program

    let reducted =
        match result with
        | Success(result, _, _) -> toString (betaReduction (buildAST result))
        | Failure(errorMessage, _, _) -> failwith errorMessage

    reducted |> should equal "a c (b c)"

[<Test>]
let ``S K K x result should be x`` () =
    let testInput = "let S = \x y z.x z (y z)
                    let K = \x y.x
                    S K K x"
    let result = testInput |> run program

    let reducted =
        match result with
        | Success(result, _, _) -> toString (betaReduction (buildAST result))
        | Failure(errorMessage, _, _) -> failwith errorMessage

    reducted |> should equal "x"

[<Test>]
let ``F a b result should be b`` () =
    let testInput = "let F = \x y.y
                    F a b"
    let result = testInput |> run program

    let reducted =
        match result with
        | Success(result, _, _) -> toString (betaReduction (buildAST result))
        | Failure(errorMessage, _, _) -> failwith errorMessage

    reducted |> should equal "b"

[<Test>]
let ``first (pair x y) result should be x`` () =
    let testInput = "let pair = \\a b f.f a b
                    let first = \p.p (\x y.x)
                    first (pair x y)"
    let result = testInput |> run program

    let reducted =
        match result with
        | Success(result, _, _) -> toString (betaReduction (buildAST result))
        | Failure(errorMessage, _, _) -> failwith errorMessage

    reducted |> should equal "x"

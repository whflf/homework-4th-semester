module ParseTree.Tests

open NUnit.Framework
open FsUnit

open ParseTree

[<Test>]
let ``evaluate should handle tree with single operand`` () =
    let tree = Operand 5.0
    let result = evaluate tree
    result |> should equal 5.0

[<Test>]
let ``evaluate should handle simple addition`` () =
    let tree = Operator ('+', Operand 2.0, Operand 3.0)
    let result = evaluate tree
    result |> should equal 5.0

[<Test>]
let ``evaluate should handle complex expression`` () =
    let tree = 
        Operator ('*', 
            Operator ('+', Operand 2.0, Operand 3.0),
            Operand 4.0
        )
    let result = evaluate tree
    result |> should equal 20.0

 
[<Test>]
let ``evaluate should handle division`` () =
    let tree = Operator ('/', Operand 10.0, Operand 2.0)
    let result = evaluate tree
    result |> should equal 5.0

[<Test>]
let ``evaluate should throw exception for unsupported operator`` () =
    let tree = Operator ('^', Operand 2.0, Operand 3.0)
    (fun () -> evaluate tree |> ignore) 
    |> should throw typeof<System.Exception>

[<Test>]
let ``evaluate should handle nested operations`` () =
    let tree = 
        Operator ('-', 
            Operator ('*', Operand 3.0, Operand 4.0),
            Operator ('+', Operand 2.0, Operand 1.0)
        )
    let result = evaluate tree
    result |> should equal 9.0

[<Test>]
let ``evaluate should handle negative operands`` () =
    let tree = Operator ('+', Operand -2.0, Operand 3.0)
    let result = evaluate tree
    result |> should equal 1.0

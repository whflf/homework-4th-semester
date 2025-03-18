module ParseTree.Tests

open NUnit.Framework
open FsUnit

open ParseTree

[<Test>]
let ``evaluate should handle empty tree`` () =
    let tree = Empty
    let result = evaluate tree
    result |> should equal 0.0

[<Test>]
let ``evaluate should handle tree with single operand`` () =
    let tree = Node(Operand 5.0, Empty, Empty)
    let result = evaluate tree
    result |> should equal 5.0

[<Test>]
let ``evaluate should handle simple addition`` () =
    let tree = Node(Operator '+', Node(Operand 2.0, Empty, Empty), Node(Operand 3.0, Empty, Empty))
    let result = evaluate tree
    result |> should equal 5.0

[<Test>]
let ``evaluate should handle complex expression`` () =
    let tree = 
        Node(Operator '*', 
            Node(Operator '+', Node(Operand 2.0, Empty, Empty), Node(Operand 3.0, Empty, Empty)),
            Node(Operand 4.0, Empty, Empty)
        )
    let result = evaluate tree
    result |> should equal 20.0

 
[<Test>]
let ``evaluate should handle division`` () =
    let tree = Node(Operator '/', Node(Operand 10.0, Empty, Empty), Node(Operand 2.0, Empty, Empty))
    let result = evaluate tree
    result |> should equal 5.0

[<Test>]
let ``evaluate should throw exception for unsupported operator`` () =
    let tree = Node(Operator '^', Node(Operand 2.0, Empty, Empty), Node(Operand 3.0, Empty, Empty))
    (fun () -> evaluate tree |> ignore) 
    |> should throw typeof<System.Exception>

[<Test>]
let ``evaluate should throw exception for invalid tree`` () =
    let tree = Node(Operator '+', Empty, Empty)
    (fun () -> evaluate tree |> ignore) 
    |> should throw typeof<System.Exception>

[<Test>]
let ``evaluate should handle nested operations`` () =
    let tree = 
        Node(Operator '-', 
            Node(Operator '*', Node(Operand 3.0, Empty, Empty), Node(Operand 4.0, Empty, Empty)),
            Node(Operator '+', Node(Operand 2.0, Empty, Empty), Node(Operand 1.0, Empty, Empty))
        )
    let result = evaluate tree
    result |> should equal 9.0

[<Test>]
let ``evaluate should handle negative operands`` () =
    let tree = Node(Operator '+', Node(Operand -2.0, Empty, Empty), Node(Operand 3.0, Empty, Empty))
    let result = evaluate tree
    result |> should equal 1.0

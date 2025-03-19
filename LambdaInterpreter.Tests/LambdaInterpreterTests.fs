module LambdaInterpreter.Tests

open NUnit.Framework
open FsUnit

open LambdaInterpreter

[<Test>]
let ``getFreeVariables should return correct set for simple variable`` () =
    let term = Variable "x"
    let result = getFreeVariables term
    result |> should equal (Set.ofList ["x"])

[<Test>]
let``getFreeVariables should return correct set for abstraction`` () =
    let term = Abstraction("x", Variable "x")
    let result = if Set.isEmpty (getFreeVariables term) then 1 else 0
    result |> should equal 1

[<Test>]
let``getFreeVariables should return correct set for application`` () =
    let term = Application(Variable "x", Variable "y")
    let result = getFreeVariables term
    result |> should equal (Set.ofList ["x"; "y"])

[<Test>]
let ``substitute should replace free variable`` () =
    let term = Variable "x"
    let substitution = Variable "y"
    let result = substitute "x" substitution term
    result |> should equal (Variable "y")

let ``substitute should not replace bound variable`` () =
    let term = Abstraction("x", Variable "x")
    let substitution = Variable "y"
    let result = substitute "x" substitution term
    result |> should equal term

[<Test>]
let ``betaReduction should reduce simple application`` () =
    let term = Application(Abstraction("x", Variable "x"), Variable "y")
    let result = betaReduction term
    result |> should equal (Variable "y")

[<Test>]
let ``betaReduction should handle nested applications`` () =
    let term = Application(Abstraction("x", Application(Variable "x", Variable "y")), Variable "z")
    let result = betaReduction term
    result |> should equal (Application(Variable "z", Variable "y"))

[<Test>]
let ``Addition should work correctly`` () =
    let two = Abstraction("f", 
                Abstraction("x", 
                    Application(Variable "f", 
                        Application(Variable "f", Variable "x"))))
    let three = Abstraction("f", 
                Abstraction("x", 
                    Application(Variable "f", 
                        Application(Variable "f", 
                            Application(Variable "f", Variable "x")))))
    let five = Abstraction("f", 
                Abstraction("x", 
                    Application(Variable "f", 
                        Application(Variable "f", 
                            Application(Variable "f", 
                                Application(Variable "f", 
                                    Application(Variable "f", Variable "x")))))))
    let successor = Abstraction("n",
                    Abstraction("f",
                    Abstraction("x", 
                        Application(Variable "f", 
                            Application(Application(Variable "n", Variable "f"), Variable "x")))))
    let plus = Abstraction("m", 
                Abstraction("n", 
                Application(Application(Variable "m", successor), Variable "n")))
    let pluss = Abstraction("m", 
                Abstraction("n",
                Abstraction("f",
                Abstraction("x",
                    Application(Application(Variable "m", Variable "f"), 
                                Application(Application(Variable "n", Variable "f"), Variable "x"))))))
    let result = betaReduction (Application(Application(plus, two), three))
    result |> should equal five

[<Test>]
let ``Y combinator should produce fixed point`` () =
    let yCombinator = Abstraction("f", Application(
        Abstraction("x", Application(Variable "f", Application(Variable "x", Variable "x"))),
        Abstraction("x", Application(Variable "f", Application(Variable "x", Variable "x")))))
    let factorial = Abstraction("f", Abstraction("n", 
            Application(Application(Variable "if", Application(Application(Variable "eq", Variable "n"), Variable "0")),
                Application(Variable "1", Application(Application(Variable "mul", Variable "n"), Application(Variable "f", Application(Variable "pred", Variable "n")))))))
    let fixedPoint = betaReduction (Application(yCombinator, factorial))
    fixedPoint |> should not' (equal factorial)

[<Test>]
let ``Conditional operator should work correctly`` () =
    let trueTerm = Abstraction("x", Abstraction("y", Variable "x"))
    let ifTerm = Abstraction("b", 
                Abstraction("t", 
                Abstraction("f", 
                Application(Application(Variable "b", Variable "t"), 
                Variable "f"))))
    let test = Application(
                Application(Application(ifTerm, trueTerm), Variable "x"),
                Variable "y")
    let result = betaReduction test
    result |> should equal (Variable "x")

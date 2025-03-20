module BracketSequence.Tests

open NUnit.Framework
open FsUnit

open NUnit.Framework
open FsUnit

open BracketSequence

[<Test>]
let ``Empty string should be valid``() =
    isValidBrackets "" |> should equal true

[<Test>]
let ``Single pair of brackets should be valid``() =
    isValidBrackets "()" |> should equal true
    isValidBrackets "[]" |> should equal true
    isValidBrackets "{}" |> should equal true

[<Test>]
let ``Multiple pairs of brackets should be valid``() =
    isValidBrackets "()[]{}" |> should equal true
    isValidBrackets "{[()]}" |> should equal true
    isValidBrackets "(([[{{}}]]))" |> should equal true
    
[<Test>]
let ``Unmatched brackets should be invalid``() =
    isValidBrackets "(]" |> should equal false
    isValidBrackets "([)]" |> should equal false
    isValidBrackets "{[}" |> should equal false
    isValidBrackets "(()" |> should equal false
    isValidBrackets "())" |> should equal false
    
[<Test>]
let ``Valid brackets with extra characters should be valid``() =
    isValidBrackets "a(b[c]{d}e)f" |> should equal true
    isValidBrackets "x{y[z]}w" |> should equal true
    
[<Test>]
let ``Invalid brackets with extra characters should be invalid``() =
    isValidBrackets "a(b[c]d}e)f" |> should equal false
    isValidBrackets "x{y[z]w" |> should equal false

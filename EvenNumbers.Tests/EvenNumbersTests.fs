module EvenNumbers.Tests

open NUnit.Framework
open FsUnit
open FsCheck.NUnit

open EvenNumbers

[<Property>]
let ``countEvenNumbersMap and countEvenNumbersFilter should return the same result`` (list: int list) =
    let resultMap = countEvenNumbersMap list
    let resultFilter = countEvenNumbersFilter list
    resultMap = resultFilter

[<Property>]
let ``countEvenNumbersMap and countEvenNumbersFold should return the same result`` (list: int list) =
    let resultMap = countEvenNumbersMap list
    let resultFold = countEvenNumbersFold list
    resultMap = resultFold

[<Property>]
let ``countEvenNumbersFilter and countEvenNumbersFold should return the same result`` (list: int list) =
    let resultFilter = countEvenNumbersFilter list
    let resultFold = countEvenNumbersFold list
    resultFilter = resultFold

[<Test>]
let ``[ 1; 3; 5; 7 ] should count 0 even numbers`` () =
    let list = [ 1; 3; 5; 7 ]
    countEvenNumbersMap list |> should equal 0

[<Test>]
let ``[ 2; 4; 8; 16 ] should count 4 even numbers`` () =
    let list = [ 2; 4; 8; 16 ]
    countEvenNumbersFilter list |> should equal 4

[<Test>]
let ``Empty list should count 0 even numbers`` () =
    let list = []
    countEvenNumbersMap list |> should equal 0

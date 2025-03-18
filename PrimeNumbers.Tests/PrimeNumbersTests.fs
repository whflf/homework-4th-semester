module PrimeNumbers.Tests

open NUnit.Framework
open FsUnit
open PrimeNumbers

let firstNPrimes n =
    initInfinitePrimeNumbers () |> Seq.take n |> Seq.toList

[<Test>]
let ``First 5 primes should be [2; 3; 5; 7; 11]`` () =
    firstNPrimes 5 |> should equal [2; 3; 5; 7; 11]

[<Test>]
let ``All elements should be prime`` () =
    firstNPrimes 100 |> List.forall isPrime |> should be True

[<Test>]
let ``Sequence should be in ascending order`` () =
    firstNPrimes 100 |> should be ascending

[<Test>]
let ``Large number of elements should be initialized correctly`` () =
    Seq.isEmpty (firstNPrimes 50000) |> should be False

[<Test>]
let ``Elements should be unique`` () =
    let primes = firstNPrimes 5000
    let listLength = List.length (Seq.toList (primes))
    let setCount = Set.count (Set.ofSeq (primes))
    listLength |> should equal setCount

[<Test>]
let ``Elements should be prime`` () =
    let primes = Seq.toList (firstNPrimes 500)
    let filteredPrimes = List.filter (fun x -> isPrime x) primes
    let lengthFiltered = List.length filteredPrimes
    List.length primes |> should equal lengthFiltered

[<Test>]
let ``First prime should be 2`` () =
    Seq.item 0 (initInfinitePrimeNumbers ()) |> should equal 2

[<Test>]
let ``1000th prime should be 7919`` () =
    Seq.item 999 (initInfinitePrimeNumbers ()) |> should equal 7919


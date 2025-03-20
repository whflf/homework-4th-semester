module PointFree.Tests

open NUnit.Framework
open FsUnit
open FsCheck.NUnit

open PointFree

[<Property>]
let testFunc (x: int) (l: int list) =
    let func'3 : int -> int list -> int list = (*) >> List.map
    func'3 (x) (l) = func x l

[<Test>]
let ``func'3 is equivalent to func`` () =
    let func'3 : int -> int list -> int list = (*) >> List.map
    func 5 [1; 2; 3] |> should equal (func'3 (5) ([1; 2; 3]))
    

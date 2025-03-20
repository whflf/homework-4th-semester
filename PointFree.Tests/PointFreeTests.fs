module PointFree.Tests

open NUnit.Framework

open FsCheck

open PointFree

[<Test>]
let testFunc (x: int) (l: int list) =
    func'3 x l = func x l

Check.Quick testFunc


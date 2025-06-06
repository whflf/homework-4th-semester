module MapForTrees.Tests

open NUnit.Framework
open FsUnit
open MapForTrees

[<Test>]
let ``map should process empty tree correctly`` () =
    let emptyTree = Empty
    let result = 
        match map (fun x -> x + 1) emptyTree with
        | Empty -> 0
        | _ -> 1
    result |> should equal 0

[<Test>]
let ``map should process single node tree correctly`` () =
    let singleNodeTree = Node(1, Empty, Empty)
    let result = map (fun x -> x * 2) singleNodeTree
    result |> should equal (Node(2, Empty, Empty))

[<Test>]
let ``map should process two level tree correctly`` () =
    let twoLevelTree = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
    let result = map (fun x -> x + 10) twoLevelTree
    result |> should equal (Node(11, Node(12, Empty, Empty), Node(13, Empty, Empty)))

[<Test>]
let ``map should process three level tree correctly`` () =
    let threeLevelTree = 
        Node(1, 
            Node(2, 
                Node(4, Empty, Empty), 
                Node(5, Empty, Empty)), 
            Node(3, 
                Node(6, Empty, Empty), 
                Empty))

    let result = map (fun x -> x * 2) threeLevelTree
    result |> should equal 
        (Node(2, 
            Node(4, 
                Node(8, Empty, Empty), 
                Node(10, Empty, Empty)), 
            Node(6, 
                Node(12, Empty, Empty), 
                Empty)))

[<Test>]
let ``map should process tree with one subtree correctly`` () =
    let leftSubtreeOnly = Node(1, Node(2, Empty, Empty), Empty)
    let resultLeft = map (fun x -> x - 1) leftSubtreeOnly
    resultLeft |> should equal (Node(0, Node(1, Empty, Empty), Empty))

    let rightSubtreeOnly = Node(1, Empty, Node(3, Empty, Empty))
    let resultRight = map (fun x -> x + 5) rightSubtreeOnly
    resultRight |> should equal (Node(6, Empty, Node(8, Empty, Empty)))

[<Test>]
let ``map should apply complex function correctly`` () =
    let tree = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
    let result = map (fun x -> if x % 2 = 0 then x * 10 else x) tree
    result |> should equal (Node(1, Node(20, Empty, Empty), Node(3, Empty, Empty)))

[<Test>]
let ``map should apply vector function correctly`` () =
    let tree = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
    let result = map (fun x -> (x, x * x, x * x * x)) tree
    result |> should equal (Node((1, 1, 1), Node((2, 4, 8), Empty, Empty), Node((3, 9, 27), Empty, Empty)))

module TestTasks.Tests

open NUnit.Framework
open FsUnit
open FsCheck.NUnit
open FsCheck

open InfiniteSequence
open BinaryTree
open PriorityQueue


// InfiniteSequence tests
[<Test>]
let ``initialSequence should start with 1, -1, 1, -1`` () =
    alternatingSigns |> Seq.take 4 |> Seq.toList |> should equal [1; -1; 1; -1]

[<Property>]
let ``initialSequence should always alternate between 1 and -1`` (n: int) =
    let n = abs n % 1000
    let element1 = alternatingSigns |> Seq.item n
    let element2 = alternatingSigns |> Seq.item (n + 1)
    (element1 = 1 && element2 = -1) || (element1 = -1 && element2 = 1)

[<Test>]
let ``initInfiniteSequence should start with 1, -2, 3, -4`` () =
    initInfiniteSequence() |> Seq.take 4 |> Seq.toList |> should equal [1; -2; 3; -4]

[<Test>]
let ``initInfiniteSequence first 10 elements should match pattern`` () =
    initInfiniteSequence() |> Seq.take 10 |> Seq.toList |> should equal [1; -2; 3; -4; 5; -6; 7; -8; 9; -10]

[<Property>]
let ``initInfiniteSequence odd positions should be positive, even - negative`` (PositiveInt n) =
    let element = initInfiniteSequence() |> Seq.item (n - 1)
    if n % 2 = 1 then 
        element > 0 && element = n
    else 
        element < 0 && element = -n


// BinaryTree tests
let leaf x = Node(x, Empty, Empty)

[<Test>]
let ``findElementsByPredicate should find even numbers`` () =
    let tree = Node(1, Node(2, leaf 3, leaf 4), Node(5, leaf 6, Empty))
    let evenPredicate x = x % 2 = 0
    let result =findElementsByPredicate evenPredicate tree
    result |> should equal [6; 4; 2]

[<Test>]
let ``findElementsByPredicate should find strings starting with 'a'`` () =
    let tree = Node("a", Node("b", Empty, leaf "c"), leaf "d")
    let startsWithAPredicate (s: string) = s.StartsWith("a")
    findElementsByPredicate startsWithAPredicate tree |> should equal ["a"]

[<Property>]
let ``findElementsByPredicate should find all matching elements`` (list: int list) =
    let rec buildTree list = 
        match list with
        | [] -> Empty
        | [x] -> leaf x
        | x :: xs -> Node(x, buildTree xs, Empty)
    
    let tree = buildTree list
    let predicate x = x % 2 = 1
    let expected = list |> List.filter predicate |> List.rev
    let actual = findElementsByPredicate predicate tree
    actual |> should equal expected

[<Test>]
let ``findElementsByPredicate should return empty list if no matches`` () =
    let predicate x = x > 10
    let tree = Node(1, Node(2, leaf 3, leaf 4), Node(5, leaf 6, Empty))
    findElementsByPredicate predicate tree |> should equal List.Empty


// PriorityQueue tests
let createQueueWithItems items =
    let priorityQueue = PriorityQueue<'T>()
    items |> List.iter (fun (p, v) -> priorityQueue.Enqueue(p, v))
    priorityQueue

// [<Test>]
// let ``Dequeue on empty queue should throw exception`` () =
//     let priorityQueue = PriorityQueue<int>()
//    (fun () -> priorityQueue.Dequeue() |> ignore) |> should throw typeof<System.Exception>

[<Test>]
let ``Enqueue and Dequeue single item should return that item`` () =
    let priorityQueue = PriorityQueue<string>()
    priorityQueue.Enqueue(1, "first")
    priorityQueue.Dequeue() |> should equal (1, "first")

[<Test>]
let ``Items should be dequeued in descending priority order`` () =
    let priorityQueue = createQueueWithItems [ (3, "low"); (1, "high"); (2, "medium") ]
    
    priorityQueue.Dequeue() |> should equal (1, "high")
    priorityQueue.Dequeue() |> should equal (2, "medium")
    priorityQueue.Dequeue() |> should equal (3, "low")

[<Test>]
let ``Equal priorities should maintain FIFO order`` () =
    let priorityQueue = createQueueWithItems [ (1, "first"); (1, "second"); (1, "third") ]
    
    priorityQueue.Dequeue() |> should equal (1, "first")
    priorityQueue.Dequeue() |> should equal (1, "second")
    priorityQueue.Dequeue() |> should equal (1, "third")

[<Property>]
let ``All dequeued items should have equal or higher priority than next item`` (items: (int * int) list) =
    let priorityQueue = createQueueWithItems items
    
    let rec checkPriority lastPriority =
        try
            let (p, _) = priorityQueue.Dequeue()
            if p < lastPriority then false
            else checkPriority p
        with _ -> true
    
    checkPriority System.Int32.MinValue

[<Test>]
let ``Complex sequence of operations should maintain correct order`` () =
    let priorityQueue = PriorityQueue<string>()
    priorityQueue.Enqueue(2, "medium1")
    priorityQueue.Enqueue(1, "high")
    priorityQueue.Enqueue(3, "low")
    priorityQueue.Enqueue(2, "medium2")
    
    priorityQueue.Dequeue() |> should equal (1, "high")
    priorityQueue.Dequeue() |> should equal (2, "medium1")
    priorityQueue.Dequeue() |> should equal (2, "medium2")
    priorityQueue.Dequeue() |> should equal (3, "low")

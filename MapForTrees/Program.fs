module MapForTrees

type Tree<'a> =
    | Node of 'a * Tree<'a> * Tree<'a>
    | Empty

type ContinuationStep<'a> =
    | Finished
    | Step of 'a * (unit -> ContinuationStep<'a>)

let rec linearize binTree cont =
    match binTree with
    | Empty -> cont()
    | Node(x, l, r) -> Step(Node(x, l, r), (fun () -> linearize l (fun () -> linearize r cont)))

let map f binTree =
    let steps = linearize binTree (fun () -> Finished)
    let rec processSteps step nodeList =
        match step with
        | Finished -> nodeList
        | Step(node, getNext) -> 
            processSteps (getNext()) (node :: nodeList)
    let nodeList = processSteps steps []

    let rec buildNewTree list stack =
        match list with 
        | [] -> stack
        | head :: tail ->
            let newNode = 
                match head with
                | Node(x, Empty, Empty) -> Node(f x, Empty, Empty)
                | Node(x, Node(_), Empty) -> Node(f x, List.head stack, Empty)
                | Node(x, Empty, Node(_)) -> Node(f x, Empty, List.head stack)
                | Node(x, Node(_), Node(_)) -> Node(f x, List.head stack, stack |> List.tail |> List.head)
                | _ -> failwith "Incorrect binary tree"

            let newStack = 
                match newNode with
                | Node(x, Empty, Empty) -> newNode :: stack
                | Node(x, Node(_), Empty) | Node(x, Empty, Node(_)) -> newNode :: List.tail stack
                | Node(x, Node(_), Node(_)) -> newNode :: (stack |> List.tail |> List.tail)
                | _ -> failwith "Incorrect binary tree"

            buildNewTree (List.tail list) newStack

    let nodeStack = buildNewTree nodeList []
    if not (List.isEmpty nodeStack) then
        match List.tail nodeStack with
        | head :: tail -> failwith "Incorrect binary tree"
        | [] -> List.head nodeStack
    else Empty

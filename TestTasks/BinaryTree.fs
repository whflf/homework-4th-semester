module BinaryTree

type Tree<'a> =
    | Node of 'a * Tree<'a> * Tree<'a>
    | Empty

type ContinuationStep<'a> =
    | Finished
    | Step of 'a * (unit -> ContinuationStep<'a>)

let rec linearize binTree cont =
    match binTree with
    | Empty -> cont()
    | Node(x, l, r) -> Step(x, (fun () -> linearize l (fun () -> linearize r cont)))

let findElementsByPredicate predicate binTree =
    let steps = linearize binTree (fun () -> Finished)
    let rec processSteps step elementList =
        match step with
        | Finished -> elementList
        | Step(element, getNext) -> 
            let newList = 
                if predicate(element) then element :: elementList
                else elementList
            processSteps (getNext()) newList
    
    processSteps steps []

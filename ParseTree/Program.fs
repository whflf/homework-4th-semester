module ParseTree

type ParseTree<'ParseTreeNode> =
    | Node of ParseTreeNode * ParseTree<'ParseTreeNode> * ParseTree<'ParseTreeNode>
    | Empty
and ParseTreeNode =
    | Operator of char
    | Operand of float

type ContinuationStep<'a> =
    | Finished
    | Step of 'a * (unit -> ContinuationStep<'a>)

let rec linearize binTree continuation =
    match binTree with
    | Empty -> continuation()
    | Node(x, l, r) -> Step(x, (fun () -> linearize l (fun () -> linearize r continuation)))

let evaluate parseTree =
    let steps = linearize parseTree (fun () -> Finished)

    let rec processNodeList nodeList (stack: float list) =
        if List.isEmpty nodeList then stack
        else
            match List.head nodeList with
            | Operator op -> 
                match stack with
                | x :: y :: rest ->
                    let result = 
                        match op with
                        | '+' -> x + y
                        | '-' -> x - y
                        | '*' -> x * y
                        | '/' -> x / y
                        | _ -> failwith "Unsupported operator"
                    processNodeList (List.tail nodeList) (result :: rest)
                | _ -> failwith "Invalid parse tree"
            | Operand x -> processNodeList (List.tail nodeList) (x :: stack)
                        
    let rec processSteps step nodeList =
        match step with
        | Finished -> nodeList
        | Step(x, getNext) ->
            processSteps (getNext()) (x :: nodeList)

    let result = processNodeList (processSteps steps []) []

    if List.isEmpty result then 0.0
    elif List.length result > 1 then failwith "Invalid parse tree"
    else List.head result

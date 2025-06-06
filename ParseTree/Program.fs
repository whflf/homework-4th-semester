module ParseTree

type ParseTree =
    | Operator of char * ParseTree * ParseTree
    | Operand of float

let evaluate tree =
    let rec eval tree cont =
        match tree with
        | Operand value -> cont value
        | Operator (op, left, right) ->
            eval left (fun leftValue ->
            eval right (fun rightValue ->
                match op with
                | '+' -> cont (leftValue + rightValue)
                | '-' -> cont (leftValue - rightValue)
                | '*' -> cont (leftValue * rightValue)
                | '/' -> cont (leftValue / rightValue)
                | _ -> failwithf "Unsupported operator '%c'" op
            ))
    
    eval tree id

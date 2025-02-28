let rec reverseList list =
    match list with
    | [] -> []
    | [x] -> [x]
    | head :: tail -> reverseList tail @ [head]

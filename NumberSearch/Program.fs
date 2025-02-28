let rec findNumber list n =
    let rec getIndex list i =
        match list with
        | [] -> None
        | head :: tail -> if head = n then Some(i) else getIndex tail (i + 1)
    getIndex list 0

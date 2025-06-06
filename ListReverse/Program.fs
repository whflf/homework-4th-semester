let reverseList list =
    let rec reverse acc lst =
        match lst with
        | [] -> acc
        | head :: tail -> reverse (head :: acc) tail
    
    reverse [] list

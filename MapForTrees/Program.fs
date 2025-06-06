module MapForTrees

type Tree<'a> =
    | Node of 'a * Tree<'a> * Tree<'a>
    | Empty

let map f tree =
    let rec mapCps tree cont =
        match tree with
        | Empty -> cont Empty
        | Node(x, l, r) ->
            mapCps l (fun mappedLeft ->
            mapCps r (fun mappedRight ->
                cont (Node(f x, mappedLeft, mappedRight))
            ))
    
    mapCps tree id

module MapForTrees

type Tree<'a> =
    | Node of 'a * Tree<'a> * Tree<'a>
    | Empty

let rec map f binTree =
    match binTree with
    | Empty -> Empty
    | Node(x, l, r) -> Node(f x, map f l, map f r)

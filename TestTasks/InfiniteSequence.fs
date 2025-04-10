module InfiniteSequence

let alternatingSigns = Seq.initInfinite(fun i -> pown -1 i)

let initInfiniteSequence () = 
    let alternatingNumbers =
        Seq.initInfinite (fun i -> i + 1)
        |> Seq.zip alternatingSigns
        |> Seq.map (fun (sign, value) -> sign * value)

    alternatingNumbers

let getExponentList n m =
    if n > m then failwith "n should be less than m"
    let rec addPowerOfTwo acc i =
        if i = m then
            addPowerOfTwo (pown 2 (n + m) :: acc) (i - 1)
        else if i = 0 then List.head acc / 2 :: acc
        else
            addPowerOfTwo (List.head acc / 2 :: acc) (i - 1)

    addPowerOfTwo [] m

printfn "%A" (getExponentList 3 5)

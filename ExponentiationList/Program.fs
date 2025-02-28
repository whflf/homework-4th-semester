let getExponentList n m =
    let rec addPowerOfTwo i =
        if i = m then
            [pown 2 (n + m)]
        else 
            let exponentList = addPowerOfTwo (i + 1)
            List.head exponentList / 2 :: exponentList
    addPowerOfTwo 0  

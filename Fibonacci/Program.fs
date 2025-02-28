let rec fibonacci n =
    if n = 0UL then 
        0UL 
    else if n = 1UL then 
        1UL 
    else fibonacci (n - 1UL) + fibonacci (n - 2UL)

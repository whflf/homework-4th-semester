module PrimeNumbers

let isPrime n =
    if n < 2 then false
    else
        let sqrtN = int (sqrt (float n))
        [2..sqrtN] |> List.forall (fun x -> n % x <> 0)

let initInfinitePrimeNumbers () =
    let rec nextPrime n =
        if isPrime n then n
        else nextPrime (n + 1)

    let rec generatePrimes n =
        seq {
            let prime = nextPrime n
            yield prime
            yield! generatePrimes (prime + 1)
        }

    generatePrimes 2

let firstNPrimes n =
    let primeNumbersSequence = initInfinitePrimeNumbers ()
    primeNumbersSequence |> Seq.take n |> Seq.toList

// Пример использования
let result = firstNPrimes 10
printfn "%A" result
        
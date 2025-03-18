module EvenNumbers

let countEvenNumbersMap list = 
    List.map (fun x -> if x % 2 = 0 then 1 else 0) list |> List.fold (+) 0

let countEvenNumbersFilter list =
    List.filter (fun x -> x % 2 = 0) list |> List.length

let countEvenNumbersFold list = 
    List.fold (fun acc number -> 
                (if (number % 2 = 0) then acc + 1 else acc)) 0 list

printfn "%A" <| countEvenNumbersFold [ 1; 2; 3; 4; 6; 2; 8 ]

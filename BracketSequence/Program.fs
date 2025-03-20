module BracketSequence

let isValidBrackets (s: string) =
    let matchingBracket = dict [(')', '('); (']', '['); ('}', '{')]
    
    let rec check stack chars =
        match chars with
        | [] -> List.isEmpty stack
        | h :: t when matchingBracket.ContainsKey h ->
            match stack with
            | top :: rest when top = matchingBracket.[h] ->
                check rest t
            | _ -> false
        | h :: t when matchingBracket.Values |> Seq.contains h ->
            check (h :: stack) t
        | _ :: t -> check stack t
    
    check [] (s |> List.ofSeq)

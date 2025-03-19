module LambdaInterpreter

open System

type LambdaTerm =
    | Variable of string
    | Application of LambdaTerm * LambdaTerm
    | Abstraction of string * LambdaTerm

type ContinuationStep<'a> =
    | Finished
    | Step of 'a * (unit -> ContinuationStep<'a>)

let rec getFreeVariables lambdaTerm =
    let rec linearize term continuation =
        match term with
        | Variable x -> continuation()
        | Application(first, second) as application -> Step(application, (fun () -> linearize first (fun () -> linearize second continuation)))
        | Abstraction(x, body) as abstraction -> Step(abstraction, fun () -> linearize body continuation)
    
    let steps = linearize lambdaTerm (fun () -> Finished)
    let rec processSteps step setAll setBounded =
        match step with
        | Finished -> 
            let setFree = Set.difference setAll setBounded
            let setFreeBounded = Set.intersect setFree setBounded
            Set.union setFree setFreeBounded
        | Step(x, getNext) ->
            match x with
            | Application(first, second) ->
                let newSetAll = 
                    match (first, second) with
                    | (Variable x, Variable y) -> setAll |> Set.add x |> Set.add y
                    | (Variable x, _) -> setAll |> Set.add x
                    | (_, Variable y) -> setAll |> Set.add y
                    | _ -> setAll
                processSteps (getNext()) newSetAll setBounded
            | Abstraction(x, body) ->
                let newSetAll = 
                    match body with
                    | Variable y -> setAll |> Set.add x |> Set.add y
                    | _ -> setAll |> Set.add x
                processSteps (getNext()) newSetAll (setBounded |> Set.add x)
            | _ -> failwith "Incorrect lambda term"

    match lambdaTerm with
    | Variable x -> Set.empty.Add(x)
    | _ -> processSteps steps Set.empty Set.empty

let rec generateRandomString length occupied =
    let random = Random()
    let chars = "abcdefghijklmnopqrstuvwxyz"
    let charsLength = chars.Length
    let randomChars = [| for _ in 1 .. length -> chars.[random.Next(charsLength)] |]

    let result = new string(randomChars)
    if not (Set.contains result occupied) then result else generateRandomString length occupied

let rec substitute freeVariable substitutionTerm generalTerm =
    match generalTerm with
    | Variable x -> if freeVariable = x then substitutionTerm else generalTerm
    | Application(firstTerm, secondTerm) -> 
        Application(substitute freeVariable substitutionTerm firstTerm, substitute freeVariable substitutionTerm secondTerm)
    | Abstraction(x, body) ->
        let substitutionTermFreeVariables = getFreeVariables substitutionTerm
        let result =
            if freeVariable = x then generalTerm
            elif not (Set.contains x substitutionTermFreeVariables) then 
                Abstraction(x, substitute freeVariable substitutionTerm body)
            else 
                let newX = generateRandomString 2 substitutionTermFreeVariables
                Abstraction(newX, substitute x (Variable(newX)) body |> substitute freeVariable substitutionTerm)
        result

let rec betaReduction lambdaTerm = 
    let rec betaReductionStep lambdaTerm = 
        match lambdaTerm with
        | Application(Abstraction(x, body), argument) -> substitute x argument body
        | Application(Variable x, argument) -> Application(Variable x, betaReductionStep argument)
        | Application(firstTerm, secondTerm) ->
            let newFirstTerm = betaReductionStep firstTerm
            match newFirstTerm with
            | Abstraction _ -> (newFirstTerm, betaReductionStep secondTerm) |> Application |> betaReductionStep
            | _ -> Application(firstTerm, betaReductionStep secondTerm)
        | Abstraction(x, body) -> Abstraction(x, betaReductionStep body)
        | _ -> lambdaTerm
    
    let rec getResult lambdaTerm =
        let reducedTerm = betaReductionStep lambdaTerm
        if reducedTerm = lambdaTerm then
            reducedTerm
        else
            getResult reducedTerm 

    getResult lambdaTerm

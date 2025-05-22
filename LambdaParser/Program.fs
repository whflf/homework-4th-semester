module Program

open System
open System.Text

open FParsec

open LambdaInterpreter
open LambdaParser

let args = System.Environment.GetCommandLineArgs()

if args.Length > 1 then
    let input = System.IO.File.ReadAllText(args[0])
    let result = input |> run program

    match result with
    | Success(result, _, _) -> printfn "%A" <| toString (betaReduction (buildAST result))
    | _ -> printfn "%A" result
else
    let infoText = "Write let-definitions and the term to reduce. Type '~' anywhere in the text to quit:"
    printfn "%s" infoText

    let stringBuilder = new StringBuilder()

    let rec programLoop () = 
        let key = Console.Read()
        if key <> -1 then
            let character = Convert.ToChar(key)
            match character with
            | '~' -> 
                printfn "\nInput ended."
                ()
            | _ -> 
                stringBuilder.Append(character) |> ignore
                programLoop()

    programLoop()

    let result = stringBuilder.ToString() |> run program

    match result with
    | Success(result, _, _) -> printfn "Result: %s" <| toString (betaReduction (buildAST result))
    | _ -> printfn "%A" result

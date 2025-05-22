module LambdaParser

open System.Collections.Generic
open FParsec

open LambdaInterpreter

type Program =
| Program of Dictionary<Term', Term> * Term
and Term =
| Atom of Term'
| Application of Term * Term
and Term' =
| Abstraction of string list * Term
| Variable of string
| Brackets of Term

let rec buildAST input =
    let rec buildTerm (dict : Dictionary<Term', Term>) = function
        | Atom(term') -> buildTerm' dict term'
        | Application(term1, term2) -> 
            LambdaTerm.Application(buildTerm dict term1, buildTerm dict term2)
    and buildTerm' (dict : Dictionary<Term', Term>) = function
        | Abstraction(vars, term) ->
            let rec processVars list =
                match list with
                | head :: tail -> LambdaTerm.Abstraction(head, processVars tail)
                | [] -> buildTerm dict term
            processVars vars
        | Brackets(term) -> buildTerm dict term
        | Variable(var) -> 
            let term'Var = Term'.Variable(var)
            if dict.ContainsKey(term'Var) then buildTerm dict dict[term'Var]
            else LambdaTerm.Variable(var)

    match input with
    | Program(dict, expr) -> buildTerm dict expr

let spacesWithoutNewline : Parser<unit, unit> = 
    skipMany (satisfy (fun c -> c = ' ' || c = '\t'))

let (!) parser = parser .>> spacesWithoutNewline
let (!!) parser = parser .>> spaces

let term, termRef = createParserForwardedToRef()

let term', term'Ref = createParserForwardedToRef()

let var = 
    asciiLetter .>>. many (asciiLetter <|> digit)
    |>> fun (first, rest) -> 
        let varName = string first + String.concat "" (List.map string rest)
        Variable varName

let varList = 
    many1 (!var |>> function Variable name -> name | _ -> failwith "Not a variable")

term'Ref := 
    (!(pchar '\\') >>. !varList .>> !(pchar '.') .>>. term 
     |>> Abstraction)
    <|> !var
    <|> (!(pchar '(') >>. !term .>> pchar ')'
         |>> Brackets)

let appChain = 
        !term' .>>. many !term'
        |>> fun (first, rest) ->
            List.fold (fun acc t -> Application(acc, Atom t)) (Atom first) rest
termRef :=     
    appChain <|> (term' |>> Atom)

let def = !(pstring "let") >>. !var .>> !(pchar '=') .>>. term .>> pchar '\n'

let defs = 
    many1 !!def
    |>> (fun pairs -> 
        let dict = Dictionary<Term', Term>()
        let rec fillDict list =
            match list with
            | head as (name, value) :: tail -> 
                if name.ToString() <> "let" then dict.Add(name, value) else failwith "'let' cannot be a variable name"
                fillDict tail
            | [] -> ()
        fillDict pairs
        dict)

let program = !!defs .>>. !term |>> Program


let rec toString term = 
    match term with
    | LambdaTerm.Variable(x) -> x.ToString()
    | LambdaTerm.Abstraction(x, body) -> "\\" + x.ToString() + "." + toString body
    | LambdaTerm.Application(term1, term2) ->
        let openBracket2, closeBracket2 = 
            match term2 with
            | LambdaTerm.Variable(_) -> "", ""
            | _ -> "(", ")"

        toString term1 + " " + openBracket2 + toString term2 + closeBracket2

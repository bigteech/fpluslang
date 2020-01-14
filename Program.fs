// Learn more about F# at http://fsharp.org

open System

type Token =
    | StringToken of string
    | NumberToken of int
    | BlockToken
    | ConditionToken
    | Statement
    | IfToken
    | BindToken
    | VarToken of string
    | DotToken
    | PlusToken
    | MinToken
    | MulToken
    | DiviToken

let isWordChar a =
    (a > 'a' && a < 'z') || (a > 'A' && a < 'Z') || a = '_' || a = '-'

let rec parseWord (text:string) index =
    if isWordChar text.[index] then
        parseWord text (index + 1)
    else
        index

let rec parseString (text: string ) index =
    if text.[index] = '"' then
        index
    else
        parseString text (index + 1)

let parseKeyWord text=
    match text with
      | "if" ->
          IfToken, 2
      | "let" ->
          BindToken, 3
      | _ ->
          VarToken(text), text.Length

let rec getToken (text:string) (index: int) (indexHandler: int -> unit) =
    match text.[index] with
        | a when (a > 'a' && a < 'z') || (a > 'A' && a < 'Z') ->
           parseWord text index
            |> (fun x ->
                indexHandler x;
                StringToken text.[index..x]
                )
            |> (fun x ->
                parseKeyWord x
                |> (fun (x,y) ->
                    indexHandler y
                    x
                    )
                )
        | '"' ->
            parseString text index
            |> (fun x ->
                indexHandler x;
                StringToken text.[index..x]
                )
        | '{' ->
           indexHandler (index+1)
           BlockToken
        | '(' ->
           indexHandler (index+1)
           ConditionToken
        | '.' ->
           indexHandler (index + 1)
           DotToken
        | '+' ->
            indexHandler (index+1)
            PlusToken
        | '-' ->
            indexHandler (index+1)
            MinToken
        | '/' ->
            indexHandler (index+1)
            DiviToken
        | '*' ->
            indexHandler (index+1)
            MulToken
        | ' ' ->
            getToken text (index+1) indexHandler
        | '\n' ->
            getToken text (index+1) indexHandler

let parseStatement (text:string) (index: int) =
    let token = getToken text index ignore
    match token with
        | VarToken x ->
            x
        | _ ->
            ""

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code


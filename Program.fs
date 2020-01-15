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
    | AddToken
    | SubToken
    | MulToken
    | DiviToken
    | SemiToken
    | NewLineToken

type Op =
    | Add
    | Sub
    | Mul
    | Get of string
    | Set
    | PushConst of string
    | LoadConst of string

module Lexical = 
    let isWordChar a =
        (a > 'a' && a < 'z') || (a > 'A' && a < 'Z') || a = '_' || a = '-'

    let rec getWord (text:string) index =
        if isWordChar text.[index] then
            getWord text (index + 1)
        else
            index

    let rec getString (text: string ) index =
        if text.[index] = '"' then
            index
        else
            getString text (index + 1)

    let getKeyWord text=
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
              getWord text index
                |> (fun x ->
                    indexHandler x;
                    StringToken text.[index..x]
                    )
                |> (fun x ->
                    getKeyWord x
                    |> (fun (x,y) ->
                        indexHandler y
                        x
                        )
                    )
            | '"' ->
                getString text index
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
                AddToken
            | '-' ->
                indexHandler (index+1)
                SubToken
            | '/' ->
                indexHandler (index+1)
                DiviToken
            | '*' ->
                indexHandler (index+1)
                MulToken
            | ' ' ->
                getToken text (index+1) indexHandler
            | '\n' ->
                indexHandler (index + 1)
                NewLineToken
            | ';' ->
                indexHandler (index+1)
                SemiToken

module Grammer =
    type Node = {
        tokenType: Token
        children: Node list
    }

let parseStatement (text:string) (index: int) =
    let token = Lexical.getToken text index ignore
    match token with
        | VarToken x ->
            let mutable indexTemp = index + 1
            let nextToken = Lexical.getToken text indexTemp (fun x -> indexTemp <- x)
            match nextToken with
                | DotToken ->
                    [Get x]
                | AddToken ->
                    [PushConst x;Add]
                | SubToken ->
                    [PushConst x;Sub]
                | _ ->
                    []
        | _ ->
            []

[<EntryPoint>]
let main argv =
    0


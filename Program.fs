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
    | AssignToken
    

type Op =
    | Add
    | Sub
    | Mul
    | Get of string
    | Set
    | PushConst of string
    | LoadConst of string
    | Assign

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

    let rec getToken (text:string) (index: int) =
        match text.[index] with
            | a when (a > 'a' && a < 'z') || (a > 'A' && a < 'Z') ->
              getWord text index
                |> (fun x ->
                    (StringToken text.[index..x]), (x+1)
                    )
                |> (fun x ->
                    getKeyWord x
                    |> (fun (x,y) ->
                        x, y+1
                        )
                    )
            | '"' ->
                getString text index
                |> (fun x ->
                    StringToken text.[index..x], x + 1
                    )
            | '{' ->
              BlockToken, index + 1
            | '(' ->
              ConditionToken, index + 1
            | '.' ->
              DotToken, index+1
            | '+' ->
                AddToken, index+1
            | '-' ->
                SubToken,index+1
            | '/' ->
                DiviToken, index+1
            | '*' ->
                MulToken,index+1
            | ' ' ->
                getToken text (index+1) 
            | '\n' ->
                NewLineToken,index+1
            | ';' ->
                SemiToken,index+1

module Grammer =
    type Node = {
        tokenType: Token
        children: Node list
    }

let parseExperience (text:string) (index: int) =
    let nextToken,indexTemp = Lexical.getToken text index
    match nextToken with
        | VarToken x ->
            []
        | _ ->
            []

let parseStatement (text:string) (index: int) =
    let token,indexTemp = Lexical.getToken text index
    match token with
        | VarToken x ->
            let nextToken,indexTemp2 = Lexical.getToken text indexTemp
            match nextToken with
                | DotToken ->
                    [Get x]
                | AddToken ->
                    [PushConst x;Add]
                | SubToken ->
                    [PushConst x;Sub]
                | AssignToken ->
                    parseExperience text indexTemp2

                | _ ->
                    []
        | _ ->
            []

[<ENTRYPOINT>]
let main argv =
    0


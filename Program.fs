// Learn more about F# at http://fsharp.org

open System
type Token =
    | StringToken of string
    | NumberToken of int
    | LeftBraceToken
    | RightBraceToken
    | IfToken
    | BindToken
    | IdenToken of string
    | DotToken
    | AddToken
    | SubToken
    | MulToken
    | DiviToken
    | SemiToken
    | AssignToken
    | LeftParentheses
    | RightParentheses
    | Eof
    

type Op =
    | Add
    | Sub
    | Mul
    | Divi
    | Get of string
    | Set
    | PushConst of string
    | LoadConst of string
    | Load of string 
    | Assign
    | EndExp
    | Throw
    | Nop
    | JumpIfFalse of int

let getLevelByToken token =
    match token with 
        | AddToken -> 2
        | MulToken -> 1


type ParseState = {
    mutable index: int32
    text: string
    mutable op: Op list
    mutable token: Token list
}

type ParseState with 
    member z.moveNext()=
        if z.index = (z.token.Length - 1)then
            Eof
        else
            z.index <- z.index + 1
            z.currentToken
    member z.nextToken()=
            if z.index = (z.token.Length - 1) then
              Eof
            else
              z.token.[z.index+1]
    member z.currentToken with get() = z.token.[z.index]
    member z.pushOp x = z.op <- z.op @ [x] 
    member z.pushOps x = z.op <- z.op @ x
    member z.pushToken x = z.token <-  z.token @ [x]



module Lexical = 
    let isWordChar a =
        (a > 'a' && a < 'z') || (a > 'A' && a < 'Z') || a = '_' || a = '-'

    let rec getWord (text:string) index =
        if isWordChar text.[index] then
            getWord text (index + 1) else
            index

    let rec getNumber (text: string ) index =
        if Char.IsNumber text.[index] then
            getNumber text (index+1) else
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
              IdenToken(text), text.Length

    let opLevelMap = {| Add = 2; Mul = 1 |}

    let rec nextToken (parseState) index : Token*int=
        let text = parseState.text
        if index = text.Length then
            Eof, 0
        else
            match text.[index] with
                  | a when (a > 'a' && a < 'z') || (a > 'A' && a < 'Z') ->
                    getWord text index
                      |> (fun x ->
                          let m,n =getKeyWord text.[index..x-1]
                          m, (index+n)
                          )
                  | a when Char.IsNumber a-> 
                      getNumber text index
                      |> (fun x ->
                          let t = text.[index..x-1]
                          (NumberToken (Int32.Parse t)), x
                          )
                  | '"' ->
                      getString text index
                      |> (fun x ->
                          (StringToken text.[index..x-1]), x
                          )
                  | '{' ->
                      LeftBraceToken, (index+1)
                  | '}' ->
                      RightBraceToken, (index+1)
                  | '(' ->
                      LeftParentheses, (index+1)
                  | ')' ->
                      RightParentheses, (index+1)
                  | '.' ->
                      DotToken, (index+1)
                  | '+' ->
                      AddToken, (index+1)
                  | '-' ->
                      SubToken, (index+1)
                  | '/' ->
                      DiviToken, (index+1)
                  | '*' ->
                      MulToken, (index+1)
                  | ' ' ->
                      nextToken parseState (index+1)
                  | '\n' ->
                      nextToken parseState (index+1)
                  | '\r' ->
                      nextToken parseState (index+1)
                  | ';' ->
                      SemiToken, (index+1)

    let initToken parseState =
        let rec temp index =
            if index < parseState.text.Length then
                let t, i = nextToken parseState index
                match t with
                    | Eof ->
                        ()
                    | _ ->
                        parseState.pushToken t
                        temp i
        temp 0
    
module Grammer =
    type Node = {
        tokenType: Token
        children: Node list
    }

let getCharByToken token = 
    match token with
        |  NumberToken x -> x.ToString()
        

let getOpByToken token = 
    match token with
        |  MulToken  -> Mul
        | AddToken -> Add
        

module rec Parser =
 
    let parseGetOrCallOrNumber parseState =
        ()
    let parseIfStatement (parseState: ParseState) =
        parseState.moveNext() |> ignore
        let ops = parseExperienceBinary parseState 10 (fun () -> [])
        match parseState.currentToken with
            | LeftBraceToken ->
                ()
            | _ ->
                raise (Exception("if需要代码块"))
        let opsBlock = (parseExperienceBinary parseState 10 (fun () -> []))
        match parseState.currentToken with
            | RightBraceToken ->
                ()
            | _ ->
                raise (Exception("if代码块需要闭合"))
        ops @ [JumpIfFalse (opsBlock.Length + 1)] @ opsBlock

    let parseExperienceBinary2 (parseState: ParseState) =
        let ops = parseExperienceBinary parseState 10 (fun () -> [])
        match parseState.currentToken with
            | RightParentheses ->
                ()
            | _ ->
                raise (Exception("括号需要闭合"))
        ops

    let parseExperienceBinary (parseState: ParseState)  level (f1: unit -> Op list) : Op list =
        let token = parseState.moveNext()
        let ops = []
        match token with
            | LeftParentheses ->
                let ops2 = parseExperienceBinary2 parseState
                let op = parseState.moveNext()
                if op = SemiToken || op = RightParentheses || op = LeftParentheses then
                    if op = SemiToken then
                        parseState.moveNext() |> ignore
                    ops 
                    @ ops2
                    @ (f1())
                else
                    let level1 = getLevelByToken op
                    if level1 > level then
                        ops 
                         @ ops2
                         @ (f1())
                         @ (parseExperienceBinary parseState 10 (fun () -> []))
                         @ [getOpByToken op]
                    else
                        ops 
                        @ (parseExperienceBinary parseState level1 (fun () -> 
                                ops2 @ [getOpByToken op]
                            ))
                        @ (f1())
            | SemiToken ->
                []
            | NumberToken x ->
                let op = parseState.moveNext()
                if op = SemiToken || op = RightParentheses || op = LeftBraceToken then
                    if op = SemiToken then
                        parseState.moveNext() |> ignore
                    ops 
                    @ [LoadConst (getCharByToken token)]
                    @ (f1())
                else
                    let level1 = getLevelByToken op
                    if level1 > level then
                        ops 
                         @ [LoadConst (getCharByToken token)]
                         @ (f1())
                         @ (parseExperienceBinary parseState 10 (fun () -> []))
                         @ [getOpByToken op]
                    else
                        ops 
                        @ (parseExperienceBinary parseState level1 (fun () -> 
                                [LoadConst (getCharByToken token); getOpByToken op]
                            ))
                        @ (f1())
                        

    let parseExperience  parseState =
        parseExperienceBinary parseState 10 (fun () -> [])
        

    let parseStatement (parseState: ParseState) = 
        match parseState.nextToken() with
            | Eof ->
                ()
            | IfToken ->
                parseState.pushOps (parseIfStatement parseState)
                parseStatement parseState
            | _  -> 
                parseState.pushOps (parseExperience parseState)
                parseStatement parseState


    let parseSourceElement text =
         let parseState = {text=text;index=(-1);op=[];token=[]}
         Lexical.initToken parseState
         parseStatement  parseState |> ignore
         parseState.op

module Vm = 
    let evalOplist (oplst: Op list) =
        let stack = Collections.Stack()
        let mutable index = 0
        let eval op =
            match op with
                | LoadConst x ->
                    stack.Push x
                    1
                | Add ->
                    let l1 = Int32.Parse(stack.Pop().ToString())
                    let l2 = Int32.Parse(stack.Pop().ToString())
                    stack.Push (l1 + l2)
                    1
                | JumpIfFalse x ->
                    let l1 = Int32.Parse(stack.Pop().ToString())
                    if l1 = 0 then
                        x
                    else
                        1
                | Mul ->
                    let l1 = Int32.Parse(stack.Pop().ToString())
                    let l2 = Int32.Parse(stack.Pop().ToString())
                    stack.Push (l1 * l2)
                    1
        while index < oplst.Length do
            index <- index + (eval (oplst.[index]))
        sprintf "%s" (stack.Pop().ToString())


[<EntryPoint>]
let main argv =
//    assert ((Vm.evalOplist (Parser.parseSourceElement "3 + 14 * 2 + 1;")) = (3 + 14 * 2 + 1).ToString())
//    assert ((Vm.evalOplist (Parser.parseSourceElement "3 + 14 * 2;")) = (3 + 14 * 2).ToString())
//    assert ((Vm.evalOplist (Parser.parseSourceElement "(3 + 14 * 2);")) = ((3 + 14 * 2)).ToString())
//    assert ((Vm.evalOplist (Parser.parseSourceElement "((3 + 14)) * 2;")) = ((3 + 14) * 2).ToString())
   assert ((Vm.evalOplist (Parser.parseSourceElement """
        if 1 {
            1 + 1;
        }
        2 + 2;
   """)) = (4).ToString())
   printf "%s" "success"
   0


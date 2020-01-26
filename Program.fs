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
    | IdenToken of string
    | DotToken
    | AddToken
    | SubToken
    | MulToken
    | DiviToken
    | SemiToken
    | NewLineToken
    | AssignToken
    | LeftParentheses
    | RightParentheses
    

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
    | IfStart
    | IfConditionStart
    | IfConditionEnd
    | IfElseConditionStart
    | IfElseConditionEnd | Else
    | IfEnd
    | Nop

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
    member z.getLastToken()=z.token.[z.token.Length - 1]
    member z.moveNextDefault()=z.index <- z.index + 1
    member z.moveNext x =z.index <- z.index + x
    member z.pushOp x = z.op <- z.op @ [x] 
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

    let rec nextToken (parseState) : Token=
        let text = parseState.text
        let index = parseState.index

        let moveNext x = 
            parseState.moveNext x

        let moveNextDefault () = 
            parseState.moveNextDefault()

        match text.[index] with
              | a when (a > 'a' && a < 'z') || (a > 'A' && a < 'Z') ->
                getWord text index
                  |> (fun x ->
                      getKeyWord text.[index..x]
                      )
                  |> (fun (x,y) ->
                      moveNext (y-index)
                      parseState.pushToken x
                      )
              | a when Char.IsNumber a-> 
                  getNumber text index
                  |> (fun x ->
                      moveNext (x-index)
                      let t = text.[index..x-1]
                      parseState.pushToken (NumberToken (Int32.Parse t))
                      )
              | '"' ->
                  getString text index
                  |> (fun x ->
                      moveNext(x)
                      parseState.pushToken (StringToken text.[index..x])
                      )
              | '{' ->
                  moveNextDefault()
                  parseState.pushToken BlockToken

              | '(' ->
                  moveNextDefault()
                  parseState.pushToken LeftParentheses
              | ')' ->
                  moveNextDefault()
                  parseState.pushToken RightParentheses
              | '.' ->
                  moveNextDefault()
                  parseState.pushToken DotToken
              | '+' ->
                  moveNextDefault()
                  parseState.pushToken AddToken
              | '-' ->
                  moveNextDefault()
                  parseState.pushToken SubToken
              | '/' ->
                  moveNextDefault()
                  parseState.pushToken DiviToken

              | '*' ->
                  moveNextDefault()
                  parseState.pushToken MulToken

              | ' ' ->
                  moveNextDefault() 
                  nextToken parseState |> ignore
              | '\n' ->
                  moveNextDefault()
                  parseState.pushToken NewLineToken
              | ';' ->
                  moveNextDefault()
                  parseState.pushToken SemiToken

        parseState.getLastToken()
    
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
    let parseUnary parseState =
        ()

    let parseExperienceBinary2 parseState =
        let nextToken () = Lexical.nextToken parseState
        let nt = nextToken()
        match nt with
            | LeftParentheses -> 
                parseExperienceBinary2 parseState 
            | _ -> 
                parseExperienceBinary parseState  nt
                

    let parseExperienceBinary parseState startToken  =
        let nextToken () = Lexical.nextToken parseState
        parseState.pushOp (LoadConst (getCharByToken (startToken) ))

        let rec temp token =
            match token with
                | RightParentheses ->
                    temp (nextToken())
                | SemiToken ->
                    SemiToken
                | MulToken | AddToken | DiviToken | SubToken->
                    let tokenNext1 = nextToken()
                    if tokenNext1 = LeftParentheses then
                        parseExperienceBinary2 parseState
                    else
                        let tokenNext = nextToken()
                        if tokenNext = SemiToken then
                            parseState.pushOp (LoadConst (getCharByToken tokenNext1))
                            parseState.pushOp (getOpByToken token)
                            tokenNext
                        elif tokenNext = RightParentheses then
                            parseState.pushOp (LoadConst (getCharByToken tokenNext1))
                            parseState.pushOp (getOpByToken token)
                            temp (nextToken())
                        else
                            let level1 = getLevelByToken token
                            let level2= getLevelByToken tokenNext
                            if level2 >= level1 then // 比自己大，算自己
                                parseState.pushOp (LoadConst (getCharByToken tokenNext1))
                                parseState.pushOp (getOpByToken token)
                                temp tokenNext
                            else // 比自己小，优先级高
                                parseState.pushOp (LoadConst (getCharByToken tokenNext1))
                                let nextNextToken = temp tokenNext
                                parseState.pushOp (getOpByToken token)
                                match nextNextToken with
                                    | SemiToken -> SemiToken
                                    | RightParentheses -> RightParentheses
                                    | _ -> temp nextNextToken
                            

        temp (nextToken())

    let parseExperience  parseState =
        parseExperienceBinary2 parseState

    let parseSourceElement text =
         let parseState = {text=text;index=0;op=[];token=[]}
         parseExperience  parseState
         parseState.op

module Vm = 
    let evalOplist oplst =
        let stack = Collections.Stack()
        let eval op =
            match op with
                | LoadConst x ->
                    stack.Push x
                    0
                | Add ->
                    let l1 = Int32.Parse(stack.Pop().ToString())
                    let l2 = Int32.Parse(stack.Pop().ToString())
                    stack.Push (l1 + l2)
                    0
                | Mul ->
                    let l1 = Int32.Parse(stack.Pop().ToString())
                    let l2 = Int32.Parse(stack.Pop().ToString())
                    stack.Push (l1 * l2)
                    0
        for x in oplst do
            eval x |> ignore
        sprintf "%s" (stack.Pop().ToString())


[<EntryPoint>]
let main argv =
   assert ((Vm.evalOplist (Parser.parseSourceElement "(3 + 14) * 2;")) = ((3 + 14) * 2).ToString())
   assert ((Vm.evalOplist (Parser.parseSourceElement "3 + 14 * 2;")) = (3 + 14 * 2).ToString())
   assert ((Vm.evalOplist (Parser.parseSourceElement "(3 + 14 * 2);")) = ((3 + 14 * 2)).ToString())
   assert ((Vm.evalOplist (Parser.parseSourceElement "((3 + 14)) * 2;")) = ((3 + 14) * 2).ToString())

   printf "%s" "success"
   0


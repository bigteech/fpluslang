// Learn more about F# at http://fsharp.org

open System


type Token =
    | StringToken of string
    | NumberToken of int
    | LeftBraceToken
    | RightBraceToken
    | IfToken
    | ElseToken
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

type ObjectCategory =
    | HashObject = 0
    | NumberObject = 1
    | StringObject = 2
    | BooleanObject = 3
    | FunctionObject = 4

type IMfsObject =
    abstract member Type: ObjectCategory  
    abstract member IsTrue: bool

type Op =
    | Add
    | Sub
    | Mul
    | Divi
    | Get of string
    | Set
    | LoadConst of IMfsObject
    | Load of string 
    | Assign
    | EndExp
    | Throw
    | Nop
    | JumpIfFalse of int
    | Jump of int



type MfsHashObject() =

    interface IMfsObject with 
        member this.Type = ObjectCategory.HashObject
        member this.IsTrue with get() = true

type NumberObject(p: int) = 

    static member Add(p1:NumberObject, p2: NumberObject) =
        NumberObject(p1.Value + p2.Value)
    
    static member Mul(p1:NumberObject, p2: NumberObject) =
        NumberObject(p1.Value * p2.Value)

    member this.Value with get() = p

    override  this.ToString() = 
        p.ToString()

    interface IMfsObject with 
        member this.Type = ObjectCategory.NumberObject
        member this.IsTrue with get() = (p <> 0)

type StringObject(p: string) = 

    static member Add(p1:StringObject, p2: StringObject) =
        StringObject(p1.Value + p2.Value)
    
    member this.Value with get() = p
    member this.ToString() = p.ToString()
    interface IMfsObject with 
        member this.Type = ObjectCategory.StringObject
        member this.IsTrue with get() = (p <> "")

type FunctionObject() = 
    let mutable oplst: Op list = []

    member this.Call(args: IMfsObject list) = 
        let stack = Collections.Stack()
        let mutable index = 0
        let eval op =
            match op with
                | LoadConst x ->
                    stack.Push x
                    1
                | Add ->
                    let l1 = stack.Pop() :?> IMfsObject
                    let l2 = stack.Pop() :?> IMfsObject
                    match l1.Type with
                        | ObjectCategory.StringObject ->
                            StringObject.Add(l1 :?> StringObject ,l2 :?> StringObject) :> IMfsObject
                        | ObjectCategory.NumberObject ->
                            NumberObject.Add(l1 :?> NumberObject, l2 :?> NumberObject) :> IMfsObject
                    |>  stack.Push
                    1
                | JumpIfFalse x ->
                    let l1 = stack.Pop() :?> IMfsObject
                    if not(l1.IsTrue) then
                        x
                    else
                        1
                | Jump x ->
                    x
                | Mul ->
                    let l1 = stack.Pop() :?> IMfsObject
                    let l2 = stack.Pop() :?> IMfsObject
                    match l1.Type with
                        | ObjectCategory.StringObject ->
                            raise (Exception "字符串不能相乘")
                        | ObjectCategory.NumberObject ->
                            NumberObject.Mul(l1 :?> NumberObject, l2 :?> NumberObject) :> IMfsObject
                    |>  stack.Push
                    1
        while index < oplst.Length do
            index <- index + (eval (oplst.[index]))
        stack.Pop()

    member this.OpList with get() = oplst
    member this.PushToOpList(p) = oplst <- (oplst @ p);

    interface IMfsObject with 
        member this.Type = ObjectCategory.FunctionObject
        member this.IsTrue with get() = true


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
          | "else" ->
              ElseToken, 4
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

let getObjectByToken token: IMfsObject = 
    match token with
        |  NumberToken x -> 
            upcast NumberObject(x)
        | StringToken x ->
            upcast StringObject(x)
        

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
        match parseState.nextToken() with
                | ElseToken ->
                    parseState.moveNext() |> ignore
                    parseState.moveNext() |> ignore
                    match parseState.currentToken with
                        | LeftBraceToken ->
                            ()
                        | _ ->
                            raise (Exception("else需要代码块"))
                    let opsElseBlock = parseExperienceBinary parseState 10 (fun () -> [])
                    match parseState.currentToken with
                        | RightBraceToken ->
                            ()
                        | _ ->
                            raise (Exception("if代码块需要闭合"))
                    ops @ [JumpIfFalse (opsBlock.Length + 2)] @ opsBlock @ [Jump (opsElseBlock.Length + 1)] @ opsElseBlock
                | _ ->
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
                    @ [LoadConst (getObjectByToken token)]
                    @ (f1())
                else
                    let level1 = getLevelByToken op
                    if level1 > level then
                        ops 
                         @ [LoadConst (getObjectByToken token)]
                         @ (f1())
                         @ (parseExperienceBinary parseState 10 (fun () -> []))
                         @ [getOpByToken op]
                    else
                        ops 
                        @ (parseExperienceBinary parseState level1 (fun () -> 
                                [LoadConst (getObjectByToken token); getOpByToken op]
                            ))
                        @ (f1())
            | _ -> 
                []        

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
    let eval (f:FunctionObject) =
        f.Call []


[<EntryPoint>]
let main argv =
   let ops = Parser.parseSourceElement (String.Join("", IO.File.ReadLines("./test.mfs")))
   let f = FunctionObject()
   f.PushToOpList ops
   let ret = (Vm.eval f) :?> IMfsObject
   assert (ret.ToString() = (3).ToString())
   printf "%s" "success"
   0


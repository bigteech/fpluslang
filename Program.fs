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
    | CommaToken
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
    | NullObject = 5

type IMfsObject =
    abstract member Type: ObjectCategory  
    abstract member IsTrue: bool

type IMfsCallable =
    abstract member Call: (IMfsObject list) -> IMfsObject  


type Op =
    | Add
    | Sub
    | Mul
    | Divi
    | Get of string
    | Set
    | LoadConst of IMfsObject
    | LoadVar of string
    | Load of string 
    | Assign
    | EndExp
    | Throw
    | Nop
    | JumpIfFalse of int
    | Jump of int
    | Store of String
    | Call of int



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
    override this.ToString() = p.ToString()
    interface IMfsObject with 
        member this.Type = ObjectCategory.StringObject
        member this.IsTrue with get() = (p <> "")

type NullObject() = 
    override this.ToString() = 
        raise (Exception "null不能ToString")
    interface IMfsObject with 
        member this.Type = ObjectCategory.NullObject
        member this.IsTrue with get() = false


let globalScope = new System.Collections.Generic.Dictionary<string, IMfsObject>();


type FunctionObject() = 
    let mutable oplst: Op list = []

    member this.OpList with get() = oplst
    member this.PushToOpList(p) = oplst <- (oplst @ p);

    interface IMfsObject with 
        member this.Type = ObjectCategory.FunctionObject
        member this.IsTrue with get() = true
    interface IMfsCallable with 
        member this.Call(args: IMfsObject list): IMfsObject = 
            let stack = Collections.Stack()
            let mutable scope = new System.Collections.Generic.Dictionary<string, IMfsObject>();
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
                    | Store x ->
                        scope.Add(x, (stack.Pop() :?> IMfsObject))
                        1
                    | Call x ->
                        let p = [ for i in 1 .. x -> (stack.Pop() :?> IMfsObject) ]
                        let f = stack.Pop() :?> IMfsCallable
                        stack.Push (f.Call p)
                        1
                    | LoadVar x ->
                        try
                            stack.Push (scope.Item(x))
                        with
                            | _ ->
                                stack.Push (globalScope.Item(x))
                        1
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
            downcast stack.Pop()

    

type PrintFunction () =
    interface IMfsCallable with 
        member this.Call(args: IMfsObject list): IMfsObject =
            let temp (p: IMfsObject): string = 
                match p.Type with 
                    | ObjectCategory.StringObject->
                        (p :?> StringObject).Value
                    | ObjectCategory.NumberObject->
                        (p :?> NumberObject).Value.ToString()
                    | _ ->
                        p.Type.ToString()
            String.Join("", args |> List.rev |> List.map temp) |> printf "%s"
            upcast NullObject()

    interface IMfsObject with 
        member this.Type = ObjectCategory.FunctionObject
        member this.IsTrue with get() = true

globalScope.Add("print", PrintFunction())
        


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
        (a >= 'a' && a <= 'z') || (a >= 'A' && a <= 'Z') || a = '_' || a = '-' || a = '$'

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
                  | a when (a >= 'a' && a <= 'z') || (a >= 'A' && a <= 'Z') || a = '_' || a = '$' ->
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
                      getString text (index+1)
                      |> (fun x ->
                          (StringToken text.[(index+1)..x-1]), (x+1)
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
                  | '=' ->
                      AssignToken, (index+1)
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
                  | ',' ->
                      CommaToken, (index+1)
                  | _ ->
                      Eof, 0

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

let getObjectByToken token: Op list = 
    match token with
        |  NumberToken x -> 
            [LoadConst (NumberObject(x))]
        | StringToken x ->
            [LoadConst (StringObject(x))]
        | IdenToken x ->
            [LoadVar x]
        

let getOpByToken token = 
    match token with
        |  MulToken  -> Mul
        | AddToken -> Add
        

module rec Parser =
 
    let parseGetOrCallOrNumber parseState =
        ()

    let parseTunple (parseState: ParseState) index =
        let token = parseState.nextToken()
        match token with
            | CommaToken ->
                parseState.moveNext() |> ignore
                parseTunple parseState (index)
            | IdenToken _ | NumberToken _ | StringToken _ ->
                parseState.moveNext() |> ignore
                let c = getObjectByToken token
                let a,b = parseTunple parseState (index+1)
                (c @ a), b
            | LeftParentheses ->
                parseState.moveNext() |> ignore
                let c = parseExperienceBinary2 parseState
                let a,b = parseTunple parseState (index+1)
                (c @ a), (b)
            | _ ->
                [], index

    let parseCall (parseState: ParseState) =
        let p,index = parseTunple parseState 0
        p @ [Call index]
        
    let parseBindStatement (parseState: ParseState) =
        parseState.moveNext() |> ignore
        match parseState.moveNext() with
            | IdenToken name ->
                match parseState.moveNext() with
                    | AssignToken ->
                        (parseExperienceBinary parseState 10 (fun () -> [])) @ [Store name]
                    | _ ->
                        raise (Exception("let需要="))
            | _ ->
                raise (Exception("let需要名称"))
    let parseIfStatement (parseState: ParseState) =
        parseState.moveNext() |> ignore
        let ops = parseExperienceBinary parseState 10 (fun () -> [])
        match parseState.currentToken with
            | LeftBraceToken ->
                ()
            | _ ->
                raise (Exception("if需要代码块"))
        let opsBlock = parseStatement parseState
        match parseState.moveNext() with
            | RightBraceToken ->
                ()
            | _ ->
                raise (Exception("if代码块需要闭合"))
        match parseState.moveNext() with
                | ElseToken ->
                    parseState.moveNext() |> ignore
                    match parseState.currentToken with
                        | LeftBraceToken ->
                            ()
                        | _ ->
                            raise (Exception("else需要代码块"))
                    let opsElseBlock = parseStatement parseState
                    match parseState.moveNext() with
                        | RightBraceToken ->
                            ()
                        | _ ->
                            raise (Exception("if代码块需要闭合"))
                    ops @ [JumpIfFalse (opsBlock.Length + 2)] @ opsBlock @ [Jump (opsElseBlock.Length + 1)] @ opsElseBlock
                | _ ->
                    ops @ [JumpIfFalse (opsBlock.Length + 1)] @ opsBlock


    let parseExperienceBinary3 (parseState: ParseState) ops2 f1 level= 
        let op = parseState.moveNext()
        if op = SemiToken 
            || op = Eof || op = RightParentheses 
            || op = LeftParentheses || op = LeftBraceToken 
            || op = RightBraceToken || op = CommaToken then
            ops2
            @ (f1())
        else
            let level1 = getLevelByToken op
            if level1 > level then
                ops2
                 @ (f1())
                 @ (parseExperienceBinary parseState 10 (fun () -> []))
                 @ [getOpByToken op]
            else
                (parseExperienceBinary parseState level1 (fun () -> 
                        ops2 @ [getOpByToken op]
                    ))
                @ (f1())

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
        match token with
            | LeftParentheses ->
                let ops = parseExperienceBinary2 parseState
                parseExperienceBinary3 parseState ops f1 level
            | SemiToken ->
                []
            | IdenToken x ->
                match parseState.nextToken() with
                    | AddToken | MulToken | DiviToken | SubToken ->
                        let ops = getObjectByToken token
                        parseExperienceBinary3 parseState ops f1 level
                    | _ ->
                        let ops = [LoadVar x] @ (parseCall parseState)
                        parseExperienceBinary3 parseState ops f1 level
            | NumberToken _ | StringToken _ ->
                let ops = getObjectByToken token
                parseExperienceBinary3 parseState ops f1 level
            | _ -> 
                []        

    let parseExperience  parseState =
        parseExperienceBinary parseState 10 (fun () -> [])
        

    let parseStatement (parseState: ParseState): Op list = 
        match parseState.nextToken() with
            | Eof | RightBraceToken ->
                []
            | IfToken ->
                (parseIfStatement parseState) @ (parseStatement parseState)
            | BindToken ->
                (parseBindStatement parseState) @ (parseStatement parseState)
            | _  -> 
                (parseExperience parseState) @ (parseStatement parseState)


    let parseSourceElement text =
         let parseState = {text=text;index=(-1);op=[];token=[]}
         Lexical.initToken parseState
         let ops = parseStatement parseState
         parseState.pushOps ops
         parseState.op

module Vm = 
    let eval (f:IMfsCallable) =
        f.Call []


[<EntryPoint>]
let main argv =
   let ops = Parser.parseSourceElement (String.Join("", IO.File.ReadLines("./test.mfs")))
   let f = FunctionObject()
   f.PushToOpList ops
   Vm.eval f |> ignore
   0


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
    | PipeToken
    | OrToken
    | AssignToken
    | LeftParenthesesToken
    | RightParenthesesToken
    | LeftSquareToken
    | RightSquareToken
    | Eof

type ObjectCategory =
    | FpHashObject = 0
    | FpNumberObject = 1
    | FpStringObject = 2
    | FpBooleanObject = 3
    | FpFunctionObject = 4
    | FpNullObject = 5

type IFpObject =
    abstract member Type: ObjectCategory
    abstract member IsTrue: bool

type IFpCallable =
    abstract member Call: (IFpObject list) -> IFpObject

type IFpHashable =
    abstract member Get: (string) -> IFpObject
    abstract member Set: (string * IFpObject) -> unit


type Op =
    | Add
    | Sub
    | Mul
    | Divi
    | Get
    | Set
    | LoadConst of IFpObject
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
    | Function of Op list * string list




type FpNumberObject(p: int) =

    static member Add(p1:FpNumberObject, p2: FpNumberObject) =
        FpNumberObject(p1.Value + p2.Value)

    static member Mul(p1:FpNumberObject, p2: FpNumberObject) =
        FpNumberObject(p1.Value * p2.Value)

    member this.Value with get() = p

    override  this.ToString() =
        p.ToString()

    interface IFpObject with
        member this.Type = ObjectCategory.FpNumberObject
        member this.IsTrue with get() = (p <> 0)

type FpStringObject(p: string) =

    static member Add(p1:FpStringObject, p2: FpStringObject) =
        FpStringObject(p1.Value + p2.Value)
    
    member this.Value with get() = p
    override this.ToString() = p.ToString()
    interface IFpObject with 
        member this.Type = ObjectCategory.FpStringObject
        member this.IsTrue with get() = (p <> "")

type FpNullObject() = 
    override this.ToString() = 
        raise (Exception "null不能ToString")
    interface IFpObject with 
        member this.Type = ObjectCategory.FpNullObject
        member this.IsTrue with get() = false

type FpHashObject() =
    let kvs = new Collections.Generic.Dictionary<string, IFpObject>();
    
    member this.Set m = 
        let p,v = m;
        try
            if kvs.ContainsKey p then
                (kvs.Remove p) |> ignore
            kvs.Add (p,v)  |> ignore
        with
            | _ ->
                ()

    member this.Get(p: string) =
            try
                kvs.Item p
            with
                | _ ->
                    upcast FpNullObject()

    interface IFpHashable with
        member this.Get(p: string) =
            this.Get p

        member this.Set m =
            this.Set m
    interface IFpObject with 
        member this.Type = ObjectCategory.FpHashObject
        member this.IsTrue with get() = true


let globalScope = new System.Collections.Generic.Dictionary<string, IFpObject>();


type FpFunctionObject(argsNames: string list) = 
    let mutable oplst: Op list = []

    member this.OpList with get() = oplst
    member this.PushToOpList(p) = oplst <- (oplst @ p);

    interface IFpObject with 
        member this.Type = ObjectCategory.FpFunctionObject
        member this.IsTrue with get() = true
    interface IFpCallable with 
        member this.Call(args: IFpObject list): IFpObject = 
            let stack = Collections.Stack()
            let mutable scope = new System.Collections.Generic.Dictionary<string, IFpObject>();
            for i = 0 to (argsNames.Length - 1) do scope.Add(argsNames.[i], args.[i])
            let mutable index = 0
            let eval op =
                match op with
                    | LoadConst x ->
                        stack.Push x
                        1
                    | Add ->
                        let l1 = stack.Pop() :?> IFpObject
                        let l2 = stack.Pop() :?> IFpObject
                        match l1.Type with
                            | ObjectCategory.FpStringObject ->
                                FpStringObject.Add(l1 :?> FpStringObject ,l2 :?> FpStringObject) :> IFpObject
                            | ObjectCategory.FpNumberObject ->
                                FpNumberObject.Add(l1 :?> FpNumberObject, l2 :?> FpNumberObject) :> IFpObject
                        |>  stack.Push
                        1
                    | JumpIfFalse x ->
                        let l1 = stack.Pop() :?> IFpObject
                        if not(l1.IsTrue) then
                            x
                        else
                            1
                    | Jump x ->
                        x
                    | Store x ->
                        scope.Add(x, (stack.Pop() :?> IFpObject))
                        1
                    | Call x ->
                        let f = stack.Pop() :?> IFpCallable
                        let p = [ for i in 1 .. x -> (stack.Pop() :?> IFpObject) ] |> List.rev
                        stack.Push (f.Call p)
                        1
                    | Get ->
                        let l1 = stack.Pop() :?> IFpHashable
                        let l2 = stack.Pop() :?> FpStringObject

                        stack.Push (l1.Get l2.Value)
                        1
                    | LoadVar x ->
                        try
                            stack.Push (scope.Item(x))
                        with
                            | _ ->
                                stack.Push (globalScope.Item(x))
                        1

                    | Function (x, y) ->
                        let f = FpFunctionObject(y)
                        f.PushToOpList x
                        stack.Push f
                        1
                    | Mul ->
                        let l1 = stack.Pop() :?> IFpObject
                        let l2 = stack.Pop() :?> IFpObject
                        match l1.Type with
                            | ObjectCategory.FpStringObject ->
                                raise (Exception "字符串不能相乘")
                            | ObjectCategory.FpNumberObject ->
                                FpNumberObject.Mul(l1 :?> FpNumberObject, l2 :?> FpNumberObject) :> IFpObject
                            | _ ->
                                raise (Exception "字符串不能相乘")
                        |>  stack.Push
                        1
            while index < oplst.Length do
                index <- index + (eval (oplst.[index]))
            downcast stack.Pop()

    

type PrintFunction () =
    interface IFpCallable with 
        member this.Call(args: IFpObject list): IFpObject =
            let temp (p: IFpObject): string = 
                match p.Type with 
                    | ObjectCategory.FpStringObject->
                        (p :?> FpStringObject).Value
                    | ObjectCategory.FpNumberObject->
                        (p :?> FpNumberObject).Value.ToString()
                    | _ ->
                        p.Type.ToString()
            String.Join("", args |> List.map temp) |> printf "%s"
            upcast FpNullObject()

    interface IFpObject with 
        member this.Type = ObjectCategory.FpFunctionObject
        member this.IsTrue with get() = true

type ReadFunction () =
    interface IFpCallable with 
        member this.Call(args: IFpObject list): IFpObject =
            let path = (args.[0] :?> FpStringObject).Value
            upcast FpStringObject (String.Join("", IO.File.ReadLines(path)))

    interface IFpObject with 
        member this.Type = ObjectCategory.FpFunctionObject
        member this.IsTrue with get() = true

type WriteFunction () =
    interface IFpCallable with 
        member this.Call(args: IFpObject list): IFpObject =
            let path = (args.[0] :?> FpStringObject).Value
            let value = (args.[0] :?> FpStringObject).Value
            IO.File.WriteAllText(path, value)
            upcast FpNullObject()

    interface IFpObject with 
        member this.Type = ObjectCategory.FpFunctionObject
        member this.IsTrue with get() = true

type FileHashObject () =
    interface IFpHashable with
        member this.Get(p: string): IFpObject = 
            match p with
                | "read" ->
                    upcast ReadFunction()
                | "write" ->
                    upcast WriteFunction()
                | _ ->
                    upcast FpNullObject()

        member this.Set m =
            raise (Exception "不能改变内置对象")

    interface IFpObject with 
        member this.Type = ObjectCategory.FpHashObject
        member this.IsTrue with get() = true


type FpArrayObject()=
    inherit FpHashObject();

    member this.Init(p: IFpObject list) = 
        for i=0 to (p.Length-1) do 
            base.Set(i.ToString(), p.[i])
        FpArrayObject()

type ArrayCreateFunction () =
    interface IFpCallable with 
        member this.Call(args: IFpObject list): IFpObject =
            let ret = FpArrayObject()
            ret.Init (args) |> ignore
            upcast ret 

    interface IFpObject with 
        member this.Type = ObjectCategory.FpFunctionObject
        member this.IsTrue with get() = true


type ArrayObject () =
    inherit FpHashObject();
    do
        base.Set("create", upcast ArrayCreateFunction())

type HahsObjectCreateFunction () =
    interface IFpCallable with 
        member this.Call(args: IFpObject list): IFpObject =
            let ret = FpHashObject()
            for i=0 to args.Length / 2 - 1 do
                let k = args.[(i * 2)] :?> FpStringObject
                let v = args.[(i * 2) + 1]
                ret.Set(k.Value,v)
            upcast ret 

    interface IFpObject with 
        member this.Type = ObjectCategory.FpHashObject
        member this.IsTrue with get() = true


type HashObject () =
    inherit FpHashObject();
    do
        base.Set("create", upcast HahsObjectCreateFunction())


globalScope.Add("print", PrintFunction())
globalScope.Add("file", FileHashObject())
globalScope.Add("array", ArrayObject())
globalScope.Add("dict", HashObject())



let getLevelByToken token =
    match token with 
        | PipeToken -> 9
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
        (a >= 'a' && a <= 'z') || (a >= 'A' && a <= 'Z') || a = '_' || a = '-' || a = '$' || (a >= '0' && a <= '9')

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
                      LeftParenthesesToken, (index+1)
                  | ')' ->
                      RightParenthesesToken, (index+1)
                  | '[' ->
                      LeftSquareToken, (index+1)
                  | ']' ->
                      RightSquareToken, (index+1)
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
                  | '|' ->
                      if text.[index+1] = '>' then
                        PipeToken, (index+2)
                      else
                        OrToken, (index+1)
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
            [LoadConst (FpNumberObject(x))]
        | StringToken x ->
            [LoadConst (FpStringObject(x))]
        | IdenToken x ->
            [LoadVar x]
        | PipeToken ->
            [Call 1]
        

let getOpByToken token = 
    match token with
        |  MulToken  -> Mul
        |  AddToken -> Add
        | PipeToken -> Call 1
        

module rec Parser =
    let parseTuple (parseState: ParseState) index =
        let token = parseState.nextToken()
        match token with
            | CommaToken ->
                parseState.moveNext() |> ignore
                parseTuple parseState (index)
            | IdenToken _ | NumberToken _ | StringToken _ ->
                parseState.moveNext() |> ignore
                let c = getObjectByToken token
                let a,b = parseTuple parseState (index+1)
                (c @ a), b
            | LeftParenthesesToken ->
                parseState.moveNext() |> ignore
                let c = parseExperienceBinary2 parseState
                let a,b = parseTuple parseState (index+1)
                (c @ a), (b)
            | _ ->
                [], index

    let parseParams (parseState: ParseState): string list = 
        let token = parseState.moveNext()
        match token with
            | IdenToken x->
                match parseState.nextToken() with
                    | CommaToken ->
                        parseState.moveNext() |> ignore
                        let m = parseParams parseState
                        ([x] @ m)
                    | _ ->
                        [x]
            | _ ->
                raise (Exception "形参必须是合法变量名")

    let tryParseCall (parseState: ParseState) =
        let p,index = parseTuple parseState 0
        if index = 0 then
            p, []
        else
            p, [Call index]

    let parseGet (parseState: ParseState) =
        ()
    let parseBindStatement (parseState: ParseState) =
        parseState.moveNext() |> ignore
        match parseState.moveNext() with
            | IdenToken name ->
                match parseState.nextToken() with
                    | AssignToken ->
                        parseState.moveNext() |> ignore
                        (parseExperienceBinary parseState 10 (fun () -> [], [])) @ [Store name]
                    | IdenToken x ->
                        let ps = parseParams parseState
                        parseState.moveNext() |> ignore
                        parseState.moveNext() |> ignore
                        let ret = [Function ((parseStatement parseState), ps); Store name]
                        parseState.moveNext() |> ignore
                        ret
                    | _ ->
                        raise (Exception("let需要="))
            | _ ->
                raise (Exception("let需要名称"))
    let parseIfStatement (parseState: ParseState) =
        parseState.moveNext() |> ignore
        let ops = parseExperienceBinary parseState 10 (fun () -> [], [])
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
        let v1,o1 = f1()
        if op = SemiToken 
            || op = Eof || op = RightParenthesesToken 
            || op = LeftParenthesesToken || op = LeftBraceToken 
            || op = RightBraceToken || op = CommaToken
            || op = RightSquareToken then
            v1
            @ ops2
            @ o1
        else
            let level1 = getLevelByToken op
            if level1 >= level then
                v1
                 @ ops2
                 @ o1
                 @ (parseExperienceBinary parseState 10 (fun () -> [], []))
                 @ [getOpByToken op]
            else
                v1
                @ (parseExperienceBinary parseState level1 (fun () -> 
                        ops2, [getOpByToken op]
                    ))
                @ o1

    let parseExperienceBinary2 (parseState: ParseState) =
        let ops = parseExperienceBinary parseState 10 (fun () -> [], [])
        match parseState.currentToken with
            | RightParenthesesToken ->
                ()
            | _ ->
                raise (Exception("园括号需要闭合"))
        ops

    let parseExperienceBinarySquare (parseState: ParseState) =
        let ops = parseExperienceBinary parseState 10 (fun () -> [], [])
        match parseState.currentToken with
            | RightSquareToken ->
                ()
            | _ ->
                raise (Exception("方括号需要闭合"))
        ops

    let parseExperienceNewArray (parseState: ParseState) =
        let ops = parseTuple parseState 0
        match parseState.moveNext() with
            | RightSquareToken ->
                ()
            | _ ->
                raise (Exception("数组方括号需要闭合"))
        ops

    let parseKv (parseState: ParseState) index =
        let ops, idx = parseTuple parseState index
        if ops.Length = 0 then
            ops, index
        else
            match parseState.nextToken() with
                | SemiToken ->
                    parseState.moveNext() |> ignore
                    let m,n = parseKv parseState idx
                    (ops @ m), n
                | _ ->
                    ops, index


    let parseExperienceNewHashObject (parseState: ParseState) =
        let ops = parseKv parseState 0
        match parseState.moveNext() with
            | RightBraceToken ->
                ()
            | _ ->
                raise (Exception("花括号需要闭合"))
        ops

    let parseExperienceBinary (parseState: ParseState)  level (f1: unit -> Op list * Op list) : Op list =
        let token = parseState.moveNext()
        match token with
            | LeftBraceToken ->
                let opsTuple, index = parseExperienceNewHashObject  parseState
                let ops = opsTuple @ [LoadConst (FpStringObject "create"); LoadVar "dict"; Get]  @ [Call (index*2)]
                parseExperienceBinary3 parseState ops f1 level
            | LeftSquareToken ->
                let opsTuple, index = parseExperienceNewArray  parseState
                let ops = opsTuple @ [LoadConst (FpStringObject "create"); LoadVar "array"; Get]  @ [Call index]
                parseExperienceBinary3 parseState ops f1 level
            | LeftParenthesesToken ->
                let ops = parseExperienceBinary2 parseState
                parseExperienceBinary3 parseState ops f1 level
            | SemiToken ->
                []
            | IdenToken x ->
                match parseState.nextToken() with
                    | AddToken | MulToken | DiviToken | SubToken | PipeToken ->
                        let ops = getObjectByToken token
                        parseExperienceBinary3 parseState ops f1 level
                    | DotToken ->
                        parseState.moveNext() |> ignore
                        let name = parseState.moveNext()
                        match name with
                            | IdenToken name -> 
                                let p,c = tryParseCall parseState
                                let ops = p @ [LoadConst (FpStringObject name);LoadVar x;Get] @ c
                                parseExperienceBinary3 parseState ops f1 level
                            | LeftSquareToken ->
                                let opsValue = parseExperienceBinarySquare  parseState
                                let p,c = tryParseCall parseState
                                let ops = p @ opsValue @ [LoadVar x] @ [Get] @ c
                                parseExperienceBinary3 parseState ops f1 level
                            | _ ->
                                raise (Exception "属性必须是字符串")
                    | _ ->
                        let p,c = tryParseCall parseState
                        let ops = p @ [LoadVar x] @ c
                        parseExperienceBinary3 parseState ops f1 level
            | NumberToken _ | StringToken _ ->
                let ops = getObjectByToken token
                parseExperienceBinary3 parseState ops f1 level
            | _ ->
                []

    let parseExperience  parseState =
        parseExperienceBinary parseState 10 (fun () -> [], [])

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
         let parseState = {text=text+" ";index=(-1);op=[];token=[]}
         Lexical.initToken parseState
         let ops = parseStatement parseState
         parseState.pushOps ops
         parseState.op

module Vm = 
    let eval (f:IFpCallable) =
        f.Call []


[<EntryPoint>]
let main argv =
   let ops = Parser.parseSourceElement (String.Join("", IO.File.ReadLines("./test.fp")))
   let f = FpFunctionObject([])
   f.PushToOpList ops
   Vm.eval f |> ignore
   0


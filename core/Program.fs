module Fp

open System
open Types

let isBinaryOpToken token =
    match token with
        | AddToken
        | SubToken
        | MulToken
        | DiviToken
        | GtToken
        | AndToken
        | OrToken
        | PipeToken
        | VirtualPipeToken
        | GteToken
        | LtToken
        | LteToken
        | DotToken
        | BindToken ->
            true
        | _ ->
            false

let mutable globalScope = new System.Collections.Generic.Dictionary<string, IFpObject>();
FpFunctionObject.SetGlobalscopt globalScope
type TypeOfFunction () =
    interface IFpCallable with
        member this.Call(args: IFpObject list): IFpObject =
            match args.[0].Type with
                | ObjectCategory.FpStringObject->
                    "string"
                | ObjectCategory.FpNumberObject->
                    "number"
                | ObjectCategory.FpBooleanObject->
                    "bool"
                | ObjectCategory.FpFunctionObject->
                    "function"
                | ObjectCategory.FpHashObject->
                    "object"
                | ObjectCategory.FpTupleObject->
                    "tuple"
                | ObjectCategory.FpListObject->
                    "list"
            |> (fun x ->
                FpStringObject x :> IFpObject
            )

    interface IFpObject with
        member this.Type = ObjectCategory.FpFunctionObject
        member this.IsTrue with get() = true

type PrintFunction () =
    interface IFpCallable with
        member this.Call(args: IFpObject list): IFpObject =
            let temp (p: IFpObject): string =
                match p.Type with
                    | ObjectCategory.FpStringObject->
                        (p :?> FpStringObject).Value
                    | ObjectCategory.FpNumberObject->
                        (p :?> FpNumberObject).Value.ToString()
                    | ObjectCategory.FpBooleanObject->
                        if p.IsTrue then
                            "true"
                        else
                            "false"
                    | _ ->
                        "<" + p.Type.ToString() + ">"
            String.Join("", args |> List.map temp) |> printf "%s"
            upcast (FpTupleObject.Empty())

    interface IFpObject with
        member this.Type = ObjectCategory.FpFunctionObject
        member this.IsTrue with get() = true

type IgnoreFunction () =
    interface IFpCallable with
        member this.Call(args: IFpObject list): IFpObject =
            upcast FpTupleObject()

    interface IFpObject with
        member this.Type = ObjectCategory.FpFunctionObject
        member this.IsTrue with get() = true

type NotFunction () =
    interface IFpCallable with
        member this.Call(args: IFpObject list): IFpObject =
            FpBooleanObject (not args.[0].IsTrue) :> IFpObject

    interface IFpObject with
        member this.Type = ObjectCategory.FpFunctionObject
        member this.IsTrue with get() = true

type AndFunction () =
    interface IFpCallable with
        member this.Call(args: IFpObject list): IFpObject =
            FpBooleanObject (args.[0].IsTrue && args.[1].IsTrue) :> IFpObject

    interface IFpObject with
        member this.Type = ObjectCategory.FpFunctionObject
        member this.IsTrue with get() = true

type OrFunction () =
    interface IFpCallable with
        member this.Call(args: IFpObject list): IFpObject =
            FpBooleanObject (args.[0].IsTrue || args.[1].IsTrue) :> IFpObject

    interface IFpObject with
        member this.Type = ObjectCategory.FpFunctionObject
        member this.IsTrue with get() = true



let ListCreateFunction  () =
    {
        new IFpCallable with
            member this.Call(args: IFpObject list): IFpObject =
                let ret = FpListObject()
                ret.Init (args) |> ignore
                upcast ret
            member this.Type = ObjectCategory.FpFunctionObject
            member this.IsTrue with get() = true
    } :> IFpObject

type ListObject () =
    inherit FpHashObject();
    do
        base.Set("create", ListCreateFunction())
        base.Set("map", ListObject.Map)
        base.Set("each", ListObject.Each)
        base.Set("length", ListObject.Length)
        base.Set("find", ListObject.Find)
        base.Set("findIndex", ListObject.FindIndex)
        base.Set("exists", ListObject.Exists)

    static member Exists =
        (fun () ->
            let fn (f : IFpObject list) =
                let f1 = f.[0] :?> IFpCallable
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let ret = FpListObject()
                            let ls = p.[0] :?> FpListObject
                            try
                                (FpNumberObject (ls.Values() |> List.findIndex (fun x ->
                                    (f1.Call [x]).IsTrue
                                ))) |> ignore
                                FpBooleanObject(true) :> IFpObject
                            with
                                | _ ->
                                    FpBooleanObject(false) :> IFpObject
                } :> IFpObject
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = fn p
                }
            ) :> IFpObject
        )()
    
    static member FindIndex =
        (fun () ->
            let fn (f : IFpObject list) =
                let f1 = f.[0] :?> IFpCallable
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let ret = FpListObject()
                            let ls = p.[0] :?> FpListObject
                            try
                                (FpNumberObject (ls.Values() |> List.findIndex (fun x ->
                                    (f1.Call [x]).IsTrue
                                ))) :> IFpObject
                            with
                                | _ ->
                                    (FpTupleObject.Empty()) :> IFpObject
                } :> IFpObject
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = fn p
                }
            ) :> IFpObject
        )()
    static member Find  =
        (fun () ->
            let fn (f : IFpObject list) =
                let f1 = f.[0] :?> IFpCallable
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let ret = FpListObject()
                            let ls = p.[0] :?> FpListObject
                            try
                                ls.Values() |> List.find (fun x ->
                                    (f1.Call [x]).IsTrue
                                )
                            with
                                | _ ->
                                    (FpTupleObject.Empty()) :> IFpObject
                } :> IFpObject
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = fn p
                }
            ) :> IFpObject
        )()
    static member Length =
        (fun () ->
            {
                new IFpCallable with
                    member this.Type = ObjectCategory.FpFunctionObject
                    member this.IsTrue with get() = true
                    member this.Call (p: IFpObject list) =
                        FpNumberObject(p.Length) :> IFpObject
            } :> IFpObject
        )()
    static member Each =
        (fun () ->
            let fn (f : IFpObject list) =
                let f1 = f.[0] :?> IFpCallable
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let ls = p.[0] :?> FpListObject
                            for x in ls.Values() do
                                if x.Type = ObjectCategory.FpTupleObject then
                                    f1.Call (x :?> FpTupleObject).Values |> ignore
                                else
                                    f1.Call [x] |> ignore
                            (FpTupleObject.Empty()) :> IFpObject
                } :> IFpObject
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = fn p
                }
            ) :> IFpObject
        )()
    static member Map =
        (fun () -> 
            let fn (f : IFpObject list) =
                let f1 = f.[0] :?> IFpCallable
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let ret = FpListObject()
                            let ls = p.[0] :?> FpListObject
                            let ret2 = ls.Values() |> List.mapi (fun i x ->
                                    f1.Call [x; FpNumberObject(i)]
                                )
                            ret.Init ret2
                            upcast ret
                } :> IFpObject
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = fn p
                }
            ) :> IFpObject
        )()


let HashObjectCreateFunction () =
    {
         new IFpCallable with
            member this.Call(args: IFpObject list): IFpObject =
                let ret = FpHashObject()
                if args.Length = 1 then
                    let array = args.[0] :?> FpListObject
                    for i in array.Values() do
                        let t = i :?> FpTupleObject
                        let k = t.Values.[0] :?> FpStringObject
                        let v = t.Values.[1]
                        ret.Set(k.Value,v)
                elif args.Length = 2 && args.[0].Type = ObjectCategory.FpStringObject then
                    let k = args.[0] :?> FpStringObject
                    let v = args.[1]
                    ret.Set(k.Value,v)
                else
                    for i=0 to args.Length - 1 do
                        let t = args.[i] :?> FpTupleObject
                        let k = t.Values.[0] :?> FpStringObject
                        let v = t.Values.[1]
                        ret.Set(k.Value,v)
                upcast ret

            member this.Type = ObjectCategory.FpFunctionObject
            member this.IsTrue with get() = true
    }
   

let TupleObjectCreateFunction () =
    {
        new IFpCallable with
            member this.Call(args: IFpObject list): IFpObject =
                let ret = FpTupleObject()
                ret.Init (args) |> ignore
                ret.Freeze()
                upcast ret
            member this.Type = ObjectCategory.FpTupleObject
            member this.IsTrue with get() = true
    }


type HashObject () =
    inherit FpHashObject();
   
    do
        base.Set("create", upcast HashObjectCreateFunction())
        base.Set("keys", HashObject.Keys)
        base.Set("values", HashObject.Values)
        base.Set("count", HashObject.Count)
        base.Set("contain", HashObject.ContainsKey)
        base.Set("effectUpdate", HashObject.EffectUpdate)

    static member EffectUpdate =
        {
            new IFpCallable with
                member this.Type = ObjectCategory.FpFunctionObject
                member this.IsTrue with get() = true
                member this.Call (p: IFpObject list) =
                    let h1 = p.[0] :?> IFpHashable
                    let h2 = p.[1] :?> FpHashObject
                    for x in h2.Keys() do
                      h1.Set  (x,h2.Get(x))
                    ((FpTupleObject.Empty())) :> IFpObject
        } :> IFpObject

    static member ContainsKey =
        (fun () -> 
            let fn (f : IFpObject list) =
                let f1 = f.[0] :?> FpStringObject
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let ls = p.[0] :?> FpListObject
                            (FpBooleanObject(ls.ContainsKey f1.Value)) :> IFpObject
                } :> IFpObject
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = fn p
                }
            ) :> IFpObject
        )()


    static member Count =
        (fun () -> 
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) =
                            let p1 = p.[0] :?> FpHashObject
                            (FpNumberObject(p1.Count())) :> IFpObject
                }
            ) :> IFpObject
        )()

    static member Keys =
        (fun () -> 
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) =
                            let p1 = p.[0] :?> FpHashObject
                            let ret = FpListObject()
                            ret.Init [for x in p1.Keys() do yield FpStringObject x]
                            ret :> IFpObject
                }
            ) :> IFpObject
        )()

    static member Values =
        (fun () -> 
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) =
                            let p1 = p.[0] :?> FpHashObject
                            let ret = FpListObject()
                            ret.Init (p1.Values())
                            ret :> IFpObject
                }
            ) :> IFpObject
        )()
type TupleObject () =
    inherit FpHashObject();
    do
        base.Set("create", upcast TupleObjectCreateFunction())
type StringObject () =
    inherit FpHashObject();
    do
        base.Set("split", StringObject.Split)
        base.Set("from", StringObject.from)
        base.Set("concat", StringObject.Concat)
        base.Set("join", StringObject.Join )
        base.Set("replace", StringObject.Replace )
        base.Set("charAt", StringObject.charAt )
        base.Set("startsWith", StringObject.StartsWith )
        base.Set("endsWith", StringObject.EndsWith )
        base.Set("trimStart", StringObject.TrimStart )
        base.Set("trimEnd", StringObject.TrimEnd )
    static member Concat =
        (fun () -> 
            let fn (f : IFpObject list) =
                let f1 = f.[0] :?> FpStringObject
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let f2 = p.[0] :?> FpStringObject
                            FpStringObject(f1.Value + f2.Value) :> IFpObject

                } :> IFpObject
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = fn p
                }
            ) :> IFpObject
        )()

    static member TrimEnd =
        (fun () -> 
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (f: IFpObject list) =
                            let f1 = f.[0] :?> FpStringObject
                            FpStringObject(f1.Value.TrimEnd()) :> IFpObject
                }
            ) :> IFpObject
        )()

    static member TrimStart =
        (fun () -> 
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (f: IFpObject list) =
                            let f1 = f.[0] :?> FpStringObject
                            FpStringObject(f1.Value.TrimStart()) :> IFpObject
                }
            ) :> IFpObject
        )()

    static member EndsWith =
        (fun () -> 
            let fn (f : IFpObject list) =
                let f1 = f.[0] :?> FpStringObject
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let ls = p.[0] :?> FpStringObject
                            FpBooleanObject(ls.Value.EndsWith(f1.Value)) :> IFpObject

                } :> IFpObject
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = fn p
                }
            ) :> IFpObject
        )()

    static member StartsWith =
        (fun () -> 
            let fn (f : IFpObject list) =
                let f1 = f.[0] :?> FpStringObject
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let ls = p.[0] :?> FpStringObject
                            FpBooleanObject(ls.Value.StartsWith(f1.Value)) :> IFpObject

                } :> IFpObject
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = fn p
                }
            ) :> IFpObject
        )()

    static member charAt =
        (fun () -> 
            let fn (f : IFpObject list) =
                let f1 = f.[0] :?> FpNumberObject
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let ls = p.[0] :?> FpStringObject
                            FpStringObject(ls.Value.[f1.Value].ToString()) :> IFpObject

                } :> IFpObject
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = fn p
                }
            ) :> IFpObject
        )()

    static member Replace =
        (fun () -> 
            let fn (f : IFpObject list) =
                let f1 = f.[0] :?> FpStringObject
                let f2 = f.[1] :?> FpStringObject
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let ls = p.[0] :?> FpStringObject
                            FpStringObject(ls.Value.Replace(f1.Value, f2.Value)) :> IFpObject

                } :> IFpObject
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = fn p
                }
            ) :> IFpObject
        )()
    static member from =
        {
            new IFpCallable with
                member this.Type = ObjectCategory.FpFunctionObject
                member this.IsTrue with get() = true
                member this.Call (p: IFpObject list) =
                    upcast (match p.[0].Type with
                        | ObjectCategory.FpNumberObject -> FpStringObject ((p.[0] :?> FpNumberObject).Value.ToString())
                        | _ -> raise (Exception "无法转换为字符串 "))

        } :> IFpObject
    static member Split =
        (fun () -> 
            let fn (f : IFpObject list) =
                let f1 = f.[0] :?> FpStringObject
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let ls = p.[0] :?> FpStringObject
                            let ret = FpListObject()
                            let ls2 = (ls.Value).Split (f1.Value)
                            ret.Init ([for x in ls2 do yield (FpStringObject x)])
                            ret :> IFpObject

                } :> IFpObject
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = fn p
                }
            ) :> IFpObject
        )()

    static member Join =
        (fun () -> 
            let fn (f : IFpObject list) =
                let f1 = f.[0] :?> FpStringObject
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let ls = p.[0] :?> FpListObject
                            FpStringObject (String.Join(f1.Value, ls)) :> IFpObject
                } :> IFpObject
            (
                {
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = fn p
                }
            ) :> IFpObject
        )()



let addGlobalObject x y =
    if isNull globalScope then
        globalScope <- new System.Collections.Generic.Dictionary<string, IFpObject>();
    globalScope.Add(x, y)

let maxLevel = 10
let getLevelByToken token =
    match token with
        | VirtualCommaToken -> 9
        | GtToken | LtToken | GteToken | LteToken | BindToken -> 7
        | AddToken -> 7
        | SubToken -> 6
        | MulToken -> 5
        | DiviToken -> 5
        | AndToken | OrToken -> 4
        | PipeToken -> 3
        | VirtualPipeToken -> 2
        | CommaToken -> 1
        | DotToken -> 0
        | _ ->
            raise (Exception "异常的符号")
       


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
    member z.nextToken with get() =
            if z.index = (z.token.Length - 1) then
              Eof
            else
              z.token.[z.index+1]
    member z.currentToken with get() = z.token.[z.index]
    member z.pushOp x = z.op <- z.op @ [x]
    member z.pushOps x = z.op <- z.op @ x
    member z.pushToken x = z.token <-  z.token @ [x]


type OpOrToken =
    | Op of Op list
    | Token of Token
    | Done of OpOrToken list

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

    let rec getComment (text: string ) index =
        if text.[index] = '\n' then
            (index+1)
        else
            getComment text (index + 1)

    let getKeyWord text=
        match text with
          | "if" ->
              IfToken, 2
          | "else" ->
              ElseToken, 4
          | "let" ->
              LetToken, 3
          | "fn" ->
              LambdaToken, 2
          | _ ->
              IdenToken(text), text.Length

    let opLevelMap = {| Add = 2; Mul = 1 |}

    let rec nextToken (parseState) index : Token*int=
        let text = parseState.text
        if index >= text.Length then
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
                  | '>' ->
                      if text.[index+1] = '=' then
                        GteToken, (index+2)
                      else
                        GtToken, (index+1)
                  | '<' ->
                      if text.[index+1] = '=' then
                        LteToken, (index+2)
                      // elif text.[index+1] = '-' then
                      //   AssignToken, (index+2)
                      else
                        LtToken, (index+1)
                  | '.' ->
                      DotToken, (index+1)
                  | '+' ->
                      AddToken, (index+1)
                  | '=' ->
                      BindToken, (index+1)
                  | '-' ->
                      SubToken, (index+1)
                  | '/' ->
                      if text.[index+1] = '/' then
                        let n = getComment text (index+2)
                        nextToken parseState n
                      else
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
                  | '&' ->
                      if text.[index+1] = '&' then
                        AndToken, (index+2)
                      else
                        raise (Exception "unkonow token &")
                  | '|' ->
                      if text.[index+1] = '>' then
                        PipeToken, (index+2)
                      elif text.[index+1] = '|' then
                        OrToken, (index+2)
                      else
                        OrToken, (index+1)
                  | _ ->
                      raise (Exception "没定义的Token")

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
        | NumberToken x ->
            [LoadConst (FpNumberObject(x))]
        | StringToken x ->
            [LoadConst (FpStringObject(x))]
        | IdenToken x ->
            [LoadVar x]
       

let getOpByToken token =
    match token with
        | PipeToken | VirtualPipeToken -> Call
        | CommaToken | VirtualCommaToken -> Zip
        | MulToken  -> Mul
        | AddToken -> Add
        | SubToken -> Sub
        | DiviToken -> Divi
        | GtToken -> Gt
        | LtToken -> Lt
        | GteToken -> Gte
        | LteToken -> Lte
        | DotToken -> Get
        | BindToken -> Eq
        | OrToken -> Or
        | AndToken -> And

let expect (parseState: ParseState) token =
    if parseState.moveNext() = token then
        ()
    else
        raise (Exception "")

let expectWithComment (parseState: ParseState) token comment =
    if parseState.moveNext() = token then
        ()
    else
        raise (Exception comment)

module rec Parser =
    let parseParams (parseState: ParseState): string list =
        let rec parse () =
            let token = parseState.moveNext()
            match token with
                | LeftParenthesesToken ->
                    parseState.moveNext() |> ignore
                    expectWithComment parseState RightParenthesesToken "无参数的函数需要空元组"
                    []
                | IdenToken x->
                    match parseState.nextToken with
                        | CommaToken ->
                            parseState.moveNext() |> ignore
                            let m = parse()
                            ([x] @ m)
                        | _ ->
                            [x]
                | _ ->
                    raise (Exception "形参必须是合法变量名")

        match parseState.nextToken with
            | LeftParenthesesToken ->
                parseState.moveNext() |> ignore
                expectWithComment parseState RightParenthesesToken "无参数的函数需要空元组"
                []
            | _ ->
                parse()

    let parseLambdaExpression (parseState: ParseState) =
        let ps = parseParams parseState
        parseState.moveNext() |> ignore
        parseState.moveNext() |> ignore
        let ret = [Function ((parseStatement parseState), ps)]
        parseState.moveNext() |> ignore
        ret

    let parseUnstruct (parseState: ParseState) =
        let rec parse (demandComma) =
            let token = parseState.nextToken
            if demandComma then
                match token with
                    | CommaToken ->
                        parseState.moveNext() |> ignore
                        parse false
                    | _ -> []
            else
                match token with
                    | IdenToken x->
                        parseState.moveNext() |> ignore
                        [x] @ (parse true)
                    | _ ->
                        raise (Exception "解构必须是合法名称")
        parse true

    let parseBindStatement (parseState: ParseState) =
        parseState.moveNext() |> ignore
        match parseState.moveNext() with
            | IdenToken name ->
                match parseState.nextToken with
                    | CommaToken ->
                        let nameList = [name] @ (parseUnstruct parseState)
                        parseState.moveNext() |> ignore
                        (parseExpression parseState) @ [Unstruct nameList]
                    | BindToken ->
                        parseState.moveNext() |> ignore
                        (parseExpression parseState) @ [Store name]
                    | IdenToken _ | LeftParenthesesToken ->
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

    let parseIfExpression (parseState: ParseState) =
        let rec loop demandOp =
            if demandOp && (isBinaryOpToken parseState.nextToken) then
                let ops = parseExpressionBinaryNode parseState maxLevel
                if ops.Length = 0 then
                    ops
                else
                    ops @ loop((ops.Length % 2 = 0))
            elif demandOp then
                []
            else
                let ops = parseExpressionBinaryNode parseState maxLevel
                if ops.Length = 0 then
                    ops
                else
                    ops @ loop((ops.Length % 2 > 0))

        let ops = loop(false) |> sortExpressionBinary

        expectWithComment parseState LeftBraceToken "if需要代码块"
        let opsBlock = parseStatement parseState
        expectWithComment parseState RightBraceToken "if代码块需要闭合"
        match parseState.nextToken with
                | ElseToken ->
                    parseState.moveNext() |> ignore
                    expectWithComment parseState LeftBraceToken "else需要代码块"
                    let opsElseBlock = parseStatement parseState
                    expectWithComment parseState RightBraceToken "if代码块需要闭合"
                    ops
                    @ [JumpIfFalse (opsBlock.Length + 2)]
                    @ opsBlock
                    @ [Jump (opsElseBlock.Length + 1)]
                    @ opsElseBlock
                | _ ->
                    ops @ [JumpIfFalse (opsBlock.Length + 2)] @ opsBlock  @ [Jump 2; LoadConst (FpTupleObject())]


    let sortExpressionBinary (ls: OpOrToken list): Op list =
        let rec joinCall (ls: OpOrToken list) =
            let mutable index = -1;
            for i=0 to ls.Length-2 do
                let current = ls.[i];
                let next = ls.[i+1];
                let isCall = (match current with
                    | Op _ | Done _ ->
                        true                 
                    | _ ->
                        false) && (match next with
                    | Op _ | Done _ ->
                        true                 
                    | _ ->
                        false)
                if isCall && index = -1 then
                    index <- i
            if index > -1 then
                joinCall (
                    (ls.[0..index-1])
                    @ [Done [ls.[index+1]; ls.[index]; Op [Call]]]
                    @ (ls.[index+2..ls.Length-1])
                )
            else
                ls 
               
        let rec sort (ls: OpOrToken list) level =
            let index = ls |> List.tryFindIndex (fun x ->
                match x with
                    | Token y ->
                        (getLevelByToken y) = level                     
                    | _ ->
                        false
            )
            match index with
                | Some x ->
                    match ls.[x] with
                        | Token y ->
                            let v1 = ls.[x-1];
                            let v2 = ls.[x+1];
                            sort (
                                (ls.[0..x-2])
                                @ [Done [v1; v2; Op [getOpByToken y]]]
                                @ (ls.[x+2..ls.Length-1])
                            ) level
                        | _ -> raise (Exception "cant access")
                | _ ->
                    ls
        let rec sortAll ls level =
            if level <= maxLevel then
                sortAll (sort ls level) (level+1)
            else
                ls

        let ret = ResizeArray()
       
        let rec unstruct x =
            match x with
                | Op t ->
                    for k in t do
                        ret.Add k
                | Done t ->
                    for k in t do
                        unstruct k
                | _ ->
                    raise (Exception "")
        let ret2 = sort (sort (sort ls 0) 1) 2
        let sorted = (sortAll (joinCall(ret2)) 2)
        if sorted.Length = 0 then
            []
        else
            unstruct (sorted.[0])
            ret |> List.ofSeq

    let parseExpressionBinaryNode (parseState: ParseState) (limitLevel: int)  : OpOrToken list =
        let token = parseState.nextToken
        match token with
            | CommaToken ->
                parseState.moveNext() |> ignore
                let ops = parseExpressionTuple parseState
                [Token CommaToken] @ ops
            | IfToken ->
                parseState.moveNext() |> ignore
                let ops = parseIfExpression parseState
                [Op ops]
            | LambdaToken ->
                parseState.moveNext() |> ignore
                let ops = parseLambdaExpression parseState
                [Op ops]
            | LeftBraceToken ->
                parseState.moveNext() |> ignore
                let ops = parseExpressionNewHashObject  parseState
                [Op ops]
            | LeftSquareToken ->
                parseState.moveNext() |> ignore
                let ops = parseExpressionNewArray  parseState
                [Op ops]
            | LeftParenthesesToken ->
                parseState.moveNext() |> ignore
                match parseState.nextToken with
                    | RightParenthesesToken ->
                        parseState.moveNext() |> ignore
                        let emptyTuple = FpTupleObject()
                        emptyTuple.Freeze()
                        let ops = [LoadConst emptyTuple]
                        [Op ops]
                    | _ ->
                        let ops = parseExpressionBinaryChild parseState
                        [Op ops]

            | IdenToken x ->
                parseState.moveNext() |> ignore
                match parseState.nextToken with
                    | _ ->
                        [Op [LoadVar x]]

            | NumberToken _ | StringToken _ ->
                parseState.moveNext() |> ignore
                let ops = getObjectByToken token
                [Op ops]
            | DotToken _ ->
                parseState.moveNext() |> ignore
                match parseState.nextToken with
                    | IdenToken x ->
                        parseState.moveNext() |> ignore
                        [Token DotToken; Op [LoadConst (FpStringObject x)]]
                    | NumberToken x ->
                        parseState.moveNext() |> ignore
                        [Token DotToken; Op [LoadConst (FpStringObject (x.ToString()))]]
                    | _ ->
                        [Token DotToken]
            | a when (isBinaryOpToken a) && ((getLevelByToken a) < limitLevel) ->
                parseState.moveNext() |> ignore
                [Token a]
            | _ ->
                []

    let parseExpressionBinary (parseState: ParseState) : OpOrToken list =
        let rec loop () =
            let ops = parseExpressionBinaryNode parseState maxLevel
            if ops.Length = 0 then
                ops
            else
                ops @ loop()
        loop()

    let parseExpressionBinaryWithLimit (parseState: ParseState) (limitLevel: int) : OpOrToken list =
        let rec loop () =
            let ops = parseExpressionBinaryNode parseState limitLevel
            if ops.Length = 0 then
              ops
            else
              ops @ loop()
        loop()

    let parseExpressionBinaryChild (parseState: ParseState) =
        let ops = parseExpression parseState
        expect parseState RightParenthesesToken
        ops

    let parseExpressionBinarySquare (parseState: ParseState) =
        let ops = parseExpression parseState
        expect parseState RightSquareToken
        ops

    let parseExpressionTuple (parseState: ParseState) =
        let rec parse  ()=
            let ops =  parseExpressionBinaryWithLimit parseState 1

            if ops.Length = 0 then
                ops
            else
                match parseState.nextToken with
                    | CommaToken ->
                        parseState.moveNext() |> ignore
                        let m = parse ()
                        (ops @ [Token CommaToken] @ m)
                    | _ ->
                        ops
        let ops = parse ()
        ops @ [Token VirtualPipeToken] @ [Op [LoadVar "tuple"; LoadConst (FpStringObject "create"); Get]]

    let parseExpressionNewArray (parseState: ParseState) =
        let rec parse () =
            let ops = parseExpressionBinary parseState
            if ops.Length = 0 then
                ops
            else
                match parseState.nextToken with
                    | SemiToken ->
                        parseState.moveNext() |> ignore
                        let m = parse ()
                        ops @ [Token VirtualCommaToken] @ m
                    | RightSquareToken ->
                        ops
                    | _ ->
                        raise (Exception "右方括号需要闭合")
        let ops = parse ()
        expectWithComment parseState RightSquareToken "右方括号需要闭合"
        if ops.Length = 0 then
            let emptyTuple = FpTupleObject()
            emptyTuple.Freeze()
            [LoadConst emptyTuple; LoadVar "list"; LoadConst (FpStringObject "create"); Get]  @ [Call]
        else
            (ops |> sortExpressionBinary) @ [LoadVar "list"; LoadConst (FpStringObject "create"); Get]  @ [Call]

    let parseKv (parseState: ParseState) index =
        let ops = parseExpression parseState
        if ops.Length = 0 then
            ops, index
        else
            match parseState.nextToken with
                | SemiToken ->
                    parseState.moveNext() |> ignore
                    let m,n = parseKv parseState (index+1)
                    (ops @ m), n
                | RightBraceToken ->
                    ops, (index+1)
                | _ ->
                    raise (Exception "右花括号需要闭合")


    let parseExpressionNewHashObject (parseState: ParseState) =
        let ops, index = parseKv parseState 0
        expect parseState RightBraceToken
        let ops2 = (
            if index > 1 then
                ops @ [for x in [1 .. index-1] do yield Zip]
            else
                ops
        ) 
        if ops.Length = 0 then
            let emptyTuple = FpTupleObject()
            emptyTuple.Freeze()
            [LoadConst emptyTuple; LoadVar "dict"; LoadConst (FpStringObject "create"); Get]  @ [Call]
        else
            ops2 @ [ LoadVar "dict"; LoadConst (FpStringObject "create"); Get]  @ [Call]
    let parseExpression  (parseState: ParseState): Op list =
        parseExpressionBinary parseState |> sortExpressionBinary

    let parseExpressionInStatement  parseState =
        let ops = parseExpression parseState
        expectWithComment parseState SemiToken "表达式需要;结尾"
        ops

    let parseStatement (parseState: ParseState): Op list =
        let rec parse () =
            let m = (
                match parseState.nextToken with
                    | Eof | RightBraceToken ->
                        [Exit]
                    | LetToken ->
                        (parseBindStatement parseState)
                    | _  ->
                        (parseExpressionInStatement parseState)
            )
            if m.Length = 1 && m.[0] = Exit then
                []
            else
                m @ parse()
        parse()

    let parseSourceElement text =
        let parseState = {text=text+" ";index=(-1);op=[];token=[]}
        Lexical.initToken parseState
        let ops = parseStatement parseState
        parseState.pushOps ops
        parseState.op

module Vm =
    let init () =
        globalScope.Add("print", PrintFunction())
        globalScope.Add("ignore", IgnoreFunction())
        globalScope.Add("typeof", TypeOfFunction())
        globalScope.Add("list", ListObject())
        globalScope.Add("dict", HashObject())
        globalScope.Add("tuple", TupleObject())
        globalScope.Add("string", StringObject())
        globalScope.Add("not", NotFunction())
        globalScope.Add("and", AndFunction())
        globalScope.Add("or", OrFunction())
        globalScope.Add("true", FpBooleanObject(true))
        globalScope.Add("false", FpBooleanObject(false))

    let eval (f:IFpCallable) = f.Call []


module Fp

open System

type FcStack<'T>() =
    let m = new ResizeArray<'T>()

    member x.Pop () =
        let ret = m.[m.Count-1]
        m.RemoveAt(m.Count-1)
        ret
    member x.Push (y: 'T) = 
        m.Add(y)


type Token =
    | StringToken of string
    | NumberToken of int
    | LeftBraceToken
    | RightBraceToken
    | IfToken
    | ElseToken
    | LetToken
    | IdenToken of string
    | DotToken
    | AddToken
    | SubToken
    | MulToken
    | DiviToken
    | SemiToken
    | CommaToken
    | PipeToken
    | VirtualPipeToken
    | OrToken
    | BindToken
    | LeftParenthesesToken
    | RightParenthesesToken
    | LeftSquareToken
    | RightSquareToken
    | LambdaToken
    | Eof
    | GtToken
    | GteToken
    | LtToken
    | LteToken
    | AssignToken

let isBinaryOpToken token =
    match token with 
        | AddToken
        | SubToken
        | MulToken
        | DiviToken
        | PipeToken
        | GtToken
        | VirtualPipeToken
        | GteToken
        | LtToken
        | LteToken
        | BindToken ->
            true
        | _ ->
            false

    
type ObjectCategory =
    | FpHashObject = 0
    | FpNumberObject = 1
    | FpStringObject = 2
    | FpBooleanObject = 3
    | FpFunctionObject = 4
    | FpNullObject = 5
    | FpTupleObject = 6

type IFpObject =
    abstract member Type: ObjectCategory
    abstract member IsTrue: bool

type IFpCallable =
    inherit IFpObject
    abstract member Call: (IFpObject list) -> IFpObject

type IFpHashable =
    inherit IFpObject
    abstract member Get: (string) -> IFpObject
    abstract member Set: (string * IFpObject) -> unit


type Op =
    | Add
    | Sub
    | Mul
    | Divi
    | Get
    | LoadConst of IFpObject
    | LoadVar of string
    | Assign
    | EndExp
    | Throw
    | Nop
    | JumpIfFalse of int
    | Jump of int
    | Store of String
    | Call
    | Zip
    | Function of Op list * string list
    | Exit
    | Gte
    | Eq
    | Lte
    | Gt
    | Lt



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

    member this.Count () = 
        kvs.Count

    member this.ContainsKey (p: string) = 
        kvs.ContainsKey p

    member this.Values () = 
        List.ofSeq kvs.Values

    member this.Keys () = 
        List.ofSeq kvs.Keys

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


let mutable globalScope = new System.Collections.Generic.Dictionary<string, IFpObject>();


type FpBooleanObject(v: bool)=
    interface IFpObject with 
        member this.Type = ObjectCategory.FpBooleanObject
        member this.IsTrue = v


type FpTupleObject()=
    inherit FpHashObject();
    
    let mutable freeze = false;

    member this.Freeze () = 
        freeze <- true

    member this.IsFreeze () =
        freeze

    member this.Set m = 
        raise (Exception "tuple 不能更改值")

    member this.Values =
        base.Values()

    member this.Init(p: IFpObject list) = 
        for i=0 to (p.Length-1) do 
            base.Set(i.ToString(), p.[i])
        FpTupleObject()

    interface IFpObject with 
        member this.Type = ObjectCategory.FpTupleObject

type FpFunctionObject(argsNames: string list, getClosureVar: string -> IFpObject option) = 
    let mutable oplst: Op list = []

    member this.OpList with get() = oplst
    member this.PushToOpList(p) = oplst <- (oplst @ p);

    interface IFpObject with 
        member this.Type = ObjectCategory.FpFunctionObject
        member this.IsTrue with get() = true
    interface IFpCallable with 
        member this.Call(args: IFpObject list): IFpObject = 
            let stack = FcStack<IFpObject>()
            let mutable scope = new System.Collections.Generic.Dictionary<string, IFpObject>();
            if argsNames.Length > 0 then
                for i = 0 to (argsNames.Length - 1) do scope.Add(argsNames.[i], args.[i])
            let mutable index = 0
            let getVarForChind name = 
                try
                    Some (scope.[name])
                with
                    | _ ->
                        getClosureVar name

            let getVarByName name = 
                match (getVarForChind name) with
                    | Some x ->
                        x
                    | _ ->
                        globalScope.[name]
                
            let eval op =
                match op with
                    | LoadConst x ->
                        stack.Push x
                        1
                    | Add ->
                        let l1 = stack.Pop()
                        let l2 = stack.Pop()
                        match l1.Type with
                            | ObjectCategory.FpStringObject ->
                                FpStringObject.Add(l2 :?> FpStringObject ,l1 :?> FpStringObject) :> IFpObject
                            | ObjectCategory.FpNumberObject ->
                                FpNumberObject.Add(l2 :?> FpNumberObject, l1 :?> FpNumberObject) :> IFpObject
                        |>  stack.Push
                        1
                    | Sub ->
                        let l1 = stack.Pop() :?> FpNumberObject
                        let l2 = stack.Pop() :?> FpNumberObject
                        FpNumberObject(l1.Value - l2.Value) |>  stack.Push
                        1
                    | Divi ->
                        let l2 = stack.Pop() :?> FpNumberObject
                        let l1 = stack.Pop() :?> FpNumberObject
                        FpNumberObject(l1.Value / l2.Value) |>  stack.Push
                        1
                    | Gt ->
                        let l2 = stack.Pop() :?> FpNumberObject
                        let l1 = stack.Pop() :?> FpNumberObject
                        FpBooleanObject(l1.Value > l2.Value) |>  stack.Push
                        1
                    | Lt ->
                        let l2 = stack.Pop() :?> FpNumberObject
                        let l1 = stack.Pop() :?> FpNumberObject
                        FpBooleanObject(l1.Value < l2.Value) |>  stack.Push
                        1
                    | Gte ->
                        let l2 = stack.Pop() :?> FpNumberObject
                        let l1 = stack.Pop() :?> FpNumberObject
                        FpBooleanObject(l1.Value >= l2.Value)
                        |>  stack.Push
                        1
                    | Eq ->
                        let l1 = stack.Pop()
                        let l2 = stack.Pop()
                        match l1.Type with
                            | ObjectCategory.FpStringObject ->
                                FpBooleanObject((l1 :?> FpStringObject).Value = (l2 :?> FpStringObject).Value)
                            | ObjectCategory.FpNumberObject ->
                                FpBooleanObject((l1 :?> FpNumberObject).Value = (l2 :?> FpNumberObject).Value)
                            | _ ->
                                FpBooleanObject((l1 = l2))
                        |>  stack.Push
                        1
                    | Lte ->
                        let l2 = stack.Pop() :?> FpNumberObject
                        let l1 = stack.Pop() :?> FpNumberObject
                        FpBooleanObject(l1.Value <= l2.Value) |>  stack.Push
                        1
                    | JumpIfFalse x ->
                        let l1 = stack.Pop()
                        if not(l1.IsTrue) then
                            x
                        else
                            1
                    | Jump x ->
                        x
                    | Store x ->
                        scope.Add(x, (stack.Pop()))
                        1
                    | Call ->
                        let f = stack.Pop() :?> IFpCallable
                        let p = stack.Pop()
                        if p.Type = ObjectCategory.FpTupleObject then
                            let p2 = (p :?> FpTupleObject).Values
                            stack.Push (f.Call p2)
                        else
                            stack.Push (f.Call [p])
                        1
                    | Zip ->
                        let l2 = stack.Pop()
                        let l1 = stack.Pop()
                        let v2 = (match l2.Type with
                            | ObjectCategory.FpTupleObject ->
                                let obj = (l2 :?> FpTupleObject)
                                if obj.IsFreeze() then
                                    [l2]
                                else
                                    obj.Values
                            | _ ->
                                [l2])
                        let v1 = (match l1.Type with
                            | ObjectCategory.FpTupleObject ->
                                let obj = (l1 :?> FpTupleObject)
                                if obj.IsFreeze() then
                                    [l1]
                                else
                                    obj.Values
                            | _ ->
                                [l1])
                        
                        let ret = FpTupleObject()
                        ret.Init (v1 @ v2) |> ignore
                        ret |>  stack.Push
                        1
                    | Get ->
                        let l2 = stack.Pop()
                        let l1 = stack.Pop() :?> IFpHashable
                        
                        if l2.Type = ObjectCategory.FpStringObject then
                            stack.Push (l1.Get (l2 :?> FpStringObject).Value)
                        else
                            stack.Push (l1.Get ((l2 :?> FpNumberObject).Value.ToString()))
                        1
                    | Assign ->
                        let l3 = stack.Pop()
                        let l2 = stack.Pop()
                        let l1 = stack.Pop() :?> IFpHashable

                        
                        if l2.Type = ObjectCategory.FpStringObject then
                            l1.Set ((l2 :?> FpStringObject).Value, l3)
                            stack.Push l3
                        else
                            l1.Set ((l2 :?> FpStringObject).Value.ToString(), l3)
                            stack.Push l3
                        1
                    | LoadVar x ->
                        stack.Push (getVarByName x)
                        1
                    | Function (x, y) ->
                        let f = FpFunctionObject(y, getVarForChind)
                        f.PushToOpList x
                        stack.Push f
                        1
                    | Mul ->
                        let l1 = stack.Pop()
                        let l2 = stack.Pop()
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
                        "<" + p.Type.ToString() + ">"
            String.Join("", args |> List.map temp) |> printf "%s"
            upcast FpNullObject()

    interface IFpObject with 
        member this.Type = ObjectCategory.FpFunctionObject
        member this.IsTrue with get() = true


type FpArrayObject()=
    inherit FpHashObject();
    
    member this.Init(p: IFpObject list) = 
        if p.Length = 0 then
            ()
        else
            for i=0 to (p.Length-1) do 
                base.Set(i.ToString(), p.[i])
        ()

let ArrayCreateFunction  () =
    {
        new IFpCallable with 
            member this.Call(args: IFpObject list): IFpObject =
                let ret = FpArrayObject()
                ret.Init (args) |> ignore
                upcast ret 
            member this.Type = ObjectCategory.FpFunctionObject
            member this.IsTrue with get() = true
    } :> IFpObject

type ArrayObject () =
    inherit FpHashObject();
    do
        base.Set("create", ArrayCreateFunction())
        base.Set("map", ArrayObject.Map)
        base.Set("each", ArrayObject.Each)
        base.Set("length", ArrayObject.Length)
        base.Set("find", ArrayObject.Find)
        base.Set("findIndex", ArrayObject.FindIndex)

    static member FindIndex = 
        (fun () ->
            let fn (f : IFpObject list) = 
                let f1 = f.[0] :?> IFpCallable
                { 
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let ret = FpArrayObject()
                            let ls = p.[0] :?> FpArrayObject
                            try
                                (FpNumberObject (ls.Values() |> List.findIndex (fun x -> 
                                    (f1.Call [x]).IsTrue
                                ))) :> IFpObject
                            with 
                                | _ ->
                                    FpNullObject() :> IFpObject
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
                            let ret = FpArrayObject()
                            let ls = p.[0] :?> FpArrayObject
                            try
                                ls.Values() |> List.find (fun x -> 
                                    (f1.Call [x]).IsTrue
                                )
                            with 
                                | _ ->
                                    FpNullObject() :> IFpObject
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
                            let ls = p.[0] :?> FpArrayObject
                            for x in ls.Values() do
                                f1.Call [x] |> ignore 
                            FpNullObject() :> IFpObject
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
                            let ret = FpArrayObject()
                            let ls = p.[0] :?> FpArrayObject
                            let ret2 = ls.Values() |> List.map (fun x -> 
                                    f1.Call [x]
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
                if args.Length = 2 && args.[0].Type = ObjectCategory.FpStringObject then
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
    
    static member ContainsKey = 
        (fun () ->  
            let fn (f : IFpObject list) = 
                let f1 = f.[0] :?> FpStringObject
                { 
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let ls = p.[0] :?> FpArrayObject
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
                            let ret = FpArrayObject()
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
                            let ret = FpArrayObject()
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
        base.Set("join", StringObject.Join )
        base.Set("replace", StringObject.Replace )
        base.Set("charAt", StringObject.charAt )
        base.Set("startsWith", StringObject.StartsWith )
        base.Set("endsWith", StringObject.EndsWith )
        base.Set("trimStart", StringObject.TrimStart )
        base.Set("trimEnd", StringObject.TrimEnd )

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
                let f2 = f.[0] :?> FpStringObject
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
                            let ret = FpArrayObject()
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
                            let ls = p.[0] :?> FpArrayObject
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
        | PipeToken -> 5
        | GtToken | LtToken | GteToken | LteToken | BindToken -> 4
        | AddToken -> 3
        | SubToken -> 3
        | MulToken -> 2
        | DiviToken -> 2
        | VirtualPipeToken -> 1
        | CommaToken -> 0
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
                    //   elif text.[index+1] = '-' then
                    //     AssignToken, (index+2)
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
                  | '|' ->
                      if text.[index+1] = '>' then
                        PipeToken, (index+2)
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
        |  PipeToken | VirtualPipeToken-> Call
        |  CommaToken -> Zip
        |  MulToken  -> Mul
        |  AddToken -> Add
        | SubToken -> Sub
        | DiviToken -> Divi
        | GtToken -> Gt
        | LtToken -> Lt
        | GteToken -> Gte
        | LteToken -> Lte
        | BindToken -> Eq

let except (parseState: ParseState) token =
    if parseState.moveNext() = token then
        ()
    else
        raise (Exception "")

let exceptWithComment (parseState: ParseState) token comment =
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
                    exceptWithComment parseState RightParenthesesToken "无参数的函数需要空元组"
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
                exceptWithComment parseState RightParenthesesToken "无参数的函数需要空元组"
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

    let parseBindStatement (parseState: ParseState) =
        parseState.moveNext() |> ignore
        match parseState.moveNext() with
            | IdenToken name ->
                match parseState.nextToken with
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
        let ops = parseExpressionBinaryNode parseState true |> sortExpressionBinary
        exceptWithComment parseState LeftBraceToken "if需要代码块"
        let opsBlock = parseStatement parseState
        exceptWithComment parseState RightBraceToken "if代码块需要闭合"
        match parseState.moveNext() with
                | ElseToken ->
                    exceptWithComment parseState LeftBraceToken "else需要代码块"
                    let opsElseBlock = parseStatement parseState
                    exceptWithComment parseState RightBraceToken "if代码块需要闭合"
                    ops 
                    @ [JumpIfFalse (opsBlock.Length + 2)] 
                    @ opsBlock 
                    @ [Jump (opsElseBlock.Length + 1)] 
                    @ opsElseBlock
                | _ ->
                    ops @ [JumpIfFalse (opsBlock.Length + 1)] @ opsBlock

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
        let ret1 = sort (sort ls 0) 1
        let sorted = (sortAll (joinCall(ret1)) 1)
        if sorted.Length = 0 then
            []
        else
            unstruct (sorted.[0])
            ret |> List.ofSeq

    let parseExpressionBinaryNode (parseState: ParseState) (ignoreOp: bool)  : OpOrToken list =
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
                        let ops = [LoadConst (FpTupleObject())]
                        [Op ops]
                    | _ ->
                        let ops = parseExpressionBinaryChild parseState
                        [Op ops]

            | IdenToken x ->
                parseState.moveNext() |> ignore
                match parseState.nextToken with
                    | DotToken ->
                        parseState.moveNext() |> ignore
                        parseExpressionGet parseState x
                    | _ ->
                        [Op [LoadVar x]]

            | NumberToken _ | StringToken _ ->
                parseState.moveNext() |> ignore
                let ops = getObjectByToken token
                [Op ops]
            | a when (isBinaryOpToken a) && (not ignoreOp) ->
                parseState.moveNext() |> ignore
                [Token a]
            | _ ->
                []

    let parseExpressionBinary (parseState: ParseState) : OpOrToken list =
        let rec loop () = 
            let ops = parseExpressionBinaryNode parseState false
            if ops.Length = 0 then
                ops
            else 
                ops @ loop()
        loop()

    let parseExpressionBinaryChild (parseState: ParseState) =
        let ops = parseExpression parseState
        except parseState RightParenthesesToken
        ops

    let parseExpressionBinarySquare (parseState: ParseState) =
        let ops = parseExpression parseState
        except parseState RightSquareToken
        ops

    let parseExpressionTuple (parseState: ParseState) =
        let rec parse  ()=
            let ops =  parseExpressionBinaryNode parseState true

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
                        ops @ [Token CommaToken] @ m
                    | RightSquareToken ->
                        ops
                    | _ ->
                        raise (Exception "右方括号需要闭合")
        let ops = parse ()
        exceptWithComment parseState RightSquareToken "右方括号需要闭合"
        (ops |> sortExpressionBinary) @ [LoadVar "list"; LoadConst (FpStringObject "create"); Get]  @ [Call]

    let parseExpressionGet (parseState: ParseState) (x: string)=
        let rec parse () =
            match parseState.moveNext() with
                | IdenToken name -> 
                    let ops = [LoadConst (FpStringObject name);Get]
                    match parseState.nextToken with
                        | AssignToken ->
                            parseState.moveNext() |> ignore
                            [LoadConst (FpStringObject name)] @ (parseExpression parseState) @ [Assign]
                        | DotToken ->
                            parseState.moveNext() |> ignore
                            ops @  parse ()
                        | _ ->
                            ops
                | LeftSquareToken ->
                    let opsValue = parseExpressionBinarySquare  parseState
                    let ops = opsValue @ [Get]
                    match parseState.nextToken with
                        | AssignToken ->
                            parseState.moveNext() |> ignore
                            opsValue @ (parseExpression parseState) @ [Assign]
                        | DotToken ->
                            parseState.moveNext() |> ignore
                            ops @  parse ()
                        | _ ->
                            ops
                | _ ->
                    raise (Exception "属性必须是字符串") 
        
        [Op (([LoadVar x]) @ (parse ()))]

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
        except parseState RightBraceToken
        (if index > 1 then
            ops @ [for x in [1 .. index-1] do yield Zip] 
         elif index = 1 then
            ops
        else 
            ops)  
            @ [ LoadVar "dict"; LoadConst (FpStringObject "create"); Get]  @ [Call]
    let parseExpression  (parseState: ParseState): Op list =
        parseExpressionBinary parseState |> sortExpressionBinary

    let parseExpressionInStatement  parseState =
        let ops = parseExpression parseState
        exceptWithComment parseState SemiToken "表达式需要;结尾"
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
        globalScope.Add("list", ArrayObject())
        globalScope.Add("dict", HashObject())
        globalScope.Add("tuple", TupleObject())
        globalScope.Add("string", StringObject())

    let eval (f:IFpCallable) =
        f.Call []


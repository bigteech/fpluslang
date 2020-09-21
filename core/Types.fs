module rec Types
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
    | AndToken
    | VirtualCommaToken
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

type ObjectCategory =
    | FpHashObject = 0
    | FpNumberObject = 1
    | FpStringObject = 2
    | FpBooleanObject = 3
    | FpFunctionObject = 4
    | FpTupleObject = 5
    | FpListObject = 6

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
    | Store of string
    | Unstruct of string list
    | Call
    | Zip
    | Function of Op list * string list
    | Exit
    | Gte
    | Eq
    | Lte
    | Gt
    | Lt
    | Or
    | And


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

    static member Add (x: FpHashObject, y:FpHashObject) =
        let ret = FpHashObject()
        for m in x.Keys() do
            ret.Set (m, x.Get(m))
        for m in y.Keys() do
            ret.Set (m, y.Get(m))
        ret

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
                    upcast (FpTupleObject.Empty())

    interface IFpHashable with
        member this.Get(p: string) =
            this.Get p

        member this.Set m =
            this.Set m
    interface IFpObject with
        member this.Type = ObjectCategory.FpHashObject
        member this.IsTrue with get() = true




type FpBooleanObject(v: bool)=
    interface IFpObject with
        member this.Type = ObjectCategory.FpBooleanObject
        member this.IsTrue = v


type FpListObject()=
    inherit FpHashObject();
   
    member this.Init(p: IFpObject list) =
        if p.Length = 0 then
            ()
        else
            for i=0 to (p.Length-1) do
                base.Set(i.ToString(), p.[i])
        ()

    static member Add (x: FpListObject, y:FpListObject) =
        let ret = FpListObject()
        ret.Init (x.Values() @ y.Values())
        ret

    interface IFpObject with
        member this.Type = ObjectCategory.FpListObject
 
type FpTupleObject()=
    inherit FpHashObject();
   
    let mutable freeze = false;

    static member Empty () =
        let ret = FpTupleObject()
        ret.Freeze()
        ret

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
        member this.IsTrue with get() = (this.Values.Length > 0)


type FpFunctionObject(argsNames: string list, getClosureVar: string -> IFpObject option) =
    let mutable oplst: Op list = []
    static let mutable globalScope = System.Collections.Generic.Dictionary<string, IFpObject>();
    static member SetGlobalscopt v =
        globalScope<- v
    member this.OpList with get() = oplst
    member this.PushToOpList(p) = oplst <- (oplst @ p);
    static member Add (f1: IFpCallable, f2: IFpCallable) =
        {
            new IFpCallable with
                member this.Call(args: IFpObject list): IFpObject =
                    let x = f1.Call args;
                    upcast (if x.Type = ObjectCategory.FpTupleObject then
                        f2.Call (x :?> FpTupleObject).Values
                    else
                        f2.Call [x])
                member this.Type = ObjectCategory.FpFunctionObject
                member this.IsTrue with get() = true
        } :> IFpObject



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
                            | ObjectCategory.FpListObject ->
                                FpListObject.Add(l2 :?> FpListObject, l1 :?> FpListObject) :> IFpObject
                            | ObjectCategory.FpHashObject ->
                                FpHashObject.Add(l2 :?> FpHashObject, l1 :?> FpHashObject) :> IFpObject
                            | ObjectCategory.FpFunctionObject ->
                                FpFunctionObject.Add(l2 :?> FpFunctionObject, l1 :?> FpFunctionObject) :> IFpObject

                        |>  stack.Push
                        1
                    | Sub ->
                        let l2 = stack.Pop() :?> FpNumberObject
                        let l1 = stack.Pop() :?> FpNumberObject
                        FpNumberObject(l1.Value - l2.Value) |>  stack.Push
                        1
                    | Or ->
                        let l2 = stack.Pop()
                        let l1 = stack.Pop()
                        FpBooleanObject(l1.IsTrue || l2.IsTrue) |>  stack.Push
                        1
                    | And ->
                        let l2 = stack.Pop()
                        let l1 = stack.Pop()
                        FpBooleanObject(l1.IsTrue && l2.IsTrue) |>  stack.Push
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
                            | ObjectCategory.FpBooleanObject ->
                                FpBooleanObject(l1.IsTrue = l2.IsTrue)
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
                    | Unstruct nameList ->
                        let target = stack.Pop()
                        match target.Type with
                            | ObjectCategory.FpHashObject ->
                                let hashTarget = target :?> FpHashObject
                                for name in nameList do
                                    scope.Add(name, hashTarget.Get(name))
                            | ObjectCategory.FpListObject ->
                                let listTarget = target :?> FpListObject
                                for index=0 to nameList.Length - 1 do
                                    scope.Add(nameList.[index], listTarget.Get(index.ToString()))
                            | ObjectCategory.FpTupleObject ->
                                let listTarget = target :?> FpTupleObject
                                for index=0 to nameList.Length - 1 do
                                    scope.Add(nameList.[index], listTarget.Get(index.ToString()))
                            | _ ->
                                raise (Exception "只有hash,list,tuple支持解构")
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
                        let l1 = stack.Pop()
                        let l3 = l1 :?> IFpHashable
                       
                        if l2.Type = ObjectCategory.FpStringObject then
                            stack.Push (l3.Get (l2 :?> FpStringObject).Value)
                        else
                            stack.Push (l3.Get ((l2 :?> FpNumberObject).Value.ToString()))
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

module App
open System
open Fable.Core.JsInterop
open Fetch
open Fable.Import
open Fp
open Fable.Core.Util
open Fable.Core

[<Emit("$0[$1]")>]
let getAttr (x: Object) (y: string) = jsNative
[<Emit("$0.apply(window, $1)")>]
let jsCall (x: Object) (y: Object list): Object = jsNative
[<Emit("typeof $0")>]
let getType (x: Object): string = jsNative
[<Emit("$0")>]
let toAny (x: Object) = jsNative
[<Emit("$.ajax({type: \"GET\",url: $0,async: false}).responseText")>]
let fetchCode (x: string): string = jsNative


module rec Main =
    let rec ConvertFpObject2JsObject (p: IFpObject) =
        match p.Type with 
            | ObjectCategory.FpFunctionObject -> 
                toAny (fun x -> 
                    (p :?> IFpCallable).Call [JSObject x]
                )
            | ObjectCategory.FpHashObject ->
                let m = (p :?> FpHashObject)
                let kvs = new Collections.Generic.Dictionary<string, IFpObject>();
                for x in m.Keys() do
                    kvs.Add(x, ConvertFpObject2JsObject((m.Get x)))
                toAny (kvs)
            | ObjectCategory.FpStringObject ->
                toAny ((p :?> FpStringObject).Value)
            | ObjectCategory.FpNullObject ->
                toAny (null)
            | ObjectCategory.FpNumberObject ->
                toAny ((p :?> FpNumberObject).Value)
            | ObjectCategory.FpBooleanObject ->
                toAny (p.IsTrue)
            | ObjectCategory.FpTupleObject ->
                let m = (p :?> FpTupleObject)
                toAny ([for x in m.Keys() do yield ConvertFpObject2JsObject((m.Get x))])
            | _ ->
                raise (Exception "无法转换的类型")
    type JSObject (obj: Object) = 
        member this.GetRawObj () = obj

        interface IFpHashable with
            member this.Get(p: string): IFpObject = 
                let v = getAttr obj p
                let tp = getType v
                match tp with
                    | "string" -> 
                        (FpStringObject v) :> IFpObject
                    | "number" -> 
                        (FpNumberObject (int32 (v))) :> IFpObject
                    | "object" -> 
                        (JSObject v) :> IFpObject
                    | "function" -> 
                        {
                            new IFpCallable with 
                                member this.Call(args: IFpObject list): IFpObject =
                                    let pa = ([for x in args do yield ConvertFpObject2JsObject(x)])?toJSON()
                                    JSObject(jsCall v ([for x in args do yield ConvertFpObject2JsObject(x)]?toJSON())) :> IFpObject
                                member this.Type = ObjectCategory.FpFunctionObject
                                member this.IsTrue with get() = true
                        } :> IFpObject
                        
                    | _ -> 
                        FpNullObject() :> IFpObject
            member this.Set m =
                raise (Exception "不能改变内置对象")
        interface IFpObject with 
            member this.Type = ObjectCategory.FpHashObject
            member this.IsTrue with get() = true

type DocumentObject () =
    inherit FpHashObject();
    do
        base.Set("createElement", DocumentObject.CreateElement)
        base.Set("getAttr", DocumentObject.GetAttr)
        base.Set("setAttr", DocumentObject.SetAttr)
        base.Set("addListener", DocumentObject.AddListener)
        base.Set("getProp", DocumentObject.GetProp)
        base.Set("append", DocumentObject.Append)
        base.Set("getElementById", DocumentObject.GetElementById)
        base.Set("getElementsByClassName", DocumentObject.GetElementByClassName)
        base.Set("getElementsByTagName", DocumentObject.GetElementByTagName)
        base.Set("body", DocumentObject.Body)
    static member GetProp = 
        (fun () ->
            let fn (key : IFpObject list) = 
                { 
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (element: IFpObject list) =
                            let el = element.[0] :?> Main.JSObject
                            let p = el.GetRawObj()
                            let key = (key.[0] :?> FpStringObject).Value
                            (FpStringObject (getAttr p key)) :> IFpObject
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
    static member SetAttr = 
        (fun () ->
            let fn (key : IFpObject list) = 
                { 
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (element: IFpObject list) =
                            let el = element.[0] :?> Main.JSObject
                            let p = el.GetRawObj() :?> Browser.Types.Element
                            let k = (key.[0] :?> FpStringObject).Value
                            let v = (key.[1] :?> FpStringObject).Value
                            p.setAttribute (k,v)
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

    static member GetAttr = 
        (fun () ->
            let fn (key : IFpObject list) = 
                { 
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (element: IFpObject list) =
                            let el = element.[0] :?> Main.JSObject
                            let p = el.GetRawObj() :?> Browser.Types.Element
                            let key = (key.[0] :?> FpStringObject).Value
                            (FpStringObject (p.getAttribute key)) :> IFpObject
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
    static member GetElementByTagName = 
        (fun () ->
            (
                {
                    new IFpCallable with 
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = 
                            let name = p.[0] :?> FpStringObject
                            let nodes = Browser.Dom.document.getElementsByTagName(name.Value)
                            let mutable ret2 = []
                            for x = 0 to nodes.length - 1 do
                                ret2 <- (Main.JSObject(nodes.Item(x)) :> IFpObject) :: ret2
                            let ret = FpListObject() 
                            ret.Init ret2
                            ret :> IFpObject
                }
            ) :> IFpObject
        )()
    static member GetElementByClassName = 
        (fun () ->
            (
                {
                    new IFpCallable with 
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = 
                            let name = p.[0] :?> FpStringObject
                            let nodes = Browser.Dom.document.getElementsByClassName(name.Value)
                            let mutable ret2 = []
                            for x = 0 to nodes.length - 1 do
                                ret2 <- (Main.JSObject(nodes.Item(x)) :> IFpObject) :: ret2
                            let ret = FpListObject() 
                            ret.Init ret2
                            ret :> IFpObject
                }
            ) :> IFpObject
        )()
    static member AddListener = 
        (fun () ->
            let fn (key : IFpObject list) = 
                { 
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (element: IFpObject list) =
                            let el = element.[0] :?> Main.JSObject
                            let p = el.GetRawObj() :?> Browser.Types.Element
                            let name = (key.[0] :?> FpStringObject).Value
                            let fnv = key.[1] :?> IFpCallable
                            p.addEventListener(name, (fun y -> 
                                fnv.Call ([Main.JSObject(y)]) |> ignore
                            ))
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

    static member GetElementById = 
        (fun () ->
            (
                {
                    new IFpCallable with 
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = 
                            let name = p.[0] :?> FpStringObject
                            Main.JSObject (Browser.Dom.document.getElementById(name.Value)) :> IFpObject
                }
            ) :> IFpObject
        )()
    static member Body  with get() = (Main.JSObject Browser.Dom.document.body) :> IFpObject
        
    static member Append = 
        (fun () ->
            let fn (parent : IFpObject list) = 
                { 
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (child: IFpObject list) =
                            let f1 = parent.[0] :?> Main.JSObject
                            let p = f1.GetRawObj() :?> Browser.Types.Element
                            let chil = ((child.[0] :?> Main.JSObject).GetRawObj()):?> Browser.Types.Element
                            p.appendChild chil |> ignore
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
    static member CreateElement = 
        (fun () ->
            let fn (tag : IFpObject list) (attr : IFpObject list)= 
                let f1 = tag.[0] :?> FpStringObject
                let f2 = attr.[0] :?> FpHashObject 
                { 
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (p: IFpObject list) =
                            let children = p.[0] :?> FpListObject
                            let el = Browser.Dom.document.createElement (f1.Value) 
                            for x in f2.Keys() do
                                let v = f2.Get(x)
                                if v.Type = ObjectCategory.FpStringObject then
                                    el.setAttribute (x, f2.Get(x).ToString())
                                    ()
                                else
                                    match x with
                                        | "onclick" -> 
                                            el.addEventListener("click", (fun y -> 
                                                let onclick = f2.Get(x) :?> IFpCallable
                                                onclick.Call ([Main.JSObject(y)]) |> ignore
                                            ))
                                        | _ -> ()
                                    ()
                            for x in children.Values() do
                                if x.Type = ObjectCategory.FpStringObject then
                                    el.textContent <- x.ToString()
                                else
                                    el.appendChild ((x :?> Main.JSObject).GetRawObj() :?> Browser.Types.Node) |> ignore
                            Main.JSObject(el) :> IFpObject
                } :> IFpObject
            let fn1 (f : IFpObject list) = 
                {
                    new IFpCallable with 
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = fn f p
                } :> IFpObject
            (
                {
                    new IFpCallable with 
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member x.Call (p: IFpObject list) = fn1 p
                }
            ) :> IFpObject
        )()
type WindowObject () =
    inherit FpHashObject();
    do
        base.Set("goto", WindowObject.Goto)

    static member Goto = 
        (
            {
                new IFpCallable with 
                    member this.Type = ObjectCategory.FpFunctionObject
                    member this.IsTrue with get() = true
                    member x.Call (p: IFpObject list) = 
                        let url = (p.[0] :?> FpStringObject).Value
                        Browser.Dom.window.``open`` url |> ignore
                        FpNullObject() :> IFpObject
            }
        ) :> IFpObject

type ImportFunction () =
    interface IFpCallable with
        member this.Call(args: IFpObject list): IFpObject =
            let ret = fetchCode (args.[0] :?> FpStringObject).Value
            let ops = Parser.parseSourceElement ret
            let f = FpFunctionObject([], (fun x -> None))
            f.PushToOpList ops
            (Vm.eval f) :?> IFpObject

    interface IFpObject with
        member this.Type = ObjectCategory.FpFunctionObject
        member this.IsTrue with get() = true


addGlobalObject "document" (Main.JSObject(Browser.Dom.document))
addGlobalObject "window" (Main.JSObject(Browser.Dom.window))
addGlobalObject "windowHelper" (WindowObject())
addGlobalObject "documentHelper" (DocumentObject())
addGlobalObject "import" (ImportFunction())

fetch "./main.fp" [] // use the fetch api to load our resource
    |> Promise.bind (fun res -> res.text()) // get the resul
    |> Promise.map (fun txt -> // bind the result to make further operation
        let ops = Parser.parseSourceElement txt
        let f = FpFunctionObject([], (fun x -> None))
        f.PushToOpList ops
        Vm.init ()
        Vm.eval f |> ignore)
    |> ignore

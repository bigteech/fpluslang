module App
open System
open Fable.Core.JsInterop
open Fetch
open Fable.Import
open Fp
open Fable.Core.Util
open Fable.Core


let AlertFunction ()=
    {
        new IFpCallable with 
            member this.Call(args: IFpObject list): IFpObject =
                let message = (args.[0] :?> FpStringObject).Value
                Browser.Dom.window.alert message
                FpNullObject() :> IFpObject

            member this.Type = ObjectCategory.FpFunctionObject
            member this.IsTrue with get() = true
    }

addGlobalObject "alert" (AlertFunction())

[<Emit("$0.[$1]")>]
let getAttr (x: Object) (y: string): string = jsNative

[<Emit("typeof x")>]
let getType (x: Object): string = jsNative

let rec jsObject2FpObject (x: Object) =
    {
        new IFpHashable with
            member this.Get(p: string): IFpObject = 
                let v = getAttr x p
                let tp = getType v
                match tp with
                    | "string" -> 
                        (FpStringObject v) :> IFpObject
                    | "object" -> 
                        (jsObject2FpObject v) :> IFpObject
                    | _ -> 
                        FpNullObject() :> IFpObject
            member this.Set m =
                raise (Exception "不能改变内置对象")

        interface IFpObject with 
            member this.Type = ObjectCategory.FpHashObject
            member this.IsTrue with get() = true
    }

type El (value: Browser.Types.Element) =

    member this.GetValue () = value

    interface IFpObject with 
        member this.Type = ObjectCategory.FpHashObject
        member this.IsTrue with get() = true

type DocumentObject () =
    inherit FpHashObject();
    do
        base.Set("createElement", DocumentObject.CreateElement)
        base.Set("getAttr", DocumentObject.GetAttr)
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
                            let el = element.[0] :?> El
                            let p = el.GetValue()
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
    static member GetAttr = 
        (fun () ->
            let fn (key : IFpObject list) = 
                { 
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (element: IFpObject list) =
                            let el = element.[0] :?> El
                            let p = el.GetValue()
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
                                ret2 <- (El(nodes.Item(x)) :> IFpObject) :: ret2
                            let ret = FpArrayObject() 
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
                                ret2 <- (El(nodes.Item(x)) :> IFpObject) :: ret2
                            let ret = FpArrayObject() 
                            ret.Init ret2
                            ret :> IFpObject
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
                            El (Browser.Dom.document.getElementById(name.Value)) :> IFpObject
                }
            ) :> IFpObject
        )()
    static member Body  with get() = (El Browser.Dom.document.body) :> IFpObject
        
    static member Append = 
        (fun () ->
            let fn (parent : IFpObject list) = 
                { 
                    new IFpCallable with
                        member this.Type = ObjectCategory.FpFunctionObject
                        member this.IsTrue with get() = true
                        member this.Call (child: IFpObject list) =
                            let f1 = parent.[0] :?> El
                            let p = f1.GetValue()
                            let chil = ((child.[0] :?> El).GetValue())
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
                            let children = p.[0] :?> FpArrayObject
                            let el = Browser.Dom.document.createElement (f1.Value) 
                            for x in f2.Keys() do
                                let v = f2.Get(x)
                                if v.Type = ObjectCategory.FpStringObject then
                                    el.setAttribute (x, f2.Get(x).ToString())
                                    ()
                                else
                                    match x with
                                        | "onclick" -> 
                                            el.onclick <- (fun y -> 
                                                let onclick = f2.Get(x) :?> IFpCallable
                                                onclick.Call ([jsObject2FpObject(y)]) |> ignore
                                            )
                                            ()
                                        | _ -> ()
                                    ()
                            for x in children.Values() do
                                if x.Type = ObjectCategory.FpStringObject then
                                    el.textContent <- x.ToString()
                                else
                                    el.appendChild ((x :?> El).GetValue()) |> ignore
                            El(el) :> IFpObject
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

addGlobalObject "document" (DocumentObject())


fetch "./main.fp" [] // use the fetch api to load our resource
    |> Promise.bind (fun res -> res.text()) // get the resul
    |> Promise.map (fun txt -> // bind the result to make further operation
        let ops = Parser.parseSourceElement txt
        let f = FpFunctionObject([], (fun x -> None))
        f.PushToOpList ops
        Vm.init ()
        Vm.eval f |> ignore)
    |> ignore

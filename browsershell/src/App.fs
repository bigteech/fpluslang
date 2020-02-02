module App
open System
open Fable.Core.JsInterop
open Fetch
open Fable.Import
open Fp


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


fetch "./main.fp" [] // use the fetch api to load our resource
    |> Promise.bind (fun res -> res.text()) // get the resul
    |> Promise.map (fun txt -> // bind the result to make further operation
        let ops = Parser.parseSourceElement txt
        let f = FpFunctionObject([], (fun x -> None))
        f.PushToOpList ops
        Vm.init ()
        Vm.eval f |> ignore)
    |> ignore

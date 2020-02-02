module App
open System
open Fable.Core.JsInterop
open Fetch
open Fable.Import
open Fp



fetch "./main.fp" [] // use the fetch api to load our resource
    |> Promise.bind (fun res -> res.text()) // get the resul
    |> Promise.map (fun txt -> // bind the result to make further operation
        let ops = Parser.parseSourceElement txt
        let f = FpFunctionObject([])
        f.PushToOpList ops
        Vm.eval f |> ignore)
    |> ignore

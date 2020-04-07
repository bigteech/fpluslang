// Learn more about F# at http://fsharp.org

open System
open Fp


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

addGlobalObject "file" (FileHashObject())


let main () =
   let ops = Parser.parseSourceElement (String.Join("\n", IO.File.ReadLines("./test.fp")))
   let f = FpFunctionObject([], (fun x -> None))
   f.PushToOpList ops
   Vm.init ()
   Vm.eval f |> ignore
   0

main () |> ignore

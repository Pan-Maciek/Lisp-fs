module rec Runtime

open Parse
open System.Collections.Generic

type Context(root: Context option) =
  let macros: Dictionary<string, (Context * AST -> Context * AST)> = Dictionary()
  let modules: Dictionary<string, Module> = Dictionary()

  member _.DefineMacro name transform = macros.Add (name, transform)
  member _.Macro (name: string): (Context * AST -> Context * AST) option = 
    match macros.TryGetValue name with
    | true, fn -> Some fn
    | false, _ -> 
      match root with
      | None -> None
      | Some rootModule -> rootModule.Macro name
  member _.DefineFunction (name: string) (fn: Function) = ()
  member _.DefineModule (name: string) () = ()

type Module(root: Module option, name: string) =
  let context = Context(None)

  member _.Name = name
  member _.FullyQualifiedName = 
    match root with
    | None -> name
    | Some rootModule -> rootModule.FullyQualifiedName + "." + name
  member _.ModuleContext = context  

type Function =
  | NativeFunction of string
  | UserFunction of AST

let defn (context: Context) (ast: AST): AST =
  let functionContext = Context(Some context)
  let rec defineFunction ast = 
    match ast with
    | [] -> ()
    | args :: body :: tail -> printfn "%A" args
    | _ -> failwith "Expected function body"
  Empty

let def (context: Context) (ast: AST): AST =
  Empty

let GlobalModule = Module(None, "global")
let GlobalContext = GlobalModule.ModuleContext
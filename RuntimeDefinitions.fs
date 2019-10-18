module rec Runtime

open Parse
open System.Collections.Generic

type Context(root: Context option) =
  let macros: Dictionary<string, (Context * AST -> Context * AST)> = Dictionary()
  let modules: Dictionary<string, Module> = Dictionary()
  let functions: Dictionary<string, Function> = Dictionary()

  member _.DefineMacro name transform = macros.Add (name, transform)
  member _.Macro (name: string): (Context * AST -> Context * AST) option = 
    match macros.TryGetValue name with
    | true, fn -> Some fn
    | false, _ -> 
      match root with
      | None -> None
      | Some root -> root.Macro name
  member _.Macros = macros.Keys

  member _.DefineModule (name: string) (module': Module) = modules.Add (name, module')
  member _.Module (name: string): Module option = 
    match modules.TryGetValue name with
    | true, module' -> Some module'
    | false, _ ->
      match root with
      | None -> None
      | Some root -> root.Module name
  member _.Modules = modules.Keys

  member _.DefineFunction (name: string) (fn: Function) = functions.Add (name, fn)

  member _.Functions = functions.Keys

type Module(root: Context option, name: string) =
  inherit Context(None)

  member _.Name = name
  member _.FullyQualifiedName = 
    match root with
    | None -> name
    | Some rootModule -> 
      match rootModule with
      | :? Module as rootModule -> rootModule.FullyQualifiedName + "." + name
      | _ -> name

type Function =
  | NativeFunction of string
  | UserFunction of AST list * AST

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
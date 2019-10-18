module rec Runtime

open Parse
open Util
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
  member _.Macros = macros.Values

  member _.DefineModule (name: string) (module': Module) = modules.Add (name, module')
  member _.Module (name: string): Module option = 
    match modules.TryGetValue name with
    | true, module' -> Some module'
    | false, _ ->
      match root with
      | None -> None
      | Some root -> root.Module name
  member _.Modules = modules.Values

  member _.DefineFunction (name: string) (fn: Function) = functions.Add (name, fn)
  member _.Function (name: string): Function option =
    match functions.TryGetValue name with
    | true, fn -> Some fn
    | flase, _ -> 
      match root with
      | None -> None
      | Some root -> root.Function name
  member _.Functions = functions.Values

  override _.ToString () = "(Context)"

type Module(root: Context option, name: string) =
  inherit Context(None)

  member _.Name = name
  member _.FullyQualifiedName = 
    match root with
    | None -> name
    | Some root -> 
      match root with
      | :? Module as root -> root.FullyQualifiedName + "." + name
      | _ -> name
  override self.ToString () = "(" + self.FullyQualifiedName + ")"

type Function(root: Context option) =
  inherit Context(root)

type UserFunction(root: Context, name: string, ast: AST list) =
  inherit Function(None)

  member _.Name = name
  member _.FullyQualifiedName =
    match root with
    | :? Module as root -> root.FullyQualifiedName + "::" + name
    | :? UserFunction as root -> root.FullyQualifiedName + "::" + name
    | _ -> name

type NativeFunction(fullyQualifiedName: string) =
  inherit Function(None)

  member _.FullyQualifiedName = fullyQualifiedName

let GlobalModule = Module(None, "Global")
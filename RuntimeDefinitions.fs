module rec Runtime

open Parse
open Util
open System.Collections.Generic

type Context(root: Context option) =
  let macros: Dictionary<string, (Context * AST -> Context * AST)> = Dictionary()
  let modules: Dictionary<string, Module> = Dictionary()
  let functions: Dictionary<string, Function> = Dictionary()
  let expressions: List<AST> = List()

  member _.DefineMacro name transform = macros.Add (name, transform)
  member _.Macro (name: string): (Context * AST -> Context * AST) option = 
    match macros.TryGetValue name with
    | true, fn -> Some fn
    | _ -> 
      match root with
      | None -> None
      | Some root -> root.Macro name
  member _.Macros = List.ofSeq macros.Values

  member _.DefineModule (name: string) (module': Module) = modules.Add (name, module')
  member _.Module (name: string): Module option = 
    match modules.TryGetValue name with
    | true, module' -> Some module'
    | _ ->
      match root with
      | None -> None
      | Some root -> root.Module name
  member _.Modules = List.ofSeq modules.Values

  member _.DefineFunction (name: string) (fn: Function) = functions.Add (name, fn)
  member _.Function (name: string): Function option =
    match functions.TryGetValue name with
    | true, fn -> Some fn
    | _ -> 
      match root with
      | None -> None
      | Some root -> root.Function name
  member _.Functions = List.ofSeq functions.Values
  member _.UserFunctions: UserFunction list = 
    functions.Values
    |> Seq.map toUserFunction 
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> List.ofSeq

  member _.AddExpression (expr: AST) = expressions.Add expr
  member _.Expressions = List.ofSeq expressions

  override _.ToString () = "(Context)"

type Module(root: Context option, name: string) =
  inherit Context(root)

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

let toUserFunction (fn: Function): UserFunction option = 
  match fn with
  | :? UserFunction as fn -> Some fn
  | _ -> None

type UserFunction(root: Context, name: string, args: string list, body: AST) as self =
  inherit Function(Some root)

  do
    self.AddExpression body

  member _.Name = name
  member _.FullyQualifiedName =
    match root with
    | :? Module as root -> root.FullyQualifiedName + "::" + name
    | :? UserFunction as root -> root.FullyQualifiedName + "::" + name
    | _ -> name
  member _.Args: string list = args

type NativeFunction(fullyQualifiedName: string, code: string) =
  inherit Function(None)

  member _.Name = fullyQualifiedName
  member _.Code = code

let GlobalModule = Module(None, "Global")
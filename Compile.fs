module rec Compile

open Parse
open Runtime

let moduleMacro (context: Context, ast: AST) =
  match ast with
  | List (Token name :: body) -> 
    let module' = Module(Some context, name)
    context.DefineModule name module'
    let ast = resolveMacros module' (List body)
    context, ast
  | _ -> failwith "Unexpected arguments to module macro"

let defnMacro (context: Context, ast: AST) =
  match ast with
  | List [Token name ; List args ; List body] ->
    match context with
    | :? Module as module' -> context.DefineFunction name (UserFunction (module', name, body))
    | :? UserFunction as function' -> context.DefineFunction name (UserFunction (function', name, body))
    | _ -> failwith "Function must be defined inside module or another user defined function"
    context, ast
  | _ -> failwith "Unexpected arguments to defn macro"

let setupRuntime () =
  GlobalModule.DefineMacro "module" moduleMacro
  GlobalModule.DefineMacro "defn" defnMacro
  GlobalModule.DefineFunction "print" (NativeFunction ("System.Console.Write"))

let rec resolveMacros (context: Context) (ast: AST) =
  match ast with
  | List (Token token::tail) -> 
    match context.Macro token with
    | None -> ast
    | Some macro -> 
      let context, ast = macro (context, List tail)
      resolveMacros context ast
  | List el -> List (el |> List.map (resolveMacros context))
  | Token t -> Token t
  | Empty -> Empty

let (|FunctionCall|_|) (ast: AST, context: Context) =
  match ast with
  | List (Token name :: tail) -> 
    match context.Function name with
    | Some fn -> Some (fn, List.map (compileExpr context) tail)
    | None -> None
  | _ -> None

let compileExpr (context: Context) (ast: AST) =
  match ast, context with
  | Token t, _ -> t
  | FunctionCall (fn, args) -> 
    match fn with
    | :? UserFunction as fn -> fn.FullyQualifiedName + "(" + String.concat ", " args + ")"
    | :? NativeFunction as fn -> fn.FullyQualifiedName + "(" + String.concat ", " args + ")"
    | _ -> "" 
  | List ops, _ -> String.concat " " (List.map (compileExpr context) ops)
  | _ -> failwith ""
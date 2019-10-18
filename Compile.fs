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
    context.DefineFunction name (UserFunction (args, List body))
    context, ast
  | _ -> failwith "Unexpected arguments to module defn"

let setupRuntime () =
  GlobalModule.DefineMacro "module" moduleMacro
  GlobalModule.DefineMacro "defn" defnMacro

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


module Compile

open Parse
open Runtime

// GlobalContext.DefineMacro "module" 
let moduleMacro (context: Context, ast: AST) =
  printfn "Module Macro!"
  match ast with
  | List (Token name :: tail) -> 
    context.DefineModule name ()
    context, List tail
  | _ -> failwith "Unexpected arguments to module macro"

let setupRuntime () =
  GlobalContext.DefineMacro "module" moduleMacro
  printfn "test"

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


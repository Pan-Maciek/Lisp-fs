module rec Compile

open Parse
open Runtime

let moduleMacro (context: Context, ast: AST) =
  match ast with
  | List (Token name :: body) -> 
    let module' = Module(Some context, name)
    context.DefineModule name module'
    let ast = resolveMacros module' (List body)
    context, Empty
  | _ -> failwith "Unexpected arguments to module macro"

let takeToken (ast: AST): string option = 
  match ast with
  | Token t -> Some t
  | _ -> None

let defnMacro (context: Context, ast: AST) =
  match ast with
  | List [Token name ; List args ; List body] ->
    let args = args |> Seq.map takeToken |> Seq.filter Option.isSome |> Seq.map Option.get |> List.ofSeq
    match context with
    | :? Module as module' -> context.DefineFunction name (UserFunction (module', name, args, List body))
    | :? UserFunction as function' -> context.DefineFunction name (UserFunction (function', name, args, List body))
    | _ -> failwith "Function must be defined inside module or another user defined function"
    context, Empty
  | _ -> failwith "Unexpected arguments to defn macro"

let commentMacro (context: Context, ast: AST) =
  context, Empty

let setupRuntime () =
  GlobalModule.DefineMacro "module" moduleMacro
  GlobalModule.DefineMacro "defn" defnMacro
  GlobalModule.DefineMacro "--" commentMacro
  GlobalModule.DefineFunction "print" (NativeFunction ("print", "(...args) { return console.log(...args) }"))
  GlobalModule.DefineFunction "+" (NativeFunction ("+", "(...args) { return args.reduce((acc, x) => acc + x) }"))
  GlobalModule.DefineFunction "-" (NativeFunction ("-", "(...args) { return args.reduce((acc, x) => acc - x) }"))
  GlobalModule.DefineFunction "empty?" (NativeFunction ("empty?", "(arg) { return arg.length == 0 }"))

let rec resolveMacros (context: Context) (ast: AST) =
  match ast with
  | List (Token token::tail) -> 
    match context.Macro token with
    | None -> ast
    | Some macro -> 
      let context, ast = macro (context, List tail)
      resolveMacros context ast
  | List el -> 

    List.iter context.AddExpression (el |> List.map (resolveMacros context))
    Empty
  | Token t -> Token t
  | Empty -> Empty

let (|FunctionCall|_|) (ast: AST, context: Context) =
  match ast with
  | List (Token name :: tail) -> 
    printfn "%A %A %A" name tail (context.Function name)
    printfn "%A" context
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
    | :? NativeFunction as fn -> fn.Name + "(" + String.concat ", " args + ")"
    | _ -> failwith "unexpected function"
  | List ops, _ -> "[" + String.concat ", " (List.map (compileExpr context) ops) + "]"
  | Empty, _ -> ""
  | _ -> failwith ""


let compileModule (module': Module) = 
  sprintf """[`%s`]: {%s}""" 
<| module'.FullyQualifiedName
<| String.concat "," (List.filter (System.String.IsNullOrWhiteSpace >> not) [
  String.concat "," (List.map compileModule module'.Modules)
  String.concat "," (List.map compileFunction module'.Functions)
])

let compileFunction (func: Function) =
  match func with
  | :? UserFunction as func -> compileUserFunction func
  | :? NativeFunction as func -> sprintf "[`%s`]%s" func.Name func.Code
  | _ -> failwith "Unexpected function type"

let compileUserFunction (func: UserFunction) =
  sprintf """[`%s`](%s){return inject(c, 'a', 'b', ()=>%s)}"""
<| func.Name
<| (String.concat ", " func.Args)
<| (String.concat "," (List.map (compileExpr func) func.Expressions))

let compile (context: Context): string =
  match context with
  | :? Module as module' -> compileModule module'
  | :? UserFunction as userFunction -> "/*UserFunction*/"
  | _ -> failwith ""
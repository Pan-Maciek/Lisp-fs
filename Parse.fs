module Parse

open System.Collections.Generic
open Lex

type AST =
  | Token of string
  | List of AST list
  | Empty

let (|SeqEmpty|SeqCons|) (xs: 'a seq) =
  if Seq.isEmpty xs then SeqEmpty
  else SeqCons(Seq.head xs, Seq.skip 1 xs)

let parse tokens = 
  let rec parse acc tokens = 
    match tokens with
    | SeqEmpty -> List.rev acc, tokens
    | SeqCons (OpenList, tail) -> 
      let tokens, tail = parse [] tail
      parse (List tokens :: acc) tail
    | SeqCons (CloseList, tail) -> List.rev acc, tail
    | SeqCons (GeneralToken t, tail) -> parse (Token t :: acc) tail
    | SeqCons (StringToken s, tail) -> parse (Token s :: acc) tail
  let ast, _ = parse [] tokens
  ast

type Context(root: Context option) =
  let macros: Dictionary<string, (Context -> AST -> AST)> = Dictionary()

  member _.DefineMacro name transform = macros.Add (name, transform)
  member _.Macro (name: string): (Context -> AST -> AST) option = 
    match macros.TryGetValue name with
    | true, fn -> Some fn
    | false, _ -> 
      match root with
      | None -> None
      | Some rootModule -> rootModule.Macro name
  member _.DefineFunction (name: string) (ast: AST) = ()


type Module(root: Module option, name: string) =
  let context = Context(None)

  member _.Name = name
  member _.FullyQualifiedName = 
    match root with
    | None -> name
    | Some rootModule -> rootModule.FullyQualifiedName + "." + name
  member _.ModuleContext = context  



// type Function(context: Context, name: string) =

let Global = Module(None, "global")
let Math = Module(Some Global, "math")

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

let GlobalContext = Global.ModuleContext
GlobalContext.DefineMacro "defn" defn
GlobalContext.DefineMacro "def" def
// GlobalContext.def
// defn GlobalContext "+" 
module Parse

open Lex
open Util

type AST =
  | Token of string
  | List of AST list
  | Empty


let parse tokens = 
  let rec parse acc tokens = 
    match tokens with
    | SeqEmpty -> List.rev acc, tokens
    | SeqCons (OpenList, tail) -> 
      let tokens, tail = parse [] tail
      parse (List tokens :: acc) tail
    | SeqCons (CloseList, tail) -> List.rev acc, tail
    | SeqCons (GeneralToken t, tail) -> parse (Token t :: acc) tail
    | SeqCons (StringToken s, tail) -> parse (Token ("`" + s + "`") :: acc) tail
  let ast, _ = parse [] tokens
  ast
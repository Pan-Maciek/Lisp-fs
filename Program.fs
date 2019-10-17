open System

open Lex
open Parse
open System.IO

[<EntryPoint>]
let main argv = 
  let file = File.ReadAllText argv.[0]
  let tokens = tokenize file
  let ast = parse tokens
  printfn "%A" tokens
  printfn "%A" ast
  0 // return an integer exit code

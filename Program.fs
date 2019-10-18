open System

open Lex
open Parse
open Compile
open Runtime
open System.IO

[<EntryPoint>]
let main argv = 
  let file = File.ReadAllText argv.[0]
  let tokens = tokenize file
  let ast = parse tokens
  setupRuntime ()
  let macrosResolved = resolveMacros GlobalContext (List ast)

  printfn "{%A}" ast
  printfn "{%A}" macrosResolved
  printfn "{%A}" GlobalContext
  0 // return an integer exit code

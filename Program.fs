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
  let macrosResolved = resolveMacros GlobalModule (List ast)

  printfn "{%A}" ast
  printfn "{%A}" macrosResolved
  printfn "Global {%A}" GlobalModule
  let TestModule = GlobalModule.Module "Test"
  printfn "Test {%A}" TestModule
  
  0 // return an integer exit code

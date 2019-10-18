open System

open Lex
open Parse
open Compile
open Runtime
open System.IO

[<EntryPoint>]
let main argv = 
  let fileInPath = argv.[0]
  let fileOutPath = Path.ChangeExtension (fileInPath, "js")
  let file = File.ReadAllText fileInPath
  let tokens = tokenize file
  let ast = parse tokens
  setupRuntime ()
  let macrosResolved = resolveMacros GlobalModule (List ast)
  let code = "const runtime = {" + compile GlobalModule + "}"

  File.WriteAllText (Path.ChangeExtension (fileInPath, "ast"), macrosResolved.ToString())
  File.WriteAllText (fileOutPath, code)
  
  0 // return an integer exit code

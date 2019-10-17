module Lex

open System.Text
open System

type Token = 
  | StringToken of string
  | GeneralToken of string
  | OpenList | CloseList

type Reader(input: string) =
  let mutable index = 0
  let mutable buffer = StringBuilder(1024)

  member _.Current = input.[index]
  member _.Previous = input.[index - 1]
  member _.Continue = index < input.Length
  member _.Buffer (s: char) = buffer.Append (s) |> ignore
  member _.Buffered = buffer.ToString ()

  member _.ClearBuffer = buffer.Clear >> ignore
  member _.Advance () = index <- index + 1


let takeString (r: Reader) (stop: char) =
  r.ClearBuffer ()

  let rec takeString (): unit =
    r.Advance ()

    if r.Continue then
      match r.Current with
      | '\\' -> 
        r.Advance ()
        if r.Current <> stop then r.Buffer '\\' 
        r.Buffer r.Current; takeString ()
      | c when c = stop -> r.Advance ()
      | c -> r.Buffer c; takeString ()
    else failwith "Unexpected end of source in string."    

  takeString (); r.Buffered

let isTokenChar c = c <> '(' && c <> ')' && not <| Char.IsWhiteSpace c

let takeToken (r: Reader) =
  r.ClearBuffer ()
  while r.Continue && r.Current |> isTokenChar do
    r.Buffer r.Current; r.Advance ()
  r.Buffered

let tokenize (source: string) = seq {
    let r = Reader source
    while r.Continue do
      match r.Current with
      | ''' -> yield StringToken (takeString r ''')
      | '(' -> r.Advance(); yield OpenList
      | ')' -> r.Advance(); yield CloseList
      | c when Char.IsWhiteSpace c -> r.Advance ()
      | _ -> yield GeneralToken (takeToken r)
  }
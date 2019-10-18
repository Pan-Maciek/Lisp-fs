module Util

let (|SeqEmpty|SeqCons|) (xs: 'a seq) =
  if Seq.isEmpty xs then SeqEmpty
  else SeqCons(Seq.head xs, Seq.skip 1 xs)
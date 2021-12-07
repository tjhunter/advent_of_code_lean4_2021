import MyAdvent.AdventStd

open Std
open Std2

def count_incr : List Int -> Int
| x1 :: x2 :: l => (if x1 < x2 then 1 else 0) + count_incr (x2 :: l)
| _ => 0

def run1 : IO Unit := do
let input ← IO.FS.lines "./inputs/1a"
let depths := input.toList.map String.toInt!
let res1 := count_incr depths
let res2: Int := depths |> List.fold_sliding_window 3 List.sum |> count_incr
IO.print s!"{res1} {res2}\n"


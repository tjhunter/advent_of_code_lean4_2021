import Std.Data.HashSet
import MyAdvent.AdventStd

open Std
open Std2

def median [Inhabited α] (lt: α -> α -> Bool) (a: Array α) : Option α := 
a.qsort lt |>.get? ((a.size-1) / 2)

def abs (i: Int) := max i (-i)

def run7a : IO Unit := do
let input0 ← Array.toList <$> IO.FS.lines "./inputs/07"
let input := input0.head!.splitOn "," |>.map String.toInt! |> List.toArray
let center := median (. < .) input |>.get!
let c := input.map (fun i => abs (i - center)) |> Array.toList |> List.sum
IO.print s!"{center}: {c}\n"

def cost (arr: Array Int) (pos: Int) := arr.map (fun i => let z := abs (i - pos); ((z+1) * z)/2) |>.toList |> List.sum

def run7b : IO Unit := do
let input0 ← Array.toList <$> IO.FS.lines "./inputs/07"
let input := input0.head!.splitOn "," |>.map String.toInt! |> List.toArray |>.qsort (. < .)
let l := List.range2 ((input.getMax? (. > .)).get!) (input.getMax? (. < .) |>.get!)
let with_res := List.minBy? Prod.snd <| (l.map (fun idx => (idx, cost input idx)))
IO.print s!"{with_res}: \n "

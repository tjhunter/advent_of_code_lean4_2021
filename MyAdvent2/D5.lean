import MyAdvent.AdventStd
import Std.Data.HashSet

open Std
open Std2

def run5 : IO Unit := do
let mut input0 ← Array.toList <$> IO.FS.lines "./inputs/05"
let input : List (Nat × Nat × Nat × Nat) := input0.map (fun s => match ((s.replace " -> " ",").splitOn ",").map (fun x => x.toNat!) with
  | [x1, y1, x2, y2] => (x1, y1, x2, y2)
  | l => panic! s!"{l}"
  )
let all_coods := List.flatmap (fun (x1, y1, x2, y2) => if x1 == x2 
          then (List.range2 (min y1 y2) (1 + (max y1 y2))).map (fun y => (x1, y))
          else if y1 == y2 then (List.range2 (min x1 x2) (1 + (max x1 x2))).map (fun x => (x.toNat, (y1:Nat)))
          else 
            let xs1 := (List.range2 (min x1 x2) (1 + (max x1 x2))).map (Int.toNat)
            let ys1 := (List.range2 (min y1 y2) (1 + (max y1 y2)))
            List.zip (if x1 < x2 then xs1 else xs1.reverse) (if y1 < y2 then ys1 else ys1.reverse)
          ) input
let overlapping := (HashMap.groupBy id all_coods).toList.filterMap (fun (c, l) => if l.length > 1 then some (c,l) else none)
IO.print s!"{overlapping.length}\n"

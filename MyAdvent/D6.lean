import Std.Data.HashSet
import Std.Data.HashMap
import MyAdvent.AdventStd

open Std
open Std2

def max_counter := 9
def num_iters := 256

def run6 : IO Unit := do
let input0 ← Array.toList <$> IO.FS.lines "./inputs/06a"
let input := (input0.head!.splitOn ",").map (fun s => s.toNat!)
let mut m : HashMap Nat Nat := HashMap.fromList ((List.range max_counter).map (fun i => (i, 0)))
for tim in input do
  m := m.insert tim ((m.getOp tim).get! + 1)
-- Need to do it in reverse order since there is only pop and append currently for arrays
let mut puzzle : Array Nat := m.toArray.qsort (fun (t1, c1) (t2, c2) => t1 > t2) |> Array.map Prod.snd
IO.print s!"{puzzle.toList.enum}\n"
for epoch in List.range num_iters do
  let cnt0 := puzzle.get! (max_counter - 1)
  if cnt0 > 0 then
    puzzle :=  (Array.mk [cnt0]).append puzzle.pop
    puzzle := puzzle.modify (max_counter - 6 - 1) (fun x => x + cnt0)
  else
    puzzle :=  (Array.mk [0]).append puzzle.pop
  IO.print s!"{epoch + 1} {puzzle.toList.reverse}  {List.sum puzzle.toList}\n"

import Std.Data.HashMap
import Std.Data.HashSet
import MyAdvent.AdventStd

open Std
open Std2

inductive Instr where
| left (x: Nat)
| up (y: Nat)
deriving Repr

abbrev State := HashSet (Nat × Nat)

def performFold (s: State) : Instr -> State
| Instr.left x => s.toList.map (fun (x0, y0) => (if x0 <= x then x0 else 2 * x - x0, y0)) |> HashSet.fromList
| Instr.up y => s.toList.map (fun (x0, y0) => (x0, if y0 <= y then y0 else 2 * y - y0)) |> HashSet.fromList

def run13a : IO Unit := do
let input0 ← Array.toList <$> IO.FS.lines "./inputs/13"
let coords0 := input0.flatmap (fun s => match s.splitOn "," with 
  | [x, y] => [(x.toNat!, y.toNat!)]
  | _ => [])
  |> HashSet.fromList
let instructions := input0.filterMap (fun s => match s.splitOn "=" with 
  | ["fold along y", y] => some <| Instr.up (y.toNat!)
  | ["fold along x", x] => some <| Instr.left (x.toNat!)
  | _ => none)
let mut coords := coords0
for i in instructions.take 1 do
  coords := performFold coords i
  IO.print s!"part 1: {coords.size}\n \n"
coords := coords0
for i in instructions do
  coords := performFold coords i
  IO.print s!"part 2: {repr i} {coords.size}\n \n"
let n : Nat := 1 + max (coords.toList.map Prod.fst |>.maximum!) (coords.toList.map Prod.snd |>.maximum!)
IO.print s!"{coords.size}\n {n}\n"
for y in List.range n do
  for x in List.range n do
    IO.print <| if coords.contains (x,y) then "#" else "."
  IO.print "\n"
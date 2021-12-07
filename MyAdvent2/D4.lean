import MyAdvent.AdventStd
import Std.Data.HashSet

open Std
open Std2

def Grid := List (List Nat) deriving Repr


partial def parse_grids : List String -> List Grid
| [] => []
| "" :: l => do
   let mut res := []
   let mut l2 := l
   for i in List.range 5 do
     let x: List Nat := l2.head!.splitOn " " |> List.filterMap (λ s => if s = "" then none else some s.toNat!)
     res := res ++ [x]
     l2 := l2.tail!
   res :: parse_grids l2
| l => panic! s!"{l}"

def run4 : IO Unit := do
let mut input ← Array.toList <$> IO.FS.lines "./inputs/04a"
let draws := input.head!.splitOn "," |> List.map (λ s => s.toNat!)
let grids := parse_grids input.tail!
let bags := grids.map (λ g => List.append g (List.transpose g))
IO.print s!"{repr grids} \n \n {bags}\n"
let mut curr_list : List Nat := []
let mut current_grids := grids.zip bags
for i in draws do
  curr_list := i :: curr_list
  let s := HashSet.fromList curr_list
  let mut next_grids: List (Grid × List (List Nat)) := []
  for (grid, bag) in current_grids do
    if bag.any (λ l => l.all (λ j => s.contains j)) then
      let rem := grid.join.filter (λ j => ! s.contains j)
      if current_grids.length = 1 then
        IO.print s!"question 2 !!! {i} {curr_list} \n {bag} \n {List.sum rem} \n {i * (List.sum rem)} \n "
        return ()
      else
        IO.print s!"question 1 000 {i} {curr_list} \n {List.sum rem} \n {i * (List.sum rem)} \n "
    else
      next_grids := (grid, bag) :: next_grids
      pure ()
  current_grids := next_grids



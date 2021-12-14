import Std.Data.HashMap
import Std.Data.HashSet
import MyAdvent.AdventStd

open Std
open Std2

def expand (rules: HashMap (Char × Char) Char) : List Char -> List Char
| c1 :: c2 :: l => let l2 := match rules.getOp (c1, c2) with 
   | some c => [c]
   | none => []
  [c1] ++ l2 ++ (expand rules (c2 :: l))
| l => l

def run14a : IO Unit := do
let input0 ← Array.toList <$> IO.FS.lines "./inputs/14"
let start := input0.head!.toList
let insertion : HashMap (Char × Char) Char := HashMap.fromList <| input0.tail!.tail!.map (fun s =>
  match s.splitOn " -> " |>.map (String.toList) with 
  | [[c1, c2], [c3]] => ((c1, c2), c3)
  | _ => panic! s!"{s}"
)
IO.print s!"{input0}\n{start}\n{insertion.toList}\n"
let mut state := start
for epoch in List.range 10 do
  state := expand insertion state
  let counts : Array Nat := HashMap.groupBy id state |>.toList.map (fun (c, l) => l.length) |>.toArray
  IO.print s!"{epoch + 1} {counts.maximum?.get! - counts.minimum?.get!} {counts} \n\n" -- {String.fromChars state}


def expand2 (rules: HashMap (Char × Char) Char) (state: HashMap (Char × Char) Nat) : HashMap (Char × Char) Nat := 
state.toList.flatmap (fun ((c1, c2), n) => match rules.getOp (c1, c2) with 
   | some c => [((c1, c), n), ((c, c2), n)]
   | none => [((c1, c2), n)])
   |> HashMap.groupBy (fun ((c1, c2), _) => (c1, c2))
   |>.mapValues (fun l => l.map Prod.snd |>.sum)


def run14b : IO Unit := do
let input0 ← Array.toList <$> IO.FS.lines "./inputs/14"
let start := input0.head!.toList
    |>.fold_sliding_window 2 (fun l => (l.head!, l.tail!.head!))
    |> HashMap.groupBy id
    |>.mapValues List.length
let insertion : HashMap (Char × Char) Char := HashMap.fromList <| input0.tail!.tail!.map (fun s =>
  match s.splitOn " -> " |>.map (String.toList) with 
  | [[c1, c2], [c3]] => ((c1, c2), c3)
  | _ => panic! s!"{s}"
)
let last_char := input0.head!.toList.toArray.last?.get!
IO.print s!"{input0}\n{start.toList}\n{insertion.toList}\n"
let mut state := start
for epoch in List.range 40 do
  state := expand2 insertion state
  let counts_map := state.toList.map (fun ((c1, c2), v) => (c1, v))
      |>.append [(last_char, 1)]
      |> HashMap.groupBy Prod.fst
      |>.mapValues (fun l => l.map Prod.snd |>.sum)
    let counts := counts_map.toArray.map Prod.snd
  IO.print s!"{epoch + 1} {counts.maximum?.get! - counts.minimum?.get!} {counts} \n\n" -- {String.fromChars state}

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


-- questions

def question1: IO Unit := do
let l : List ((Int × Int) × (Int × Int)) := [((1,2), (3,4))]
let z := l.map (fun ((x1, x2), x3, x4) => 1)
let l2 : List ((Int × Int) × Int × Int) := [((1,2), (3,4))]
let z := l2.map (fun ((x1, x2), (x3, x4)) => 1)
let l3 : List (Prod (Prod Int Int) (Prod Int Int)) := [((1,2), (3,4))]
let z := l3.map (fun ((x1, x2), x3, x4) => 1)
pure ()


namespace List
def f1: List α -> Nat
| l => 1

def f2: List α -> Nat -> Nat
| _, x => x

def f3: Nat -> List α -> Nat
| x, _ => x

def g: List α -> List α -> Nat
| _, l => l.length

end List

def question2: IO Unit := do
let l : List Int := [1,2,3]
assert! l.f1 = 1
assert! l.f2 1 = 1
assert! l.f3 1 = 1
assert! [].g [1] = 1


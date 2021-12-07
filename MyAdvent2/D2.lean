import MyAdvent.AdventStd

open Std
open Std2

def parse_line : String -> (Nat × Int)
| s => match s.splitOn " " with
  | ["forward", s2] => (s2.toNat!, 0)
  | ["up", s2] => (0, -s2.toNat!)
  | ["down", s2] => (0, s2.toNat!)
  | _ => panic! s!"{s}"


def perform (l: List (Nat × Int)) : (Nat × Int) := do
let mut aim : Int := 0
let mut pos := 0
let mut depth := 0
for ⟨ m , d ⟩ in l do
  aim := aim + d
  pos := pos + m
  depth := depth + aim * m
(pos, depth)

def run2 : IO Unit := do
let input ← IO.FS.lines "./inputs/2a"
let lines := input.toList.map parse_line
let depth := lines.map Prod.snd |> List.sum
let move := lines.map Prod.fst |> List.sum
let (m2, d2) := perform lines
IO.print s!"{depth} {move} {depth * move}\n"
IO.print s!"{d2} {m2} {m2 * d2}\n"


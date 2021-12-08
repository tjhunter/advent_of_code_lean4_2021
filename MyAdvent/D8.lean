import Std.Data.HashMap
import Std.Data.HashSet
import MyAdvent.AdventStd

open Std
open Std2

def run8a : IO Unit := do
let input0 ← Array.toList <$> IO.FS.lines "./inputs/08"
let res := input0.map (fun s => s.splitOn " | " 
  |>.tail!.head!
  |>.splitOn " "
  |>.map (fun s => s.length)
  |>.filter [(2:Nat),4,3,7].elem
  |>.length
  ) |> List.sum
IO.print s!"{res}\n"


instance : Hashable Char where
  hash (c: Char) := hash (c.toNat)

def alphabet := "abcdefg"

def digits := ["abcefg", "cf", "acdeg","acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

def digitsSet := HashSet.fromList digits

def digitsMap := HashMap.fromList $ digits.enum.map (fun (idx, i) => (i, idx))

def sortedString (s: String) : String := s.toList.toArray.qsort (. < .) |>.toList |> String.mk

def buildTrans (table: String) : HashMap Char Char := HashMap.fromList $ table.toList.zip (alphabet.toList)

def check (lin: List String) (table: String): Bool := 
let dict := buildTrans table
let translations := lin.map (fun s => s.toList.toArray.map (fun c => (dict.getOp c).get!)
   |>.toList 
   |> String.mk
   |> sortedString)
translations.all (fun s => digitsSet.contains s)

def run_problem (encoding: String) (challenge: String) : Nat := 
let encoded := encoding.splitOn " " |>.map sortedString
let res2 := findCombi alphabet.toList (fun l =>
  let test_alph := String.mk l
  if check encoded test_alph then some test_alph else none
) |> Option.get!
let trans := dbg_trace s!"res2: {res2}" ;  buildTrans res2
let is := challenge.splitOn " " |>.map (fun s => s.toList.map (fun c => trans.find! c) 
  |> String.mk |> sortedString
  |> digitsMap.find!)
(String.intercalate "" (is.map (fun i => s!"{i}"))).toNat!


def run8b : IO Unit := do
let input0 ← Array.toList <$> IO.FS.lines "./inputs/08"
let all_nums := input0.map (fun l => match l.splitOn " | " with
  | [encoding, challenge] => run_problem encoding challenge
  | _ => panic! ""
)
IO.print s!"{all_nums}\n  {List.sum all_nums}"

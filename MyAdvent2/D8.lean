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

-- 0-9 in digit representation
def digits := ["abcefg", "cf", "acdeg","acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

-- Digits as a set
def digitsSet := HashSet.fromList digits

-- Map digit str -> value
def digitsMap := HashMap.fromList $ digits.enum.map (fun (idx, i) => (i, idx))

-- Sorts the chars of a string
def sortedString (s: String) : String := s.toList.toArray.qsort (. < .) |>.toList |> String.mk

-- Translation final digit -> candidate
def buildTrans (table: String) : HashMap Char Char := HashMap.fromList $ table.toList.zip (alphabet.toList)

-- Given a list of scrambled digit and a candidate permutation, checks that running
-- the permutation recovers the digits
def check (lin: List String) (table: String): Bool := 
let dict := buildTrans table
let translations := lin.map (fun s => s.toList.toArray.map dict.find!
   |>.toList 
   |> String.mk
   |> sortedString)
translations.all (fun s => digitsSet.contains s)

-- Tries all the permutation of digit values until it finds one that works
def run_problem (encoding: String) (challenge: String) : Nat := 
let encoded := encoding.splitOn " " |>.map sortedString
let res2 := List.findCombi alphabet.toList (fun l =>
  let test_alph := String.mk l
  if check encoded test_alph then some test_alph else none
) |> Option.get!
let trans := dbg_trace s!"res2: {res2}" ;  buildTrans res2
-- The digits
let is : List Nat := challenge.splitOn " " |>.map (fun s => s.toList.map trans.find!
  |> String.mk |> sortedString
  |> digitsMap.find!)
-- Brute-force converse the digits to base10
(String.intercalate "" (is.map (fun i => s!"{i}"))).toNat!


def run8b : IO Unit := do
let input0 ← Array.toList <$> IO.FS.lines "./inputs/08"
let all_nums := input0.map (fun l => match l.splitOn " | " with
  | [encoding, challenge] => run_problem encoding challenge
  | _ => panic! ""
)
IO.print s!"{all_nums}\n  {List.sum all_nums}"

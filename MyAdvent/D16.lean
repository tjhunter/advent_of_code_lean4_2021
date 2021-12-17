import Std.Data.HashMap
import Std.Data.HashSet
import MyAdvent.AdventStd

open Std
open Std2



def to_binary_auxi (c: Char) : List Bool :=
let hex := ['0', '1', '2', '3', '4', '5', '6', '7',
 '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'].zip ((List.range 16).map (fun i => 
    [(i/8)%2 == 1, (i/4)%2 == 1, (i/2)%2 == 1, i%2 == 1]))
hex.lookup c |>.get!


inductive Message where
| literal (ver: Nat) (val: Nat)
| operator (ver: Nat) (opid: Nat) (children: List Message)
deriving Repr

instance : Inhabited Message where 
  default := Message.literal 0 0 


def read_bools (n: Nat) : (StateM (List Bool) (Option (List Bool))) := do
let l0 <- get
if l0.length >= n then
  modify (fun s => s.drop n)
  pure (some (l0.take n))
else pure none

def read_version : (StateM (List Bool) (Option Nat)) := do
let v0 <- read_bools 3
match v0 with
| some l => pure <| bin_to_nat l
| _ => pure none

partial def read_number_auxi : (StateM (List Bool) (Option (List Bool))) := do
let l <- read_bools 5
match l with 
  | some (true :: l2) => do
      let l3o <- read_number_auxi
      pure <| ((l2 ++ · ) <$> l3o) -- Not the most readable
  | some (false :: l2) => pure (some l2)
  | _ => pure none

partial def readGreedy : (StateM (List Bool) (Option Message ×  List Bool)) -> List Bool -> List Message
| sm, [] => []
| sm,l => match (sm.run' l) with
   | (none, _) => []
   | (some m, l2) => m :: readGreedy sm l2


partial def parse : (StateM (List Bool) (Option Message ×  List Bool)) := do
let v0 <- read_version
let t0 <- read_bools 3
match (v0, t0) with
| (some v, some [true,false,false]) => do
    let no <- read_number_auxi 
    let curr <- get
    pure (Message.literal v (bin_to_nat no.get!), curr)
| (some v, some l2) => do
    let opid := bin_to_nat l2
    let i <- (Option.get!) <$> read_bools 1
    if i == [true] then do
      let l <- (Option.get!) <$> read_bools 11
      let num_sub_packets := bin_to_nat l
      let mut res: List Message := []
      for _ in List.range num_sub_packets do
        let (p, _) <- parse
        res := res ++ [p.get!]
      let curr <- get
      pure (Message.operator v opid res, curr)
    else do
      let l <- (Option.get!) <$> read_bools 15
      let num_bits := bin_to_nat l
      let buffer <- read_bools num_bits
      let curr <- get
      pure (Message.operator v opid (readGreedy parse buffer.get!), curr)
| _ => do
  let curr <- get
  pure (none, curr)

partial def sum_versions : Message -> Nat
| Message.literal ver _ => ver
| Message.operator ver _ l => ver + List.sum (l.map sum_versions)


partial def evaluate : Message -> Nat
| Message.literal ver x => x
| Message.operator ver 0 l => List.sum (l.map evaluate)
| Message.operator ver 1 l => (l.map evaluate).foldl (· * · ) 1
| Message.operator ver 2 l => (l.map evaluate).foldl (min) 100000000000
| Message.operator ver 3 l => (l.map evaluate).foldl (max) 0
| Message.operator ver 5 [p1, p2] => if evaluate p1 > evaluate p2 then 1 else 0
| Message.operator ver 6 [p1, p2] => if evaluate p1 < evaluate p2 then 1 else 0
| Message.operator ver 7 [p1, p2] => if evaluate p1 == evaluate p2 then 1 else 0
| m => panic! s!"{repr m}"

def run16a : IO Unit := do
let input0 ← IO.FS.lines "./inputs/16"
let input := input0.first? |>.get! |>.toList |>.flatmap to_binary_auxi
-- IO.print s!"{input}\n"
let res : Option Message := (parse.run' input).fst
IO.print s!"{repr res}\n {sum_versions res.get!} \n "

IO.print s!"{evaluate res.get!} \n "

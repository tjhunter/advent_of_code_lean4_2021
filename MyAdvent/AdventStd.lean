import Lean.Data.Json
import Std.Data.HashSet
import Std.Data.HashMap
import Lean.Util.SCC

-- it is not partial, but unsure how to make it not partial
-- TODO: how to make it not partial?
private partial def fold_slid_aux_opt (n: Nat) (l: List α) : Option (List α) := 
match (n, l) with
  | (0, _) => Option.some []
  | (_, []) => Option.none
  | (i+1, x::l2) => (x :: .) <$> fold_slid_aux_opt i l2

-- ****** List functions ******
namespace List

private def minByAux [LE β] [DecidableRel (@LE.le β  _)] : (List (α × β)) -> Option (α × β)
| [] => none
| (a,b)::as => match minByAux as with
  | none => some (a,b)
  | some (a1, b1) => some <| if LE.le b1 b then (a1, b1) else (a,b)

def minBy? [LE β] [DecidableRel (@LE.le β  _)] (f: α -> β) (l: List α ) : Option α := 
  Prod.fst <$> (l.map (fun a => (a, f a)) |> minByAux )


-- The flatten operation in scala
-- I could not find the corresponding operation in Lean
def flatten : (List (List α )) -> List α 
| [] => []
| l :: t => List.append l (flatten t)

def flatmap (f: α -> List β) (l: List α) : List β := List.flatten (l.map f)

-- TODO: fix the partial
partial def transpose: (List (List α )) -> List (List α )
| [] => []
| [[]] => []
| [] :: others => transpose others
| (h :: l1) :: others =>
  let heads := others.filterMap List.head?
  let tails := others.filterMap List.tail?
  (h :: heads) :: transpose (l1 :: tails)

def sum [m : Add α] [m2: OfNat α 0] (l: List α ): α := l.foldl (m.add) (m2.ofNat)

-- Generalized range.
def range2 (fr: Int) (to: Int) : List Int := (List.range (to - fr).toNat).map f where
  f : Nat -> Int
  | i => ((i: Int) + fr)

-- Only runs complete sliding windows, no incomplete
def fold_sliding_window (n: Nat) (f: List α -> β) (l: List α) : List β :=
  match l, fold_slid_aux_opt n l with 
    | (x :: l2), (Option.some l3) => (((f l3) :: fold_sliding_window n f l2) : List β )
    | [], Option.some l3 => ([f l3] : List β )
    | _, _ => ([] : List β )

partial def List.sort (lt: α -> α -> Bool) (l: List α ): List α := match l with
| [] => []
| p :: l2 => 
    let (lower, upper) := List.partition (lt . p) l2
    (sort lt lower) ++ [p] ++ (sort lt upper)


private def all_list_splits : List α -> List ((List α × List α))
| [] => []
| (x :: l) => all_list_splits l |>.map (fun (l1, l2) => (x :: l1, l2)) |>.cons ([], x :: l)

private def findCombi_auxi (head: List α) (l: List α ) (test: List α -> Option β) : Option β := match l with
| [] => test head
| (x :: l2) => Id.run do
  let mut ret := none
  for (bef, aft) in (all_list_splits head) ++ [(head, [])] do
    let h2 := bef ++ [x] ++ aft
    match findCombi_auxi h2 l2 test with
    | some res =>
        ret := (some res)
        break
    | none => continue
  ret

def findCombi (l: List α ) (test: List α -> Option β) : Option β := findCombi_auxi [] l test

end List

-- ****** Array functions *****

namespace Array

def slice (a : Array α ) (start: Int) (until: Int) : Array α := 
  let start_idx: Nat := if start < 0 then (a.size + start).toNat else start.toNat
  let end_idx:Nat := if until < 0 then (a.size + until).toNat else until.toNat
  (List.toArray (a.data.drop start_idx)).shrink (a.size - end_idx - 1)

def first? (a: Array α ): Option α := a.data.head?

def last? (a: Array α ): Option α := if a.isEmpty then none else a.get? (a.size -1)

def minimum? [LE α] [DecidableRel (@LE.le α  _)] (arr: Array α) : Option α := arr.getMax? (fun x1 x2 => LE.le x2 x1)

def maximum? [LE α] [DecidableRel (@LE.le α  _)] (arr: Array α) : Option α := arr.getMax? (fun x1 x2 => LE.le x1 x2)

def maximum! [Inhabited α] [LE α] [DecidableRel (@LE.le α  _)] (arr: Array α) : α := maximum? arr |>.get!

def mininum! [Inhabited α] [LE α] [DecidableRel (@LE.le α  _)] (arr: Array α) : α := minimum? arr |>.get!

end Array


-- ****** HashSet functions *****

namespace Std
namespace HashSet

def fromList [BEq a] [Hashable a] (l: List a) : (Std.HashSet a) :=
  l.foldl Std.HashSet.insert Std.HashSet.empty


end HashSet
end Std


-- ****** HashMap functions *****

namespace Std
namespace HashMap

def fromList [BEq a] [Hashable a] (l: List (a × b)) : (Std.HashMap a b) := Id.run do
  let mut m: Std.HashMap a b := Std.HashMap.empty
  for ⟨ a , b ⟩ in l do
    m := m.insert a b
  m

def groupList [BEq a] [Hashable a] (l: List (a × b)) : (Std.HashMap a (List b)) := Id.run do
  let mut m: Std.HashMap a (List b) := Std.HashMap.empty
  for ⟨ a , b ⟩ in l do
    match m.getOp a with
    | Option.none => m := m.insert a [b]
    | Option.some l2 => m := m.insert a (b :: l2)
    -- Put the values back in order after insertion.
  let z : List (a × (List b)) := m.toList.map (fun ⟨a, l⟩ => ⟨a, l.reverse⟩ )
  fromList z

def groupBy [BEq b] [Hashable b] (f: a -> b) (l: List a) : Std.HashMap b (List a) := 
  groupList (l.map (fun a0 => (f a0, a0)))

def mapValues [BEq α ] [Hashable α ]  (m: Std.HashMap α β ) (f: β -> γ) : Std.HashMap α γ := 
m.toList.map (fun (k,v) => (k, f v)) |> fromList

end HashMap
end Std


-- ****** Extra functions ******

namespace Std2 

instance : Hashable Char where
  hash c := hash (s!"{c}")

-- Given an integer in boolean representation, converts it to a natural.
def bin_to_nat (l: List Bool) : Nat := 
let n := l.length
List.sum $ (List.range n).zipWith (λ i b => if b then 2 ^ (n-i-1) else 0) l

def Char.toString (c: Char) : String := s!"{c}"

def String.fromChars (cs: List Char) : String := String.mk cs

end Std2


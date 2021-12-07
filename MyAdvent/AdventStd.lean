import Lean.Data.Json
import Std.Data.HashSet
import Std.Data.HashMap
import Lean.Util.SCC
open List

-- it is not partial, but unsure how to make it not partial
-- TODO: how to make it not partial?
private partial def fold_slid_aux_opt (n: Nat) (l: List α) : Option (List α) := 
match (n, l) with
  | (0, _) => Option.some []
  | (_, []) => Option.none
  | (i+1, x::l2) => (x :: .) <$> fold_slid_aux_opt i l2


namespace Std2 

namespace List

def minByAux [LE β] [DecidableRel (@LE.le β  _)] : (List (α × β)) -> Option (α × β)
| [] => none
| (a,b)::as => match minByAux as with
  | none => some (a,b)
  | some (a1, b1) => some <| if LE.le b1 b then (a1, b1) else (a,b)

def minBy? [LE β] [DecidableRel (@LE.le β  _)] (f: α -> β) (l: List α ) : Option α := 
  Prod.fst <$> (l.map (fun a => (a, f a)) |> minByAux )

end List


-- TODO: fix the partial
partial def List.transpose: (List (List α )) -> List (List α ) 
| [] => []
| [[]] => []
| [] :: others => transpose others
| (h :: l1) :: others =>
  let heads := others.filterMap List.head?
  let tails := others.filterMap List.tail?
  (h :: heads) :: transpose (l1 :: tails)

def List.sum [m : Add α] [m2: OfNat α 0] (l: List α ): α := l.foldl (m.add) (m2.ofNat)

-- The flatten operation in scala
-- I could not find the corresponding operation in Lean
def List.flatten : (List (List α )) -> List α 
| [] => []
| l :: t => List.append l (flatten t)

def List.flatmap (f: α -> List β) (l: List α) : List β := List.flatten (l.map f)

def List.range2 (fr: Int) (to: Int) : List Int := (List.range (to - fr).toNat).map f where
  f : Nat -> Int
  | i => ((i: Int) + fr)

-- Given an integer in boolean representation, converts it to a natural.
def bin_to_nat (l: List Bool) : Nat := 
let n := l.length
List.sum $ (List.range n).zipWith (λ i b => if b then 2 ^ (n-i-1) else 0) l


-- Only runs complete sliding windows, no incomplete
def List.fold_sliding_window (n: Nat) (f: List α -> β) (l: List α) : List β :=
  match l, fold_slid_aux_opt n l with 
    | (x :: l2), (Option.some l3) => (((f l3) :: fold_sliding_window n f l2) : List β )
    | [], Option.some l3 => ([f l3] : List β )
    | _, _ => ([] : List β )

def HashSet.fromList [BEq a] [Hashable a] (l: List a) : (Std.HashSet a) :=
  l.foldl Std.HashSet.insert Std.HashSet.empty

def HashMap.fromList [BEq a] [Hashable a] (l: List (a × b)) : (Std.HashMap a b) := do
  let mut m: Std.HashMap a b := Std.HashMap.empty
  for ⟨ a , b ⟩ in l do
    m := m.insert a b
  m

def HashMap.groupList [BEq a] [Hashable a] (l: List (a × b)) : (Std.HashMap a (List b)) := do
  let mut m: Std.HashMap a (List b) := Std.HashMap.empty
  for ⟨ a , b ⟩ in l do
    match m.getOp a with
    | Option.none => m := m.insert a [b]
    | Option.some l2 => m := m.insert a (b :: l2)
    -- Put the values back in order after insertion.
  let z : List (a × (List b)) := m.toList.map (fun ⟨a, l⟩ => ⟨a, l.reverse⟩ )
  HashMap.fromList z

def HashMap.groupBy [BEq a] [Hashable a] (f: a -> b) (l: List a) : Std.HashMap a (List b) := 
  HashMap.groupList (l.map (fun a0 => (a0, f a0)))

partial def List.sort (lt: α -> α -> Bool) (l: List α ): List α := match l with
| [] => []
| p :: l2 => 
    let (lower, upper) := List.partition (lt . p) l2
    (sort lt lower) ++ [p] ++ (sort lt upper)

end Std2


namespace Std
def Array.slice (a : Array α ) (start: Nat) (until: Nat) : Array α := panic! ""
end Std
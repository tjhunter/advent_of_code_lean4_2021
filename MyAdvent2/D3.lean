import MyAdvent.AdventStd

open Std
open Std2



def maxes (l: Array (Array Nat)) : Array Bool := Id.run do
let n := l.get! 0 |>.size
let mut res : Array Nat := Array.mkArray n (0 : Nat)
for a in l do
  for i in List.range n do
    res := res.modify i (. + (a.get! i))
res.map (λ i => i >= (l.size / 2))

partial def maxes_rec (l: List (List Nat)) : (List Bool × List Bool) := Id.run do
if l.isEmpty || l.head!.isEmpty then ([],[]) else
let count_heads := List.sum $ l.map (List.head!)
dbg_trace s!"{count_heads}"
let most_common := if count_heads >= l.head!.length / 2 then true else false
let l1 := maxes_rec $ l.filter (λ a => a.head! = 1) |> List.map List.tail!
let l0 := maxes_rec $ l.filter (λ a => a.head! = 0) |> List.map List.tail!
let ox := if most_common = true then true :: Prod.fst l1 else false :: Prod.fst l0
let co := if most_common = true then false :: Prod.snd l0 else false :: Prod.snd l1
(ox, co)




-- partial def ox_rec (l: List (List Nat)) : List Bool := do
-- if l.isEmpty || l.head!.isEmpty then [] else
-- let count1 := List.sum $ l.map (List.head!)
-- let count0 := l.length - count1
-- let l1 := ox_rec $ l.filter (λ a => a.head! = if count1 >= count0 then 1 else 0) |> List.map List.tail!
-- (count1 >= count0) :: l1

-- partial def co_rec (l: List (List Nat)) : List Bool := do
-- if l.isEmpty || l.head!.isEmpty then [] else
-- let count1 := List.sum $ l.map (List.head!)
-- let count0 := l.length - count1
-- let l1 := co_rec $ l.filter (λ a => a.head! = if count0 >= count1 then 0 else 1) |> List.map List.tail!
-- (count1 <= count0) :: l1


-- partial def ox_rec2 (l: List (List Nat × List Nat)) : List Bool := match l with
-- | [] => []
-- | [(_, l)] => l.map (. = 1)
-- | l => do
--     let n := l.length
--     let count1 := List.sum $ l.map (λ (r, a) => r.head!)
--     let count0 := l.length - count1    
--     dbg_trace s!"ox_rec2: count1 {count1} count0 {count0}"
--     let l2 := l.filterMap (λ (r, a) => if r.head! = (if count1 >= count0 then 1 else 0) then some (r.tail!, a) else none)
--     ox_rec2 l2

-- partial def co_rec2 (l: List (List Nat × List Nat)) : List Bool := dbg_trace s!"{l}"; match l with
-- | [] => []
-- | [(_, l)] => l.map (. = 1)
-- | l => do
--     let n := l.length
--     let count1 := List.sum $ l.map (λ (r, a) => r.head!)
--     let count0 := l.length - count1    
--     dbg_trace s!"co_rec2: count1 {count1} count0 {count0}"
--     let l2 := l.filterMap (λ (r, a) => if r.head! = (if count1 >= count0 then 0 else 1) then some (r.tail!, a) else none)
--     co_rec2 l2


partial def rec2 (choice: Nat -> Nat -> Nat) (l: List (List Nat × List Nat)) : List Bool := match l with
| [] => panic! "invalid input"
| [(_, l)] => l.map (. = 1)
| l => Id.run  do
    let n := l.length
    let count1 := List.sum $ l.map (λ (r, a) => r.head!)
    let count0 := l.length - count1    
    let l2 := l.filterMap (λ (r, a) => if r.head! = (choice count0 count1) then some (r.tail!, a) else none)
    rec2 choice l2


def run3 : IO Unit := do
let input ← IO.FS.lines "./inputs/3a"
let measures := input.toList.map (λ l => l.toList.map (λ s => s.toString.toNat!)) |>.map (λ l => (l, l))
let ox := bin_to_nat <| rec2 (λ count0 count1 => (if count1 >= count0 then 1 else 0)) measures
let co := bin_to_nat <| rec2 (λ count0 count1 => (if count1 >= count0 then 0 else 1)) measures
IO.print s!"{ox} {co} {ox * co}"


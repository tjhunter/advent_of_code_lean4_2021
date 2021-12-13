import Std.Data.HashMap
import Std.Data.HashSet
import MyAdvent.AdventStd
import MyAdvent.Graph

open Std
open Std2

def run9a : IO Unit := do
let input0 ← IO.FS.lines "./inputs/09"
let input : Array (Array Nat) := input0.map (fun s => 
  s.toList.map (fun c => 
      Char.toString c |> String.toNat!)
  |> List.toArray)
let rows := input.size
let cols := (input.get! 0).size
let mut res := 0
for r in List.range rows do
  for c in List.range cols do
    let cur_r := input.get! r
    let cur := cur_r.get! c
    let neighs := [
      if c > 0 then cur_r.get! (c-1) else 10,
      if c < cols-1 then cur_r.get! (c+1) else 10,
      if r > 0 then input.get! (r-1) |>.get! c else 10,
      if r < rows -1 then input.get! (r+1) |>.get! c else 10]
    let neigh := neighs.minimum? |>.getD cur
    IO.print s!"{r} {c} {cur} {neighs} {neigh} {res} \n"
    if neigh > cur then
      res := 1 + cur + res
    else pure ()
IO.print s!"{res}\n"

def run9b : IO Unit := do
let input0 ← IO.FS.lines "./inputs/09test"
let input : Array (Array Nat) := input0.map (fun s => 
  s.toList.map (fun c => 
      Char.toString c |> String.toNat!)
  |> List.toArray)
let rows := input.size
let cols := (input.get! 0).size
let mut edges : List ((Nat × Nat)× (Nat × Nat)) := []
for r in List.range rows do
  for c in List.range cols do
    let cur_r := input.get! r
    let cur := cur_r.get! c
    if cur < 9 then
      let neighs : List (Nat × Nat × Nat ) := [
        (r, c-1, if c > 0 then some <| cur_r.get! (c-1) else none),
        (r, c+1, if c < cols-1 then some <| cur_r.get! (c+1) else none),
        (r-1, c, if r > 0 then some <| input.get! (r-1) |>.get! c else none),
        (r+1, c, if r < rows -1 then some <| input.get! (r+1) |>.get! c else none)].filterMap (match . with
          | (r0, c0, some h0) => if h0 < cur then some (r0, c0, h0) else none
          | _ => none
        )
      match List.minBy? (fun (_,_,h) => h) neighs with
      | some (r0, c0, _) => edges := ((r,c), (r0,c0)) :: edges
      | none => ()
let g := SimpleGraph.fromArray! (List.range rows |>.map (fun r => List.range cols |>.map (., r)) |> List.flatten |> List.toArray) edges.toArray
let cc := g.connectedComponents.map (fun a => a.size) --.filter (fun a => a.size > 1)
IO.print s!"{edges} \n{cc}\n"

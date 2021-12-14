import Lean.Data.Json
import Std.Data.HashSet
import Std.Data.HashMap

open Std

universe u v w

-- Simple undirected graph with self loops
-- No vertex attribute or edge attribute.
-- Super simple implementation, nothing fancy.
-- The graph is fully materialized in memory (no partial graph creation)
structure SimpleGraph (NodeId: Type u) [BEq NodeId] [Hashable NodeId] where
  vx_internal: HashSet NodeId
  e_internal: Array (NodeId × NodeId)


namespace SimpleGraph

variable {α : Type u} {β : Type v} {_ : BEq α} {_ : Hashable α} {_ : ToString α }

def empty {α : Type u} [BEq α] [Hashable α] : SimpleGraph α := { vx_internal := HashSet.empty, e_internal := Array.empty } 

instance [BEq α] [Hashable α] : Inhabited (SimpleGraph α ) where
  default := empty

def fromArray! (vertices: Array (α)) (edges: Array (α × α)) [BEq α] [Hashable α] : SimpleGraph α := 
{vx_internal := vertices.foldl (fun hs nid => hs.insert nid) HashSet.empty,
 e_internal := edges}

-- This is the in- and out- edges
-- None if nid is not a vertex
def edges? (graph: SimpleGraph α ) (nid: α ): Option (Array α) := 
if graph.vx_internal.contains nid then
  graph.e_internal.filterMap (fun (n1, n2) => if n1 == nid then some n2 else if n2 == nid then n1 else none)
  |>.foldl (fun hs nid0 => hs.insert nid0) HashSet.empty
  |>.toArray
else none


private partial def connectedComponents_internal [ToString α] (nid: α ) (expand: α → List α) (curr: HashSet α) : HashSet α := Id.run do
if curr.contains nid then curr else
let mut set := dbg_trace s!"connectedComponents_internal: {nid} {expand nid} {curr.toList}" ;  curr.insert nid
for nid2 in expand nid do
  if ! set.contains nid2 then
    set := connectedComponents_internal nid2 expand set
set


def connectedComponents [ToString α] (graph: SimpleGraph α ) : Array (Array α) := Id.run do
  let mut clusters : HashMap α α := HashMap.empty
  let mut res : List (Array α) := []
  for nid in graph.vx_internal.toList do
    if ! clusters.contains nid then
      clusters := clusters.insert nid nid
      let reachable := dbg_trace s!"connectedComponents: {nid} " ; connectedComponents_internal nid (fun nid0 => graph.edges? nid0 |>.get! |> Array.toList) (HashSet.empty) |>.insert nid
      for nid0 in reachable.toList do
        clusters := clusters.insert nid0 nid
      res := (reachable.toArray) :: res
  res.toArray
end SimpleGraph


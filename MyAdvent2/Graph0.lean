import Lean.Data.Json
import Std.Data.HashSet
import Std.Data.HashMap

open Std

universe u v w


structure GraphImpl (NodeId: Type u) (Vertex: Type v) (Edge: Type w) [BEq NodeId] [Hashable NodeId] where
  vx_internal: HashMap NodeId Vertex
  e_internal: Array (NodeId × NodeId × Edge)

def mkGraphImpl {NodeId: Type u} {Vertex: Type v} {Edge: Type w} [BEq NodeId] [Hashable NodeId] : GraphImpl NodeId Vertex Edge := 
{
  vx_internal := HashMap.empty,
  e_internal := Array.empty
}



-- Undirected graph with self loops
structure Graph (NodeId: Type u) (Vertex: Type v) (Edge: Type w) [BEq NodeId] [Hashable NodeId] where
  impl : GraphImpl NodeId Vertex Edge

-- *** GraphImpl namespace ***

namespace GraphImpl

variable {NodeId : Type u} {Vertex : Type v} {Edge: Type w} {_ : BEq NodeId} {_ : Hashable NodeId}

def build! (vertices: Array (NodeId × Vertex)) (edges: Array (NodeId × NodeId × Edge)) : GraphImpl NodeId Vertex Edge :=
let vx_internal: HashMap NodeId Vertex := vertices.foldl (fun hm (nid, v) => hm.insert nid v) HashMap.empty
{  vx_internal := vx_internal, e_internal := edges }

end GraphImpl

-- *** Graph namespace ***

namespace Graph

instance [BEq α] [Hashable α] : Inhabited (Graph α β γ ) where
  default := ⟨ mkGraphImpl ⟩ 

def empty {α : Type u} {β : Type v} {γ : Type w} [BEq α] [Hashable α] : Graph α β γ := ⟨  mkGraphImpl ⟩ 

def fromArray! (vertices: Array (α × β)) (edges: Array (α × α × γ)) [BEq α] [Hashable α] : Graph α β γ := ⟨ GraphImpl.build! vertices edges ⟩ 

end Graph
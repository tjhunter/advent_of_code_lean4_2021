import Std.Data.HashMap
import Std.Data.HashSet
import MyAdvent.AdventStd

open Std
open Std2



def run10a : IO Unit := do
let input0 ← IO.FS.lines "./inputs/10test"
let l := [1,2].flatmap2 (fun x => [x])

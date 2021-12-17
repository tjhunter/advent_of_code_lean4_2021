-- Python code for the first question. This was solved by solving the 2nd order equation of the parabolic trajectory.
-- [w0 for (w0, test) in ([(w0, math.ceil(math.sqrt((w0+0.5)**2 - 2 * ymax) + 0.5 + w0) <= math.floor(math.sqrt((w0+0.5)**2 - 2 * ymin) + 0.5 + w0)) for w0 in range(200)]) if test]
-- >>> w0 = 123
-- >>> (w0 + 1/2) ** 2
-- 15252.25
-- >>> (w0 + 1/2) ** 2 / 2
-- 7626.125
-- >>> nmax = 123.5
-- >>> nmax = 123
-- >>> (w0+1/2)*nmax - nmax ** 2 /2
-- 7626.0
-- >>> nmax = 124
-- >>> (w0+1/2)*nmax - nmax ** 2 /2

import Std.Data.HashMap
import Std.Data.HashSet
import MyAdvent.AdventStd

open Std
open Std2

def xmin : Int := 211
def xmax : Int := 232
def ymin : Int := -124
def ymax : Int := -69

def run (v0: Int) (w0: Int) : Option Nat := Id.run do
let mut x : Int := 0
let mut y : Int := 0
let mut v := v0
let mut w := w0
for n in List.range 300 do
  if x >= xmin && x <= xmax && y >= ymin && y <= ymax then return (some n)
  if x > xmax || y < ymin then return none
  x := x + v
  y := y + w
  v := max (v-1) 0
  w := w - 1
return none


def run17b : IO Unit := do
let mut res : List (Int × Int) := []
for v0 in List.range2 (-1) (xmax + 2) do
  for w0 in List.range2 (ymin-1) 150 do
    match run v0 w0 with 
    | some n => do 
        IO.print s!"{v0} {w0} {n} \n"
        res := (v0, w0) :: res
    | none => pure ()
IO.print s!"{res.length}"

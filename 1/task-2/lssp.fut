-- Parallel Longest Satisfying Segment
-- Longest Satisfying Segment       --
-- ASSIGNMENT 1: fill in the blanks --
--       See lecture notes          --
--
-- ==
-- compiled input { 
--    [1, -2, -2, 0, 0, 0, 0, 0, 3, 4, -6, 1]
-- }  
-- output { 
--    9
-- } 

import "/futlib/math"
type int = i32
let max (x:int, y:int) = i32.max x y

let pred1 (k : int, x: int) : bool =
  if      k == 1 then x == 0 -- zeroes
  else if k == 2 then true   -- sorted  
  else if k == 3 then true   -- same
  else true                  -- default

let pred2(k : int, x: int, y: int): bool =
  if      k == 1 then (x == 0) && (y == 0) -- zeroes
  else if k == 2 then x <= y               -- sorted  
  else if k == 3 then x == y               -- same
  else true                                -- default

-- the task is to implement this operator by filling in the blanks
let redOp (pind : int) 
          (x: (int,int,int,int,int,int)) 
          (y: (int,int,int,int,int,int)) 
        : (int,int,int,int,int,int) =
  let (lssx, lisx, lcsx, tlx, firstx, lastx) = x
  let (lssy, lisy, lcsy, tly, firsty, lasty) = y
-- Check whether the two segments follows the predicate. connect is used
-- in case any new segments crosses x and y
  let connect = pred2(pind, lastx, firsty)
-- Calculate new lss 
  let newlss  = if connect &&  max(lssx, lssy) <  lcsx + lisy
                then lcsx + lisy
                else max(lssx, lssy)
-- Calculate new initial segment 
  let newlis  = if connect && lisx == lssx
                then lisx + lisy
                else lisx
-- Calculate new concluding segment 
  let newlcs  = if connect && lcsy ==  lssy
                then lcsy + lcsx
                else lcsy
  let first   = if tlx == 0 then firsty else firstx
  let last    = if tly == 0 then lastx else lasty in

  (newlss, newlis, newlcs, tlx+tly, first, last)

let mapOp (pind : int) (x: int) : (int,int,int,int,int,int) =
  let xmatch = if pred1(pind, x) then 1 else 0 in
  (xmatch, xmatch, xmatch, 1, x, x)

let lssp (pind : int) (xs : []int) : int =
  let (x,_,_,_,_,_) =
    reduce (redOp pind) (0,0,0,0,0,0) (map (mapOp pind) xs) in
  x

let main(xs: []int): int =
  lssp 2 xs -- computes sorted 
  -- you may also try with zeroes, i.e., lssp 1 xs, and same, i.e., lssp 3 xs



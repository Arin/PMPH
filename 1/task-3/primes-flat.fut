-- For Assignment 1, Task 3, Please implement the Flat-Parallel Version 
-- of prime numbers computation (sieve).
-- ==
-- compiled input { 30 } output { [2,3,5,7,11,13,17,19,23,29] }

import "/futlib/array"
import "sgm-scan.fut"


let segmScanEx (neutral : i32) (arr : [n]i32) (flags : [n]i32) : []i32 =
  let copy = arr
  let 

-- The current implementation is dummy, i.e., produces garbage.
-- Your implementation should recognize all prime numbers less than n.
let primesFlat (n : i32) : []i32 =
  if n <= 2 then [2]
  else let sq= i32( f64.sqrt (f64 n) )
       let sq_primes = primesFlat sq
       ----------------------------------------
       -- ASSIGNMENT 1, Task 3: insert here your code
       -- that implements the flat-parallel version
       -- of prime-number computation (sieve). The
       -- code should compute the not_primes array  
       -- and its length mm.
       -- The two lines below are dummy, replace them as well!
       -- ...
       -- not_primes is nested, flattened - so here's where the flattening needs 
       -- to happen!
       --  nested = map (\p -> let m = (n / p)
       --                      in  map (\j -> j*p) (filter (\n -> n > 1) (iota m))
       --               ) sq_primes              ^^ drop first 2 ints
       -- not_primes start, taken from slide 41 of L2-Flattening
       let not_primes = let m     = map (p -> n / p) sq_primes -- n / p  -- distribute map
                        let mm1   = map (mm -> mm - 1) m       -- m - 1  -- distribute map
                        let inds1 = reduce (+) 0 sq_primes     -- let iot  = iota mm1  -- rule 5
                        let inds  = concat [0] (inds1[0:n-1]) 
                        let size  = (last inds) + (last arr)
                        let flag  = write inds arr (replicate size 0)
                        let tmp   = replicate size 1
                        let iot1  = sgm-scan.main  
                        let twom  = map (+2) iot -- rule 2
                        let rp    = replicate mm1 p -- rule 4
                        in  map (\(j,p) -> j*p) (zip twom rp) -- rule 2
       let mm = (shape not_primes)[0]
       -- ...
       -- ... 
       -- This is how the implementation should end,
       -- i.e., the code remanining after flattening
       -- the nested maps.
       let zero_array = replicate mm 0
       let mostly_ones= map (\ x -> if x > 1 then 1 else 0) (iota (n+1))
       let prime_flags= scatter mostly_ones not_primes zero_array
       in  filter (\i -> unsafe prime_flags[i] != 0) (iota (n+1))

let main (n : i32) : []i32 = primesFlat n


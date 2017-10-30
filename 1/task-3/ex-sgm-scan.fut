import "/futlib/math"
import "/futlib/array"

let exScan [n] (flags : []i32) (vals : [n]i32) : []i32 = 
  let copy  = vals
  let k     = i32(f32.log (f32(n))) 
  let ds    = iota k
  let i     = 0
  let copy1 = map (\d -> let next = i + 2 ** (d+1) - 1
                         let cur  = i + 2 ** (d) - 1
                         in if   flags[next] == 0
                            then copy[cur] + copy[next]
                                         else copy[next]
                        let flags[next] = flags[cur] | flags[next]
                        in  d
                 ) ds
  in copy1

let main (flags : []i32) (vals : []i32) : []i32 = 
  exScan flags vals

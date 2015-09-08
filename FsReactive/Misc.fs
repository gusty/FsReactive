namespace FsReactive

open System

module Misc =

    let catOption l =
        let rec proc acc = function
            | []   -> acc
            | h::t -> match h with
                        |Some x -> proc (x::acc) t
                        |None   -> proc acc      t
        List.rev (proc [] l)
   
    let isSome  = function Some _ -> true | None -> false
    let getSome = function Some x -> x    | None -> failwith "error: not some"
             
    let curry   f  a  b  = f (a, b)
    let uncurry f (a, b) = f  a  b
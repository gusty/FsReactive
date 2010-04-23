#light


namespace FReactive

open System

 module Misc =


  let catOption l =
    let rec proc l acc =
        match l with
        |[] -> acc
        |h::t -> match h with
                 |Some x -> proc t (x::acc)
                 |None -> proc t acc
    List.rev (proc l [])
   
  let isSome x =  match x with 
                  |None -> false
                  |Some _ -> true

  let getSome x =
    match x with
    |Some x -> x
    |None -> failwith "error: not some"
  
  let memoize f =
    let cache = ref None
    let compute x = let res = f x;
                    in
                    cache := Some (x, res);
                    res
    in
    fun x -> match !cache with
             |Some (x1, y1) when x=x1 -> y1
             |Some (x1, y1)  -> compute x
             |None -> compute x
             
             
  let curry f a b = f (a,b)
  let uncurry f (a, b) = f a b
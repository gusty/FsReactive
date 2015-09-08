namespace FsReactive

open FsReactive
open Misc

module DynCol = 
 
    // 'a option Behavior list -> ['a option Behavior] Event -> 'a list Behavior

    let rec dyncolB lb (Evt evt) = 
        let bf t = 
            let r, nb = List.unzip (List.filter (fst >> isSome) (List.map (fun (Beh b) -> b t) lb))
            let proc () = 
                let l = List.map (fun x -> x()) nb
                let re, ne = evt t
                let l' = match re with
                            | None   -> l
                            | Some b -> b@l
                dyncolB l' (ne())
            catOption r, proc
        Beh bf
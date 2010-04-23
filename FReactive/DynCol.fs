#light


namespace FReactive

 open FReactive
 open Misc
 
 module DynCol = 
 
 // 'a option Behavior list -> ['a option Behavior] Event -> 'a list Behavior
 let rec dyncolB lb evt = 
    let isSome x =  match x with 
                    |None -> false
                    |Some _ -> true
    let bf t = let (r,nb) = List.unzip (List.filter (fst >> isSome) 
                                                    (List.map (fun b -> atB b t) lb ))
               let proc () = let l = List.map (fun x -> x()) nb
                             let (re, ne) = atE evt t
                             let l' = match re with
                                      |None -> l
                                      |Some b -> b@l
                             dyncolB l' (ne())
               (catOption r, proc)       
    Beh bf


 type 'a DynColElem = DynColElem of ('a option * (unit->'a DynColElem Behavior) list)
 with
    static member getValue (DynColElem (x, _)) = x
    static member getBehaviorGenerators (DynColElem (_, l)) = l
    
 // 'a DynColElem Behavior list -> 'a DynColElem Behavior list Event -> 'a list Behavior
 let rec dyncolB2 lb evt = 
    let bf t = let rs =  (List.map (fun b -> atB b t) lb )
               let (r,nb) = List.unzip rs
               let chooser = (fun e ->  isSome (DynColElem.getValue (fst e)))
               let proc () = let l = List.map (fun x -> (snd x)()) (List.filter chooser rs)
                             let (re, ne) = atE evt t
                             let l' = match re with
                                      |None -> l
                                      |Some b -> b@l
                             let proc l (DynColElem (_, bgl)) = l @ List.map (fun x -> x()) bgl
                             let l'' = List.fold proc l' r
                             dyncolB2 l'' (ne())
               let chooser (DynColElem (x, _)) = x
               ( (List.choose DynColElem.getValue r), proc)       
    Beh bf       

 let rec split l =
    let rec proc  ll e lr =
            match lr with
            |[] -> [(e, ll)]
            |h::t -> (e, (ll @ lr)) :: proc (ll @[e])  h t
    match l with
    |[] -> []
    |h::t -> proc [] h t


 type 'a ColItem = 
    |ColItem of ('a  * ( 'a -> 'a -> (unit -> 'a ColItem option Behavior) option))


 let rec dyncolWithCollisionB lb evt = 
    let isSome x =  match x with 
                    |None -> false
                    |Some _ -> true
    let bf t = let r =  (List.filter (fst >> isSome) (List.map (fun b -> atB b t) lb ))
               let r' = List.map (fun (x,y) -> let x' = getSome x
                                               (x' , y)) r
               let splitted = split r'
               let procCol (ColItem (v1, colf)) (ColItem (v2, _)) =
                    colf v1 v2
               let rec procCols ((v, bv), l) =
                    match l with 
                    |[] -> (Some v, bv)
                    |(vh, _)::t -> match procCol v vh with
                                   |Some nb ->(Some v, nb)
                                   |None -> procCols ((v,bv), t)
               let (r'', nbr'') = List.unzip  (List.map procCols splitted)
               let proc () = let l = List.map (fun x -> x()) nbr''
                             let (re, ne) = atE evt t
                             let l' = match re with
                                      |None -> l
                                      |Some b -> b::l
                             dyncolWithCollisionB l' (ne())
               (List.map (fun (ColItem (x, _)) -> x) (catOption r''), proc)       
    Beh bf


 let rec colB lb = 
    let isSome x =  match x with 
                    |None -> false
                    |Some _ -> true
    let bf t = let (r,nb) = List.unzip (List.map (fun b -> atB b t) lb )
               let proc () = let l = List.map (fun x -> x()) nb
                             colB l 
               (r, proc)       
    Beh bf

    
    
       
              
              
              
         
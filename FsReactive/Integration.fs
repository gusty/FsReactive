namespace FsReactive

open FsReactive
 
module Integration = 
 
    // aliasB : 'a -> ('a Behavior * ('a -> unit))

    let aliasB x0 =
        let xref = ref x0
        let rec bb = Beh (fun _ -> (!xref, fun()->bb))
        bb, fun x -> xref := x
  
    // bindAliasB : 'a Behavior -> ('b Behavior * ('a -> unit)) -> 'a Behavior

    let bindAliasB xb (Beh bbf', fAliasB) =
        let rec bf (Beh bbf) t =
            let r, nb = bbf t
            fAliasB r
            let r', _ = bbf' t
            r, fun() -> Beh (bf (nb()))
        memoB (Beh (bf xb))
    

  
    // integrateGenB : ('a -> Time -> 'a) -> 'a Behavior -> Time -> 'a -> 'a Behavior

    let inline integrateGenB (mult: 'a -> Time -> 'a) b t0 i0 = 
        let plus (a:'a) (b:'a) = a + b : 'a
        let rec bf b t0 i0 t = 
            let r, nb = atB b t
            let i = plus i0 (mult r (t-t0))
            i, fun() -> Beh (bf (nb()) t i)
        Beh (bf b t0 i0)
     
    // integrate : float Behavior -> Time -> float -> float Behavior

    let integrate b t0 i0 = integrateGenB (*) b t0 i0


    // integrateGenB : ('a -> Time -> 'a) -> ('a -> 'a) Behavior -> 'a Behavior -> Time -> 'a -> 'a Behavior
  
    let inline integrateWithConstraintsGenB (mult: 'a -> Time -> 'a) constraintsBf b t0 i0 =
        let plus (a:'a) (b:'a) = a + b : 'a
        let rec bf constraintsBf b t0 i0 t = 
            let r, nb = atB b t
            let i = plus i0 (mult r (t-t0))
            let rcf, ncB = atB constraintsBf t
            let i' = rcf i
            i', fun() -> Beh (bf (ncB()) (nb()) t i')
        Beh (bf constraintsBf b t0 i0 )   
    

    // integrateWithConstraints :  (float -> float) Behavior -> float Behavior -> Time -> float -> float Behavior
  
    let integrateWithConstraints b t0 i0 = integrateWithConstraintsGenB (*) b t0 i0
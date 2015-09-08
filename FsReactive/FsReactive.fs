namespace FsReactive

open Misc
 
module FsReactive = 


    // Reactive

    type Time = float
    
    type 'a Behavior = Beh of (Time -> ('a * (unit -> 'a Behavior)))
    
    type 'a Event = Evt of (Time -> ('a option * (unit -> 'a Event)))
    
    let atB (Beh bf) = bf
    let atE (Evt bf) = bf


    // memoization

    let private memoG unwrap wrap b =
        let cache = ref None
        let bref  = ref b
        let compute x t = 
            let r, nb = unwrap x t
            cache := Some (t, r)
            r, nb
        let rec bf t = match !cache with
                        | Some (t0, r) when t = t0 -> r, fun () -> resB
                        | Some (t0, r) when t < t0 -> failwith "error"
                        | _ -> 
                            let r, nb = compute !bref t
                            r, fun () -> 
                                    bref := nb()
                                    resB
        and resB = wrap bf
        resB

    let memoB x = x |> memoG (fun (Beh x) -> x) Beh
    let memoE x = x |> memoG (fun (Evt x) -> x) Evt


    // applicative functor 
    
    let rec pureB f = Beh (fun t -> (f, fun() -> pureB f))

    let rec (<.>) (Beh f) (Beh baf) = 
        let bf t = 
            let r, nb = baf t
            let rf, nbf = f t
            rf r, fun () -> nbf () <.>  nb ()
        Beh bf

    let rec pureE f = Evt (fun t -> (Some f, fun() -> pureE f))

    let rec (<..>) (Evt f) (Evt baf) = 
        let bf t = 
            let r, nb = baf t
            match f t with
            | Some rf, nbf -> rf r, fun () -> (nbf()) <..> nb ()
            | None   , nbf -> None, fun () -> (nbf()) <..> nb ()
        Evt bf


    //Some common Behaviors
    
    // timeB : Time Behavior

    let rec timeB = Beh (fun t -> (t, fun() -> timeB))
 
    // deltaTimeB : Time -> Behavior 

    let deltaTimeB t0 = 
        let rec bf t0 t = t-t0, fun() -> Beh (bf t)
        Beh (bf t0)
                     


    // switchB : 'a Behavior -> 'a Behavior Event -> 'a Behavior

    let switchB b e =
        let rec bf (Beh b) (Evt e) t = 
            let r, nb = b t
            let proc() =
                match e t with
                | None     , ne -> Beh (bf (nb()) (ne()))
                | Some newB, ne -> Beh (bf  newB  (ne()))
            r, proc
        Beh (bf b e)

    // val untilB : 'a Behavior -> 'a Behavior Event -> 'a Behavior

    let untilB b e =
        let rec bf (Beh b) (Evt e) t =
            let r, nb = b t
            let proc() =
                match e t with
                | None     , ne -> Beh (bf (nb()) (ne()))
                | Some newB, ne ->
                    ne() |> ignore
                    newB
            r, proc
        Beh (bf b e)



    // events

    let rec noneE   = Evt (fun t -> (None  , fun() -> noneE  ))
    let rec someE v = Evt (fun t -> (Some v, fun() -> someE v))

    // val ( =>> ) : 'a Event -> ('a -> 'b) -> 'b Event
 
    let (=>>) evt f = 
        let proc = function
            | Some evt -> Some (f evt) 
            | None     -> None
        let rec bf evt t = 
            let r, nevt = atE evt t
            proc r, fun() -> Evt (bf (nevt()) )
        Evt (bf evt)
    
    
    // val ( --> ) : 'a Event -> 'b -> 'b Event

    let (-->) ea b = ea =>> (fun _ -> b)
  
    // val snapshotE : 'a Event -> 'b Behavior -> ('a * 'b) Event

    let snapshotE evt b =
        let rec bf (Evt evt) (Beh b) t = 
            let r, nb = b t
            match evt t with
            | Some v, nevt -> Some (v, r), fun() -> Evt (bf (nevt()) (nb()) )
            | None  , nevt -> None       , fun() -> Evt (bf (nevt()) (nb()) )
        Evt (bf evt b)

    // val snapshotBehaviorOnlyE : 'a Event -> 'b Behavior -> 'b Event

    let rec snapshotBehaviorOnlyE evt b = snapshotE evt b =>> (fun (_, x) -> x)
     
    // val stepB : 'a -> 'a Event -> 'a Behavior

    let stepB (a:'a) (evt:'a Event) = switchB (pureB a) (evt =>> pureB)

 
    // val stepAccumB : 'a -> ('a -> 'a) Event -> 'a Behavior

    let rec stepAccumB a (Evt evt) = 
        let bf t = 
            let re, ne = evt t
            let a = match re with
                    | Some f -> f a
                    | None   ->   a
            a, fun() -> stepAccumB a (ne())
        Beh bf
    

 
    // val ( .|. ) : 'a Event -> 'a Event -> 'a Event
     
    let rec  (.|.)  ea  eb  = 
        let proc = function
            | Some a, _      -> Some a
            | None  , Some b -> Some b
            | None  , None   -> None
        let bf (Evt ea) (Evt eb) t = 
            let ra, nea = ea t
            let rb, neb = eb t
            proc (ra, rb), fun() -> nea()  .|. neb()
        Evt (bf ea eb)


    // orE : ('a -> 'a -> 'a) -> 'a Event -> 'a Event -> 'a Event

    let orE comp ea eb = 
        let proc = function
            | Some a, Some b -> Some (comp a b)
            | Some a, None   -> Some a
            | None  , Some b -> Some b
            | None  , None   -> None
        let rec bf (Evt ea) (Evt eb) t = 
            let ra, nea = ea t
            let rb, neb = eb t
            proc (ra, rb), fun() -> Evt (bf (nea()) (neb()))
        Evt (bf ea eb)



    // val ( .&. ) : 'a Event -> 'b Event -> ('a * 'b) Event
 
    let rec (.&.) ea eb = 
        let proc = function (Some va , Some vb) -> Some (va, vb) | _ -> None
        let bf ea eb t = 
            let ra, nea = atE ea t
            let rb, neb = atE eb t
            proc (ra, rb), fun() -> nea() .&. neb()
        Evt (bf ea eb)
    

    // val iterE : 'a Event -> 'b list -> ('a * 'b) Event

    let rec iterE (Evt evt) = function  
        | []                -> noneE
        | head::tail as lst -> 
            let bf t = 
                let re, ne = evt t
                match re with
                | Some v -> Some (v, head), fun() -> iterE (ne()) tail
                | None   -> None          , fun() -> iterE (ne()) lst
            Evt bf  
                    
                    
    // val loopE : 'a Event -> 'b list -> ('a * 'b) Event
                    
    let rec loopE (Evt evt) = function  
        | []                -> noneE
        | head::tail as lst -> 
            let bf t = 
                let re, ne = evt t
                match re with
                | Some v -> Some (v,head), fun() -> loopE (ne()) (tail @ [head])
                | None   -> None         , fun() -> loopE (ne()) lst
            Evt bf


    // val whileE : bool Behavior -> unit Event

    let rec whileE (Beh b) =
        let bf t = 
            let r, nb = b t
            if r then Some (), fun() -> whileE (nb())
            else      None   , fun() -> whileE (nb())
        Evt bf


    // val whenE : bool Behavior -> unit Event

    let whenE b =
        let rec bf (Beh b) previous t = 
            let r, nb = b t
            match previous, r with
            | false, true -> Some (), fun() -> Evt (bf (nb()) r)
            | _           -> None   , fun() -> Evt (bf (nb()) r)
        Evt (bf b false)


    // val whenBehaviorE : 'a option Behavior -> 'a Event

    let whenBehaviorE b =
        let rec bf (Beh b) previous t = 
            let r, nb = b t
            match previous, r with
            | None, Some v -> Some v, fun() -> Evt (bf (nb()) r)
            | _            -> None  , fun() -> Evt (bf (nb()) r)
        Evt (bf b None)
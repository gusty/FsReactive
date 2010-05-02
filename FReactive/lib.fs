#light




namespace FReactive

 open Misc
 
 module Lib = 
 
 open System
 open Misc
 open FReactive
 
 // debug
 let rec tronB msg b = 
    let bf t = let (r, nb) = atB b t
               printf "%s: (t=%f) v=%s \n" msg t (any_to_string r)
               (r, fun() -> tronB msg (nb()))
    Beh bf
    
  
 let rec tronE msg e = 
    let bf t = let (r, ne) = atE e t
               printf "%s: (t=%f) v=%s \n" msg t (any_to_string r)
               (r, fun() -> tronE msg (ne()))
    Evt bf
        
 // some constants
 let zeroB = pureB 0.0
 let oneB = pureB 1.0
 let twoB = pureB 2.0
  
 let piB = pureB Math.PI
  
 let trueB = pureB true
 let falseB = pureB false
 
 let rec noneB() = Beh (fun _ -> (None, noneB))
 
 let couple x y = (x,y)
 let coupleB() = pureB couple 
 let triple x y z= (x,y,z)
 let tripleB() = pureB triple 
 
  
 // lifting of classical functions
 
 let (.*.) (a:Behavior<float>) b = pureB (*) <$> a <$> b 
 let (./.) (a:Behavior<float>) b = pureB (/) <$> a <$> b 
 let (.+.) (a:Behavior<float>) b = pureB (+) <$> a <$> b 
 let (.-.) (a:Behavior<float>) b = pureB (-) <$> a <$> b 
  
 let rec negB (a:Behavior<float>)  = pureB (fun x -> -x) <$> a
 
 let foo f = fun (b:Behavior<_>) -> pureB f <$> b
 
 let (.>.) (a:Behavior<_>) b = pureB (>) <$> a <$> b
 let (.<.) (a:Behavior<_>) b = pureB (<) <$> a <$> b
 let (.>=.) (a:Behavior<_>) b = pureB (>=) <$> a <$> b
 let (.<=.) (a:Behavior<_>) b = pureB (<=) <$> a <$> b
 let (.=.) (a:Behavior<_>) b = pureB (=) <$> a <$> b
 let (.<>.) (a:Behavior<_>) b = pureB (<>) <$> a <$> b

 let (.&&.) (a:Behavior<_>) b = pureB (&&) <$> a <$> b
 let (.||.) (a:Behavior<_>) b = pureB (||) <$> a <$> b

 let notB (a:Behavior<_>)  = pureB (not) <$> a 
 
 // very degenerated option monad
 let mapOption f v = 
    match v with
    |Some v -> Some (f v)
    |_ -> None

 
 type Discontinuity<'a, 'b> = Disc of ('a Behavior *  (Time -> 'a -> 'b) Event * (Time -> 'a -> (Time -> 'a -> 'b) ->  Discontinuity<'a, 'b>))
    
 let rec discontinuityE (Disc (xB, predE, bg))  = 
        let evt = snapshotE predE ((coupleB() <$> timeB <$> xB))  =>>  
                        (fun (e,(t,vb)) -> let disc = bg t vb e
                                           discontinuityE disc)
        untilB xB evt

 let rec seqB ba bb = 
    let bf t = let (_, na) = atB ba t
               let (b, nb) = atB bb t
               (b, fun() -> seqB (na()) (nb()))
    Beh bf
               


 type 'a IdentifiedObject =
    |IdItem of int * 'a
    with
        static member id = fun item -> let (IdItem (id, _)) = item in id
        static member item = fun item -> let (IdItem (_, (x:'a))) = item in x

 let createId =
        let  id = ref  0
        fun () -> let i = !id
                  id := i+1
                  i

 let rec mkIdObjB b =
        let i = createId() 
        let rec proc b = 
            let rec bf t = let (r, nb) = atB b t
                           (IdItem (i, r), fun () -> proc (nb()))
            Beh bf
        proc b

 let rec waitE delta = 
    let rec bf2 tend t = if t >= tend 
                         then (Some (), fun () -> NoneE)
                         else (None, fun () -> Evt (bf2 tend))
    let bf t = (None, fun () -> Evt (bf2 (t+delta)))
    Evt bf

    
 let startB fb = 
        let bf t = let b = fb t
                   atB b t
        Beh bf

        
 let rec periodicB period = 
            let E2 = (someE ()) =>> (fun () -> periodicB period)
            let E1 = (waitE period) --> ( untilB (pureB true) E2)
            untilB (pureB false) E1

 let mapOptionB f = pureB (mapOption f)
 let someizeBf b = (pureB Some) <$> b
    
 let delayB b v0 = 
        let rec bf b v0 t = let (r, nb) = atB b t
                            (v0, fun () -> Beh (bf (nb()) r))
        Beh (bf b v0)


 
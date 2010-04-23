#light

namespace FReactive

 open FReactive
 
 module Integration = 
 
  let aliasB x0 =
    let xref = ref x0
    let rec bb = Beh (fun _ -> (!xref, fun()->bb))
    (bb, fun x -> xref := x)
  
  let bindAliasB xb aliasB =
    let rec bf aliasedB t = let  (Beh bbf) = aliasedB
                            let (r,nb) = bbf t
                            (r,fun()-> (snd aliasB) r
                                       Beh (bf (nb())))
    memoB (Beh (bf xb))
    
  type NumClass<'a, 'b> = 
    {plus : 'a -> 'a -> 'a;
     minus : 'a -> 'a -> 'a;
     mult : 'a -> 'b -> 'a;
     div : 'a -> 'b -> 'a;
     neg : 'a -> 'a
    }
    
  let floatNumClass = 
    { plus = (+);
      minus = (-);
      mult = (*);
      div = (/);
      neg = (fun (x:float) -> -x)
    }
  
  
  let integrateGenB numClass b t0 i0 = 
    let rec bf b t0 i0 t = let (r,nb) = atB b t
                           let i = numClass.plus i0 (numClass.mult r (t-t0))
                           (i, fun() -> Beh (bf (nb()) t i))
    Beh (bf b t0 i0 )   
    
  let integrate b t0 i0 = integrateGenB floatNumClass b t0 i0

  
  let integrateWithConstraintsGenB numClass constraintsBf b t0 i0 = 
    let rec bf constraintsBf b t0 i0 t = 
                           let (r,nb) = atB b t
                           let i = numClass.plus i0 (numClass.mult r (t-t0))
                           let (rcf, ncB) = atB constraintsBf t
                           let i' = rcf i
                           (i', fun() -> Beh (bf (ncB()) (nb()) t i'))
    Beh (bf constraintsBf b t0 i0 )   
    
  let integrateWithConstraints b t0 i0 = integrateWithConstraintsGenB floatNumClass b t0 i0



//namespace FReactive

//
//open Misc;

//module A = 
 //open FReactive;
//
//open FReactive
 
 
//let f l = catOption l
 
  (*
let integrate f t0 (x0:float) =
    let rec bf (Beh f)  i0 t0 t =
        let (r,nb) = f t
        let i = i0+r*(t-t0)
        let nbf() = Beh (bf (nb()) t i)
        (i, nbf)
    Beh (bf f t0 x0)

let solve f t0 (x0:float) = 
    let xref = ref x0
    let rec xb = Beh (fun _ -> (!xref, fun() -> xb))

    let fb = f xb
    let rec bf (Beh ibf) t0 t =
        let (r, nb) = ibf t
        xref := !xref + r * (t-t0)
        //printf "time = %s\n" (any_to_string t)
        let nsolve() = Beh (bf (nb()) t)
        (!xref, nsolve)
    Beh (bf fb  t0)



let solven fl t0 xl0 =
    let xrefl = List.map ref xl0
    let mkBeh xref = let rec xb = Beh (fun _ -> (!xref, fun() -> xb))
                     xb
    let xbl = List.map mkBeh xrefl
    let fbl = List.map (((|>) xbl) >> delay) fl
    let rec solver fbl t0 t =
        let (rl, nbl) = List.unzip (List.map ((($<)()) >> (fun (Beh bf) -> bf t)) fbl)
        List.iter2 (fun xref r ->  xref := !xref + r * (t-t0)) xrefl rl
        solverRef := (memoize (solver nbl t))
    and solverRef = ref (memoize (solver fbl t0))
    let rec bf xref t = (!solverRef) t
                        let nb() = Beh (bf xref)
                        (!xref, nb)
    List.map (fun xref -> Beh (bf xref) ) xrefl
       
    


//=========================


let rec liftB f (Beh af) =
    let bf t = let (r, nb) = af t
               (f r, fun() -> liftB f (nb()))
    Beh bf

let (fB:float Behavior -> float Behavior)  = idB

let f1B b = match b with
            |b1::b2::[] -> b2
let f2B b = match b with
            |b1::b2::[] -> (liftB (( * )(-1.0)))  b1

let fbl = [f1B; f2B]
let x0l = [0.0;1.0]


let ls = Seq.to_list (seq {for i in [1..10] -> (float)i})
let ls2 = Seq.to_list (seq {for i in [11..20] -> (float)i})
let ls3 = Seq.to_list (seq {for i in [1..500] -> ((float)i)/10.0})


let xbl = solven fbl 0.0 x0l
let b1 = List.hd xbl
let b2 = (List.hd (List.tl  xbl))

let rec combine (Beh bf1) (Beh bf2) =
    let bf t = let (r1, nb1) = bf1 t
               let (r2, nb2) = bf2 t
               ( (r1,r2), fun() -> combine (nb1()) (nb2()) )
    Beh bf
   
//let b = combine b1 (idB b1)
let b = combine b1 b2

runList b1 [1.0; 2.0; 3.0; 3.0; 3.0; 4.0; 5.0]
runList b1 ls2

runList b ls3


List.map (fun x -> printf "%s\n" (any_to_string x)) (runList b ls3)

runList b1 ls

runList (List.hd xbl) ls
runList (List.hd (List.tl  xbl)) ls

let r  = (runAll b 1000000.0)

runList b1 ls3


 *)
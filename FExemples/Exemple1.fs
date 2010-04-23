#light

//namespace FExemples

#r @"..\FReactive\obj\Debug\FReactive.dll"
#r @"..\Graphics\obj\Debug\Graphic.dll"

open FReactive.Misc
open FReactive.FReactive
open FReactive.Integration
open FReactive.DynCol
open Graphic.Picture
open Graphic.Draw
open System
open System.Windows.Forms


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
    let bf t = let r =  (List.filter (fst >> isSome) //fun (ColItem (v, _)) -> isSome v) 
                                                    (List.map (fun b -> atB b t) lb ))
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
               (r'', proc)       
    Beh bf



 let undelay delayedB = 
    let bf t = let b = delayedB()
               atB b t
    Beh bf
 
 let rec nullB = Beh (fun _->((), fun() -> nullB))
  
 let random = let r = new Random()
              fun () -> r.NextDouble()  
               
 let randomB = (pureB (random)) <$> nullB
 
 let curry f a b = f (a,b)
 let uncurry f (a, b) = f a b
  
 let createWindowEventE (evt:IEvent<EventHandler,EventArgs>) = 
    let n = ref []
    let add r = n:= r::(!n)
    let handle x = List.iter (fun r -> r x) !n
                   n:=[]
    evt.Add handle
    add
    
    

 let rec bindEvt evt = 
    let eventValue = ref None
    evt (fun v -> eventValue := Some v)
    let rec bf t = match !eventValue with
                   |None -> (None, fun() -> Evt bf)
                   |Some v -> let ne = bindEvt evt
                              (Some v, fun() -> ne)
    Evt bf
  
 //module FExemples = 
  
  let (.*) a b = (pureB (*)) <$> a <$> b 
  let cosB = pureB Math.Cos
  let sinB = pureB Math.Sin
  let (.<=) a b = pureB (<=) <$> a <$> b
  let (.>=) a b = pureB (>=) <$> a <$> b
  
  (*
  // ball on a circle
  let pict1B = let colPictB = pureB (fun shape color -> ColoredPicture (shape, color))
               let circleB = pureB (fun x y -> Circle {centre= {x=x; y=y} ; radius=15.0})
               let colorB = pureB (Color (128, 128, 128))
               let freqB = pureB (fun t -> 2.0*Math.PI/25.0*t)
               let x = (pureB 30.0) .* (cosB <$> (freqB <$> timeB))
               let y = (pureB 30.0) .* (sinB <$> (freqB <$> timeB))
               colPictB <$> (circleB <$> x <$> y) <$> colorB


  // bouncing ball
  let pict2B = let colPictB = pureB (fun shape color -> ColoredPicture (shape, color))
               let circleB = pureB (fun x y -> Circle {centre= {x=x; y=y} ; radius=15.0})
               let colorB = pureB (Color (128, 128, 128))
               let condf = pureB (fun x -> x <= (-100.0) || x >= 100.0)
               let rec x' = aliasB 0.0
               let rec condE = whenE (condf <$> (fst x')) --> (~-)
               and speed = stepAccumB 40.0 condE          
               and x = bindAliasB (integrate speed 0.0 0.0) x'
               colPictB <$> (circleB <$> x <$> (pureB 0.0)) <$> colorB
*)
  // bouncing ball
  let red = (Color (255, 0, 0))
  
  let boxPict = 
            let v1 = {x= -70.0; y= -70.0}
            let v2 = {x= 70.0; y= -70.0}
            let v3 = {x= 70.0; y= 70.0}
            let v4 = {x= -70.0; y= 70.0}
            let v01 = {x= -70.0; y= 0.0}
            let v02 = {x= 70.0; y= 0.0}
            let e1 = {p1=v1;p2=v2}
            let e2 = {p1=v2;p2=v3}
            let e3 = {p1=v3;p2=v4}
            let e4 = {p1=v4;p2=v1}
            let ex = {p1=v01;p2=v02}
            let ep1 = ColoredPicture ((Edge e1),red)
            let ep2 = ColoredPicture ((Edge e2),red)
            let ep3 = ColoredPicture ((Edge e3),red)
            let ep4 = ColoredPicture ((Edge e4),red)
            let epx = ColoredPicture ((Edge ex),red)
            let p = Over (ep1, Over (ep2, Over (ep3,Over (ep4, epx))))
            //let pp =Over (p, ColoredPicture (Circle {centre= {x=0.0; y=0.0} ; radius=30.0}, red))
            p
            
            
  
  let pict3B = let boxB = pureB boxPict 
               let colPictB = pureB (fun shape color -> ColoredPicture (shape, color))
               let circleB = pureB (fun x y -> Circle {centre= {x=x; y=y} ; radius=10.0})
               let colorB = pureB (Color (128, 128, 128))
               let condxf = pureB (fun x -> x <= (-65.0) || x >= 65.0)
               let condyf = pureB (fun y -> y <= (-65.0) || y >= 65.0)
               let rec x' = aliasB 0.0
               and y' = aliasB 0.0
               let rec condxE = whenE (condxf <$> (fst x')) --> (~-)
               let rec condyE = whenE (condyf <$> (fst y')) --> (~-)
               and speedx = stepAccumB 20.0 condxE          
               and speedy = stepAccumB 18.0 condyE          
               and x = bindAliasB (integrate speedx 0.0 0.0) x'
               and y = bindAliasB (integrate speedy 0.0 0.0) y'
               let cb = colPictB <$> (circleB <$> x <$> y) <$> colorB
               pureB(curry Over) <$> boxB <$> cb
               

 run 5 pict3B 
  
  let (form:MyForm) = (new MyForm()) //:>Form
 
  let clickWEvt = createWindowEventE (form.Click)
 
  let clickE = bindEvt clickWEvt
 
 
  let rec waitE remain = 
    let bf remain t = printfn "ok %A" remain
                      let remain' = remain - t
                      if remain' < 0.0 then printfn "fini" 
                                            (Some(), fun() -> NoneE)
                                       else (None, fun() -> waitE remain')
    Evt (bf remain)
 
  let pict3B = let boxB = pureB boxPict 
               let colPictB = pureB (fun shape color -> ColoredPicture (shape, color))
               let circleB = pureB (fun x y -> Circle {centre= {x=x; y=y} ; radius=10.0})
               let colorB = pureB (Color (128, 128, 128))
               let condxf = pureB (fun x -> (*printfn "%A" x ;*) x <= (-65.0) || x >= 65.0)
               let condyf = pureB (fun y -> y <= (-65.0) || y >= 65.0)
               let rec x' = aliasB 0.0
               and y' = aliasB 0.0
               let rec condxE = whenE (condxf <$> (fst x')) --> (~-)
               let rec condyE = whenE (condyf <$> (fst y')) --> (~-)
               and speedx = stepAccumB 20.0 condxE          
               and speedy = stepAccumB 18.0 condyE          
               and x = bindAliasB (integrate speedx 0.0) x'
               and y = bindAliasB (integrate speedy 0.0) y'
               let cb = colPictB <$> (circleB <$> x <$> y) <$> colorB
               let ballB = (pureB(curry Over) <$> boxB <$> cb) 
               let ballB' = untilB ballB ((waitE 5.0)  --> (pureB EmptyPicture))
               untilB (pureB EmptyPicture) (clickE --> ballB')
               
 
 run form 20 pict3B
  
  
  
  
  
  
  
  // dyncol
  
  
  let (form:MyForm) = (new MyForm()) //:>Form
 
  let clickWEvt = createWindowEventE (form.Click)
 
  let clickE = bindEvt clickWEvt
 
 
  let rec waitE remain = 
    let bf remain t = let remain' = remain - t
                      if remain' < 0.0 then (Some(), fun() -> NoneE)
                                       else (None, fun() -> waitE remain')
    Evt (bf remain)
 
  let undelay delayedB = 
    let bf t = let b = delayedB()
               atB b t
    Beh bf
    

  

 
  let pict3B = let boxB = pureB boxPict 
               let colPictB = pureB (fun shape color -> ColoredPicture (shape, color))
               let circleB = pureB (fun x y -> Circle {centre= {x=x; y=y} ; radius=10.0})
               let colorB = pureB (Color (128, 128, 128))
               let condxf = pureB (fun x -> (*printfn "%A" x ;*) x <= (-65.0) || x >= 65.0)
               let condyf = pureB (fun y -> y <= (-65.0) || y >= 65.0)
               let newBall () = 
                    let speedChange x = let nx = -x * (1.0 + random()/2.0)
                                        if (Math.Abs nx) > 30.0 then 30.0 * ((float) (Math.Sign nx));
                                                               else nx
                    let rec x' = aliasB 0.0
                    and y' = aliasB 0.0
                   // let rec condxE = whenE (condxf <$> (fst x')) --> (~-)
                   // let rec condyE = whenE (condyf <$> (fst y')) --> (~-)
                    let rec condxE = whenE (condxf <$> (fst x')) --> speedChange
                    let rec condyE = whenE (condyf <$> (fst y')) --> speedChange
                    and speedx = stepAccumB 20.0 condxE          
                    and speedy = stepAccumB 18.0 condyE          
                    and x = bindAliasB (integrate speedx 0.0) x'
                    and y = bindAliasB (integrate speedy 0.0) y'
                    let cb = colPictB <$> (circleB <$> x <$> y) <$> colorB
                    let ballB = (pureB(curry Over) <$> boxB <$> cb) 
                    let ballB' = untilB ballB ((waitE 25.0)  --> (pureB EmptyPicture))
                    ballB'
               //untilB (pureB EmptyPicture) (clickE --> ballB')
               let overizeList l =  List.fold (fun p e -> Over (p, e)) EmptyPicture l
               (pureB overizeList) <$> (dyncolB [] (clickE --> ((pureB Some) <$> (undelay newBall))))
               
 
 run form 20 pict3B
  
  
let f h =  List.fold_left (fun p e -> Over (p, e)) EmptyPicture h
  
  
  
  
  
  
  // collision entre 2 corps
  
  
  let (form:MyForm) = (new MyForm()) //:>Form
 
  let clickWEvt = createWindowEventE (form.Click)
 
  let clickE = bindEvt clickWEvt
 
 
  let rec waitE remain = 
    let bf remain t = printfn "ok %A" remain
                      let remain' = remain - t
                      if remain' < 0.0 then printfn "fini" 
                                            (Some(), fun() -> NoneE)
                                       else (None, fun() -> waitE remain')
    Evt (bf remain)
 
  let rec pairB b1 b2 = 
    let bf t = let (r1,nb1) = atB b1 t
               let (r2,nb2) = atB b2 t
               ((r1,r2) , fun() -> pairB (nb1()) (nb2()) )
    Beh bf
    
    
  let pict3B = let boxB = pureB boxPict 
               let ballPict =  pureB (fun (x,y) -> ColoredPicture (Circle {centre= {x=x; y=y} ; radius=10.0}, (Color (128, 128, 128))))
             //  let colPictB = pureB (fun shape color -> ColoredPicture (shape, color))
              // let circleB = pureB (fun x y -> Circle {centre= {x=x; y=y} ; radius=10.0})
              // let colorB = pureB (Color (128, 128, 128))
               let condxf = pureB (fun x -> (*printfn "%A" x ;*) x <= (-65.0) || x >= 65.0)
               let condyf = pureB (fun y -> y <= (-65.0) || y >= 65.0)
               let newBall x0 y0 vx vy collisionE = 
                    let speedChange x = let nx = -x * (1.0 + random()/2.0)
                                        if (Math.Abs nx) > 30.0 then 30.0 * ((float) (Math.Sign nx));
                                                               else nx
                    let rec x' = aliasB 0.0
                    and y' = aliasB 0.0
                   // let rec condxE = whenE (condxf <$> (fst x')) --> (~-)
                   // let rec condyE = whenE (condyf <$> (fst y')) --> (~-)
                    let rec condxE = whenE (condxf <$> (fst x')) --> speedChange
                    let rec condyE = whenE (condyf <$> (fst y')) --> speedChange
                    and speedx = stepAccumB vx condxE          
                    and speedy = stepAccumB vy condyE          
                    and x = bindAliasB (integrate speedx x0) x'
                    and y = bindAliasB (integrate speedy y0) y'
                    let ballB =  untilB ((pureB Some) <$> (pairB x y)) (NoneE  --> (pureB None))
                    ballB
               let rec ball1 = newBall 20.0 20.0 20.0 18.0 collisionE
               and ball2 = newBall (-20.0) (-20.0) (-20.0) (-18.0) collisionE
               and collisionE b1 b2 = 
                let bf t =  let (r1,nb1) = atB b1 t
                            let (r2,nb2) = atB b1 t
                            let r = match (r1,r2) with
                                    |(None, None)->None
                                    |(None, _) -> None
                                    |(_, None) -> None
                                    |(Some (x1:float,y1), Some(x2:float,y2)) -> if Math.Abs(x1-x2) < 10.0 then Some() else None
                            (r, fun () -> collisionE (nb1()) (nb2()))
                Evt bf
               let overizeList l =  List.fold (fun p e -> Over (p, e)) EmptyPicture l
               let ballPictB1 = ballPict <$> ball1
               let ballPictB2 = ballPict <$> ball2
               (pureB overizeList) <$> (dyncolB [ballPictB1; ballPictB2] NoneE)
               
 
 run form 20 pict3B
  
  
  
  
  
  
  
  
  
  
  
  let pict3B = let colPictB = pureB (fun shape color -> ColoredPicture (shape, color))
               let circleB = pureB (fun x y -> Circle {centre= {x=x; y=y} ; radius=10.0})
               let redB = pureB (Color (255,0,0))
               let greenB = pureB (Color (0,255,0))
               let colorB = switchB greenB ((loopE  clickE  [redB; greenB]) =>> snd)
               let cb = colPictB <$> (circleB <$> (pureB 0.0) <$> (pureB 0.0) ) <$> colorB
               cb
              
 
 form.Show()
 
 
 .Add (fun _ -> printfn "Stopping timer")
  let pict3B = let boxB = pureB boxPict 
               let colPictB = pureB (fun shape color -> ColoredPicture (shape, color))
               let circleB = pureB (fun x y -> Circle {centre= {x=x; y=y} ; radius=10.0})
               let colorB = pureB (Color (128, 128, 128))
               let condxf = pureB (fun x -> x <= (-65.0) || x >= 65.0)
               let condyf = pureB (fun y -> y <= (-65.0) || y >= 65.0)
               let rec x' = aliasB 0.0
               and y' = aliasB 0.0
               let rec condxE = whenE (condxf <$> (fst x')) --> (~-)
               let rec condyE = whenE (condyf <$> (fst y')) --> (~-)
               and speedx = stepAccumB 20.0 condxE          
               and speedy = stepAccumB 18.0 condyE          
               and x = bindAliasB (integrate speedx 0.0 0.0) x'
               and y = bindAliasB (integrate speedy 0.0 0.0) y'
               let cb = colPictB <$> (circleB <$> x <$> y) <$> colorB
               pureB(curry Over) <$> boxB <$> cb
 
   
  
 let tronB b msg =
    pureB (fun x -> printfn "%A : %A" msg x; x) <$> b 
    
    

  let x      = let condf = pureB (fun x -> printfn "%A" x
                                           let r = x <= (-100.0) || x >= 100.0 
                                           r)
               let rec x' = aliasB 0.0
               let rec condE = whenE (condf <$> (fst x')) --> (~-)
//               and speed = stepAccumB 21.0 condE          
//               and speed = switchB (pureB 21.0) ((loopE  (whenE (condf <$> (fst x')))  [(pureB 21.0);(pureB (-21.0))]) =>> snd)
                 and speed = switchB (pureB 21.0) ((loopE  (whenE (tronB  (pureB true) "h"))  [(pureB 21.0);(pureB (-21.0))]) =>> snd)
               and x = bindAliasB (integrate speed 0.0 0.0) x'
               x
  


  let x      = let condf = pureB (fun x -> printfn "%A" x
                                           let r = x <= (-100.0) || x >= 100.0 
                                           r)
               let rec x' = aliasB 0.0
               let rec condE = whenE (condf <$> (fst x')) --> (~-)
               and speed = stepAccumB 21.0 condE          
               //and speed = switchB (pureB 21.0) ((loopE  (whenE (condf <$> (fst x')))  [(pureB -21.0);(pureB (+21.0))]) =>> snd)
               //and speed = switchB (pureB 21.0) ((loopE  (whileE (tronB  (pureB true) "h"))  [(pureB -21.0);(pureB (21.0))]) =>> snd)
               and x = bindAliasB (integrate speed 0.0 0.0) x'
               x
  
  
  let ls = Seq.to_list (seq { for i in 1..20 -> (float) i})
  
  runList x ls
  
  let x = switchB (pureB 21.0) ((loopE (whileE (pureB true)) [pureB 21.0;pureB (-21.0)]) =>> snd)
  
  
  
  
  let aliasB x0 =
    let xref = ref x0
    let rec bb = Beh (fun _ -> (!xref, fun()->bb))
    (bb, fun x -> xref := x)
  
  let bindAliasB xb aliasB =
    let rec bf aliasedB t = let  (Beh bbf) = aliasedB
                            let (r,nb) = bbf t
                            (snd aliasB) r
                            (r,fun()-> Beh (bf (nb())))
    (Beh (bf xb))
    
  
  let integrate b i0 t0 = 
    let rec bf b i0 t0 t = let (r,nb) = atB b t
                           let i = i0 + r * (t-t0)
                           (i, fun() -> Beh (bf (nb()) i t))
    Beh (bf b i0 t0)   
  
  
  
   
               
let createWindowEventE (evt:IEvent<EventHandler,EventArgs>) = 
    let n = ref []
    let add r = n:= r::(!n)
    let handle x = List.iter (fun r -> r x) !n
                   n:=[]
    evt.Add handle
    add
    
    

let rec bindEvt evt = 
    let eventValue = ref None
    evt (fun v -> eventValue := Some v)
    let rec bf t = match !eventValue with
                   |None -> (None, fun() -> Evt bf)
                   |Some v -> let ne = bindEvt evt
                              (Some v, fun() -> ne)
    Evt bf
  
  snapshotB
  
  let tickE dt = 
    let rec bf t0 t = if (t-t0)>=dt
                      then (Some(), fun() -> Evt (bf t))
                      else (None, fun() -> Evt (bf t0))
    Evt (bf 0.0)
    
  let x =  let cond = stepAccumB true (tickE 5.0 --> not)
           let w = switchB (pureB "bye")  (( loopE (whenE cond) [pureB "hello"; pureB "bye"]) =>> snd)
           w

  let x =  let cond = stepAccumB true (tickE 5.0 --> not)
           let w = switchB (pureB "bye")  (( loopE cond [pureB "hello"; pureB "bye"]) =>> snd)
           w


  let x =  let cond = stepAccumB true (tickE 5.0 --> not)
           let w = stepAccumB true ((whenE cond) --> not)
           w
  
  (*
    
  let  x = let x' = aliasB 1.0
           bindAliasB (integrate (fst x') 0.0 0.0) x'
  and 
  
  let ls = Seq.to_list (seq { for i in 1..10 -> (float) i})
  
  runList x ls
  
  let y= atB x 1.0
    
    
  let integrate f (Beh xbf) =
    let rec xb' = Beh (fun t -> (!(fst(xbf t)), fun () -> xb'))
    let (Beh fbf) = f xb'
    let rec bf t = let ((r:float), nfb) = fbf t
                   let i = !fst(xbf t) + r
                   fst(xbf t) := i
                   (i, fun() -> Beh (bf))
    Beh bf
    *)
    
let dyncolB lb evt = 
//    let run b t = let (r,nb) = atB b t
    let isSome x =  match x with 
                    |None -> false
                    |Some _ -> true
    let bf t = let (r,nb) = List.unzip (List.filter (fst >> isSome) 
                                                    (List.map (fun b -> atB b t) lb ))
               (r,nb)      
    bf
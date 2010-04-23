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
  
    
  

                 
             
        
  // dyncol avec collision
  
  type 'a M = {o:'a; n:string; keep:bool} 
  
  let f {o=_; n=n; keep=_} = n
  
  let f (x:int M) = x.n
  
  let rec dyncolB (lb:'a M Behavior list) evt =
    let bf t = let r = List.map (fun b -> atB b t) lb 
               let r' = List.filter (fst >> (fun x -> x.keep)) r
               let (rl, nbl) =  List.unzip r'
               let proc () = let l = List.map (fun x -> x()) nbl
                             let (re, ne) = atE evt t
                             let l' = match re with
                                      |None -> l
                                      |Some bl -> bl @ l
                             dyncolB l' (ne())
               (rl, proc)
    Beh bf
    
  let mvtB a b = 
    let rec bf t = (a*t+b,  fun() -> Beh bf)
    Beh bf

  let sinB period  = 
    let rec bf t = (Math.Sin(2.0*Math.PI*period/t),  fun() -> Beh bf)
    Beh bf

  let touch a bl t = 
    let proc a b t = let (ra,_) = atB a t
                     let (rb,_) = atB b t
                     if (ra.n = rb.n) then  false
                                      else Math.Abs((ra.o:float) - rb.o) < 1.0
    let r = List.fold_left (fun acc b -> acc || (proc a b t)) false bl
    r
      
      
      
    
  let rec detect l pred acc = 
    match l with 
    |[] -> acc
    |(h, nb)::t -> let r = List.fold_left (fun s (e, _) -> s || (pred h e)) false t
                   if r then detect l pred (nb::acc)  else detect l pred acc 
    
  
  // collision entre 2 corps
  
  
  let (form:MyForm) = (new MyForm()) //:>Form
 
  let clickWEvt = createWindowEventE (form.Click)
 
  let clickE = bindEvt clickWEvt
 
 
    
    
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
                    ballBV
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
               let overizeList l =  List.fold_left (fun p e -> Over (p, e)) EmptyPicture l
               let ballPictB1 = ballPict <$> ball1
               let ballPictB2 = ballPict <$> ball2
               (pureB overizeList) <$> (dyncolB [ballPictB1; ballPictB2] NoneE)
               
 
 run form 20 pict3B
  
  
  (*    
                 
    
  let rec b = 
    let rec bf c t = printfn "%A" t
                     ((t,c), fun() -> Beh (bf (c+1)))
    Beh (bf 0)
            
 
 let rec runList b l  =
    match l with
    |[] -> []
    |h::t -> let (r, nb) = atB b h
             r:: runList (nb()) t           
            
 let bb = (memoB b)
 runList  (pureB (fun x y -> x,y) <$> bb <$> bb)  [1.0;1.0;2.0;2.0;3.0;4.0;4.0;5.0] //[1.0;2.0;3.0;4.0]

 runList  (pureB (+) <$> b <$> b) [1.0;2.0;3.0;4.0]
 
 [1.0;1.0;2.0;2.0;3.0;4.0;4.0;5.0]
          *)
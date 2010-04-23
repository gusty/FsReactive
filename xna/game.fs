#light

//namespace FExemples
(*#I @"C:\Program Files\Microsoft XNA\XNA Game Studio\v3.1\References\Windows\x86"

#r "Microsoft.Xna.Framework.dll"
#r "Microsoft.Xna.Framework.Game.dll"
#r @"..\FReactive\obj\Debug\FReactive.dll"
*)
//#r @"..\Graphics\obj\Debug\Graphic.dll"

namespace Xna
 module Game =
  open System
  open FReactive.Misc
  open FReactive.FReactive
  open FReactive.Integration
  open FReactive.Lib


  open Microsoft.Xna.Framework
  open Microsoft.Xna.Framework.Graphics
  open  Microsoft.Xna.Framework.Input;


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

  let couple =  (fun x y -> (x, y))
  let triple =  (fun x y z-> (x, y, z))
  let sidef f = pureB (fun x -> f x
                                x)

  
 //module FExemples = 
  
  let (.*) a b = (pureB (*)) <$> a <$> b 
  let (.-) a b = (pureB (-)) <$> a <$> b 
  let (.+) a b = (pureB (+)) <$> a <$> b 
  let cosB = pureB Math.Cos
  let sinB = pureB Math.Sin
  let (.<=) a b = pureB (<=) <$> a <$> b
  let (.>=) a b = pureB (>=) <$> a <$> b
  

  let mousePos (game:Game) () = 
        let (xm, ym) = let ms = Mouse.GetState() in ((float)ms.X, (float)ms.Y)
        let (xw, yw, ww, hw) = let cb = game.Window.ClientBounds
                               ((float)cb.X, (float)cb.Y, (float)cb.Width, (float)cb.Height)
        let (xmo, ymo) = (2.0*(xm)/ww-1.0, 1.0-2.0*(ym)/hw)
        //printf "%A\n" (xm, ym, xmo, ymo,xw, yw, ww, hw)
        if (xmo < -1.0 || 1.0 < xmo || ymo < -1.0 || 1.0 < ymo) 
        then None
        else Some  (xmo, ymo)
    

  let game (game:Game) = 
            let condxf = pureB (fun x -> x <= (-1.0) || x >= 1.0)
            let condyf = pureB (fun y -> y <= (-1.0) || y >= 1.0)
            let couple =  (fun x y -> (x, y))
            let sidef f = pureB (fun x -> f x
                                          x)

            let mousePos = mousePos game
            let rec mousePosEvt = Evt (fun _ -> (mousePos(), fun() -> mousePosEvt))
            let mousePosB  = stepB (0.0, 0.0) mousePosEvt
            let mousePosXB = pureB fst <$> mousePosB
            let mousePosYB = pureB snd <$> mousePosB
            let rec sys t0 x0 vx0 mousePosXB = 
                let x' = aliasB x0
                let vx' = aliasB vx0
                let accx =  pureB 1.0 .* (mousePosXB .- (fst x')) .- (pureB 0.05 .* (fst vx')) 
                let rec condxE = snapshotE (whenE (condxf <$> (fst x')))
                                           (pureB triple <$> timeB <$> (fst x') <$> (fst vx'))
                                 =>> (fun (_, (t0, x0, vx0)) -> //printf "%A %A\n" x0 vx0 
                                                            sys t0 x0 (-vx0) mousePosXB)
                let vx =  //sidef (fun vx -> printf "v = %A\n" vx) <$>
                                (bindAliasB (integrate accx t0 vx0 ) vx')  
                            
                let x = switchB (bindAliasB (integrate vx t0 x0 ) x') condxE
                x //sidef (fun vx -> printf "x = %A\n" vx) <$> x
            pureB (fun x y -> printf "%A\n" (x, y)
                              (x, y)) <$> (sys 0.0 1.0 0.0 mousePosXB) <$> (sys 0.0 1.0 0.0 mousePosYB)

            // val snapshotE : 'a Event -> 'b Behavior -> ('a * 'b) Event

            
  let game1 (game:Game) = 
            let mousePos = mousePos game
            let rec mousePosEvt = Evt (fun _ -> (mousePos(), fun() -> mousePosEvt))
            let mousePosB  = stepB (0.0, 0.0) mousePosEvt
            let mousePosXB = pureB fst <$> mousePosB
            let mousePosYB = pureB snd <$> mousePosB
            let x' = aliasB 0.0
            let y' = aliasB 0.0
            let vx' = aliasB 0.0
            let vy' = aliasB 0.0
            let accx =  pureB 2.0 .* (mousePosXB .- (fst x')) .- (pureB 0.001 .* (fst vx')) 
            let accy =  pureB 2.0 .* (mousePosYB .- (fst y')) .- (pureB 0.001 .* (fst vy')) 
            let vx = bindAliasB (integrate accx 0.0 0.0) vx'
            let vy = bindAliasB (integrate accy 0.0 0.0) vy'
            let x = bindAliasB (integrate vx 0.0 0.0) x'
            let y = bindAliasB (integrate vy 0.0 0.0) y'
            pureB (fun x y -> printf "%A\n" (x,y) 
                              (x,y)) <$> x <$> y

 module Paddle =

    open Game
    open System
    open FReactive.Misc
    open FReactive.FReactive
    open FReactive.Integration

    open Microsoft.Xna.Framework
    open Microsoft.Xna.Framework.Graphics
    open  Microsoft.Xna.Framework.Input;


    type StateRec = { xball : float; yball : float ; xpaddle : float }
    type State = 
        |State of StateRec
        |End

    

    let game (game:Game) = 
            let ballRadius = 0.005;
            let paddleY = -0.95;
            let paddleHalfLength = 0.1;

            let condxf = pureB (fun x -> x <= (-1.0) || x >= 1.0)
            let condyf = pureB (fun x y xpad -> y >= 1.0 || ((xpad - paddleHalfLength) <= x && x <= (xpad + paddleHalfLength)
                                                             &&  y <= (paddleY+ballRadius)))
            let condyf' = pureB (fun x y xpad -> y >= 1.0 ||  y <= (-1.0))
            let condExitYf = pureB (fun y -> printf "exit = %A\n" y 
                                             y <= (-1.0))
           
            let mousePos = mousePos game
            let rec mousePosEvt = Evt (fun _ -> (mousePos(), fun() -> mousePosEvt))
            let mousePosB  = stepB (0.0, 0.0) mousePosEvt
            let mousePosXB =  pureB  (fun x -> if x - paddleHalfLength < -1.0 
                                               then  (-1.0 )
                                               else if x - paddleHalfLength > 1.0
                                                    then 1.0 
                                                    else x)
                              <$> (pureB fst <$> mousePosB)
            let rec sys t0 xball0 yball0 xpad0 mousePosXB = 
                let xpad = mousePosXB
                let xball' = aliasB xball0
                let yball' = aliasB yball0
                let rec condxE = (whenE (condxf <$> (fst xball'))) --> (fun x -> -x)
                let vballx = stepAccumB (0.3) condxE
                let rec condyE = (whenE (condyf <$> (fst xball') <$> (fst yball') <$> xpad)) --> (fun x -> printf "r\n"
                                                                                                           -x)
                //let rec condyE = (whenE (condyf <$> (fst yball'))) --> (fun x -> printf "r\n" 
                 //                                                                -x)
                let vbally = stepAccumB (-0.45) condyE

                let xball = memoB ( bindAliasB (integrate vballx t0   xball0) xball' )
                let yball = memoB ( bindAliasB (integrate vbally  t0 yball0) yball' )
                let state = (pureB (fun xb yb xp -> State {xball = xb ; yball = yb ; xpaddle = xp}))
                             <$> xball <$> yball <$> xpad
                let condeExitE =  (whenE (condExitYf <$> yball)) =>> (fun _ -> printf "stop\n" 
                                                                               pureB End)
                untilB state condeExitE
                //state
            sys 0.0 0.0 0.0 0.0 mousePosXB

 module DynCol = 
 
 
    open Game
    open System
    open FReactive.Misc
    open FReactive.FReactive
    open FReactive.Integration

    open Microsoft.Xna.Framework
    open Microsoft.Xna.Framework.Graphics
    open Microsoft.Xna.Framework.Input;
    open FReactive.DynCol

    let rec mouseLeftButtonB = Beh (fun _ ->( Mouse.GetState().RightButton.Equals(ButtonState.Pressed), fun () -> mouseLeftButtonB))

    let waitE dt = 
        let tmax = 2.0
        let bf t = let tmax = t+dt
                   let rec bf' t = if (t < tmax) then (None, fun () -> Evt bf')
                                                 else (Some (), fun () -> NoneE)
                   (None, fun () -> Evt bf')
        Evt bf
           
         
// balls with collision

    let colf (x0,y0) (x1,y1) =
        if (x1-x0)*(x1-x0)+(y1-y0)*(y1-y0) < 0.002
        then Some (fun() -> FReactive.Lib.noneB())
        else None

    let dc' = dyncolWithCollisionB


    //let (dc : ColItem option  Behavior list -> ColItem option  Behavior Event -> (Time*Time) list Behavior) =
      //          dyncolWithCollisionB

    let game (game:Game) = 
            let condxf = pureB (fun x -> x <= (-1.0) || x >= 1.0)
            let condyf = pureB (fun y -> y <= (-1.0) || y >= 1.0)
            let clickE = whenE mouseLeftButtonB

            let newBall t0  = 
                    let speedChange x = let nx = -x * (1.0 + random()/2.0)
                                        if (Math.Abs nx) > 30.0 then 30.0 * ((float) (Math.Sign nx));
                                                                else nx
                    let rec x' = aliasB 0.0
                    let rec y' = aliasB 0.0
                    let rec condxE = whenE (condxf <$> (fst x')) --> speedChange
                    let rec condyE = whenE (condyf <$> (fst y')) --> speedChange
                    and speedx = stepAccumB (speedChange 0.2) condxE          
                    and speedy = stepAccumB (speedChange 0.18) condyE          
                    and x = bindAliasB (integrate speedx t0 0.0) x'
                    and y = bindAliasB (integrate speedy t0 0.0) y'
                    let ballB = (pureB Some) <$> ((pureB (fun ball -> ColItem (ball, colf))) <$> (((pureB couple)  <$> x <$> y)))
                    //let ballB' = untilB ballB ((waitE 10.0)  --> (pureB None))
                    ballB
//            dyncolB [] (clickE --> ((pureB newBall) <$> timeB))
            let e = (snapshotE clickE timeB) 
            let e' = e =>> ( fun (_, t0) -> newBall t0)
//            dyncolWithCollisionB [] ((snapshotE clickE timeB) =>> ( fun (_, t0) -> newBall t0))
            let xx = dyncolWithCollisionB [] e'
            xx

// balls
    let game1 (game:Game) = 
            let condxf = pureB (fun x -> x <= (-1.0) || x >= 1.0)
            let condyf = pureB (fun y -> y <= (-1.0) || y >= 1.0)
            let clickE = whenE mouseLeftButtonB

            let newBall t0  = 
                    let speedChange x = let nx = -x * (1.0 + random()/2.0)
                                        if (Math.Abs nx) > 30.0 then 30.0 * ((float) (Math.Sign nx));
                                                                else nx
                    let rec x' = aliasB 0.0
                    let rec y' = aliasB 0.0
                    let rec condxE = whenE (condxf <$> (fst x')) --> speedChange
                    let rec condyE = whenE (condyf <$> (fst y')) --> speedChange
                    and speedx = stepAccumB 0.2 condxE          
                    and speedy = stepAccumB 0.18 condyE          
                    and x = bindAliasB (integrate speedx t0 0.0) x'
                    and y = bindAliasB (integrate speedy t0 0.0) y'
                    let ballB = (pureB Some) <$> ((pureB couple)  <$> x <$> y)
                    let ballB' = untilB ballB ((waitE 10.0)  --> (pureB None))
                    ballB'
//            dyncolB [] (clickE --> ((pureB newBall) <$> timeB))
            dyncolB [] ((snapshotE clickE timeB) =>> ( fun (_, t0) -> [newBall t0]))
(*
            snapshotE (whenE (condxf <$> (fst x')))
                                           (pureB triple <$> timeB <$> (fst x') <$> (fst vx'))
                                 =>> (fun (_, (t0, x0, vx0)) -> //printf "%A %A\n" x0 vx0 
                                                            sys t0 x0 (-vx0) mousePosXB)
*)
(*
let boxB = pureB boxPict 
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
               *)

(*
let boxB = pureB boxPict 
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
#light

//namespace FExemples

// #r @"..\FReactive\obj\Debug\FReactive.dll"

namespace Xna
 module Meteor =

    open FReactive.Misc
    open FReactive.FReactive
    open FReactive.Integration
    open FReactive.DynCol
    open FReactive.Lib
    open System
    
    open Microsoft.Xna.Framework
    open Microsoft.Xna.Framework.Graphics
    open  Microsoft.Xna.Framework.Input;

    


    let rand = new System.Random();
    
    let randAngle() = rand.NextDouble() * 2.0*Math.PI
    
    type  Vector = Vector of (float * float)
        with 
            static member (+) ((Vector (xa,ya)), (Vector (xb, yb))) = Vector ((xa+xb), (ya+yb))
            static member (-) ((Vector (xa,ya)), (Vector (xb, yb))) = Vector ((xa-xb), (ya-yb))
            static member (*) ((Vector (xa,ya)), k) = Vector (k*xa, k*ya)
            static member (/) ((v:Vector), k) = v * (1.0/k)
            static member neg (Vector (xa,ya)) = Vector (-xa, -ya)
            static member length (Vector (xa,ya)) = Math.Sqrt(xa*xa+ya*ya)
            static member norm (v:Vector) = v / (Vector.length v)
            static member rot (Vector (x,y)) angle    = let cosa = Math.Cos angle
                                                        let sina = Math.Sin angle
                                                        Vector ((x * cosa - y * sina), (x * sina + y * cosa))
            static member unit = Vector(1.0, 0.0)
            static member zero = Vector(0.0, 0.0)
            
    
    
    let vectorNumClass = 
        {   plus = curry Vector.(+);
            minus = curry Vector.(-);
            mult = curry Vector.(*);
            div = curry Vector.(/);
            neg = Vector.neg
        }
        
    let inBoxPred (Vector (x,y)) = 
        let proc x = -1.0 <= x && x <= 1.0 
        if  (proc x && proc y) 
        then  true
        else  //printf "out\n"
              false   

    let adaptToBox (Vector (x,y)) = 
        let rec proc x = if x < -1.0 then proc (x+2.0) else if 1.0 < x then proc (x-2.0) else x 
        Vector (proc x, proc y) 
    
    let (.*) a b = (pureB vectorNumClass.mult) <$> a <$> b 
    let (./) a b = (pureB vectorNumClass.div) <$> a <$> b 
    let (.+) a b = (pureB vectorNumClass.plus) <$> a <$> b 
    let (.-) a b = (pureB vectorNumClass.minus) <$> a <$> b 
    
    let optionizeB f = pureB (optionize f)
    

    type Ship = Ship of (Vector * float) // pos, velocity angle
    type Bullet = Bullet of Vector
    type MeteorSize = Big|Medium|Small
    with
        static member size s =  match s with
                                |Big -> 0.1
                                |Medium -> 2.0/30.0
                                |Small -> 2.0/40.0
        static member smaller s =  match s with
                                |Big -> Medium
                                |Medium -> Small
                                |Small -> Small
                                
    type Meteor = Meteor of (Vector * MeteorSize)
    
    type Elem =
        |S of Ship
        |B of Bullet
        |M of Meteor
    with
        static member isMeteor e = match e with
                                   |M _ -> true
                                   |_ -> false     
        static member isBullet e = match e with
                                   |B _ -> true
                                   |_ -> false     
        static member isShip e =   match e with
                                   |S _ -> true
                                   |_ -> false 
        static member getMeteor e = match e with
                                    |M v -> v
        static member getBullet e = match e with
                                    |B v -> v
        static member getShip e =   match e with
                                    |S v -> v


    type Message = 
    |DestroyMeteor of Elem IdentifiedObject * Elem IdentifiedObject // meteor  x bullet 
        
    // angle governed by keyboard    
    let mkVelocityAngleB rightTurnB leftTurnB t0 (angle:float) increment = 
        let proc b = if b then increment else 0.0
        let angleVelocity = (((pureB proc) <$> leftTurnB) .-. ((pureB proc) <$> rightTurnB))
        memoB <| (integrate angleVelocity t0 angle)
        

     // create a meteor
     // check for collision with bullets   
    let mkMeteor t0 x0 v0 msize  = 
        let integrate = integrateGenB vectorNumClass
        let x = memoB ((pureB (fun v -> Some (Meteor (v, msize)))) <$> integrate v0 t0 x0)
        x
        
     // check for collision of a meteor with a bullet
    let colMeteorBullet = 
        let colPred (Meteor (xm, msize)) (Bullet xb) = 
            Vector.length (xm - xb) < (MeteorSize.size msize *1.0)
            //true
        let detect bl xm  =
            match xm with
            |Some xm -> List.exists (colPred xm) (List.map Elem.getBullet (List.filter Elem.isBullet bl))
            |None -> false
        (pureB detect)

    let detectDestroyedMeteor  = 
        let colPred (Meteor (xm, msize)) (Bullet xb) = 
            Vector.length (xm - xb) < (MeteorSize.size msize *10.3)
            true
        let detect (elm: Elem IdentifiedObject list) = 
            let ml =  List.filter (IdentifiedObject<Elem>.item >> Elem.isMeteor) elm
            let bl =  List.filter (IdentifiedObject<Elem>.item >> Elem.isBullet) elm
            let rec proc lml b rml = 
                match rml with
                |[] -> None
                |h::t -> if colPred (IdentifiedObject<Elem>.item >> Elem.getMeteor <| h) (IdentifiedObject<Elem>.item >> Elem.getBullet <| b) 
                         then Some ((lml @ t), DestroyMeteor (h, b))
                         else proc (lml @ [h]) b t
            let proc' state b = 
                let (hits, ml) = state
                match proc [] b ml with
                |Some (nml, hit) -> (hit::hits, nml) 
                |None -> state             
            List.fold proc' ([], ml) bl |> fst 
        pureB detect
                       
    let rec mkBreakableMeteor t0 x0 meteorVelocity msize elemListB = 
        let meteorB = mkMeteor t0 x0 (pureB meteorVelocity) msize 
        let hitPred = (colMeteorBullet <$> elemListB)
        let hitE = whenE (hitPred <$> meteorB)
        let hitMeteorB = untilB meteorB (hitE --> (noneB()))
        let meteorB' =  (pureB (fun meteor  -> DynColElem (meteor, []))) <$> ((optionizeB M) <$> hitMeteorB)  
        meteorB'
        
    // create a bullet
    // limit its time of life
    // check for collision with meteors   
    let mkBullet2 maxLifeTime t0 x0 v0 colPred = 
        let integrate = integrateGenB vectorNumClass
        let x = (pureB Some) <$> integrate (pureB v0) t0 x0
        let ageE = (whenE (timeB .-. (pureB t0) .>. (pureB maxLifeTime)) --> (noneB()))
        let x' = memoB (untilB x ageE)
        (optionizeB Bullet) <$> untilB x' (whenE (colPred <$> x') --> (noneB()))

    let mkBullet3 maxLifeTime t0 x0 v0 colPred = 
        let integrate = integrateGenB vectorNumClass
        let rec bg t0 x0 e0 = 
            let x0' = e0 t0 x0
            let x = integrate (pureB v0) t0 x0'
            let boxE = (whileE ((pureB (inBoxPred >> not)) <$> x)) --> (fun _ x -> adaptToBox x)
            Disc (x, boxE, bg)
        let x = discontinuityE (bg t0 x0 (fun _ x -> adaptToBox x))
        let ageE = (whenE (timeB .-. (pureB t0) .>. (pureB maxLifeTime)) --> (noneB()))
        let x' = memoB (untilB ((pureB Some) <$> x) ageE)
        (optionizeB Bullet) <$> untilB x' (whenE (colPred <$> x') --> (noneB()))
 
 
     // check for collision of a bullet with a meteor
    let colBulletMeteor = 
        let colPred xb (Meteor (xm, msize)) = 
            Vector.length (xm - xb) < MeteorSize.size msize *10.3
            true
        let detect ml xb =
            match xb with
            |Some (Bullet xb) -> match List.tryFind (colPred xb) (List.map Elem.getMeteor (List.filter Elem.isMeteor ml)) with
                                |Some meteor -> //printf "bullet hist %A\n" xb
                                                Some meteor
                                |None -> None
            |None -> None
        (pureB detect)


(*    let rec switchRestartB2 b e =
        let rec bf b e t =  let (r,nb) = atB b t
                            match atE e t with
                            |(None, ne) -> (r, fun() -> switchRestartB2 (nb()) (ne()))
                            |(Some newB, ne) -> (r, fun () -> Beh (bf2 (nb()) newB (ne())))
                                      
        and bf2 pb newb e t = let (r,nb) = atB newb t
                              (r, fun () -> switchRestartB2 pb e)          
        Beh (bf b e)
        *)
        
    let rec switchRestartB2 b e =
     let rec bf b e t = let (r,nb) = atB b t
                        let (_,ne) = atE e t
                        (r, fun() -> Beh (bf2 (nb()) (ne())))       
     and bf2 b e t = let (r,nb) = atB b t
                     let proc() = match atE e t with
                                  |(None, ne) -> Beh (bf2 (nb()) (ne()) )
                                  |(Some newB, ne) -> Beh (bf3 (nb()) newB (ne()))
                     (r,  proc)
     and bf3 pb newb e t = let (r,nb) = atB newb t
                           let (_,ne) = atE e t
                           (r,  fun() -> switchRestartB2 pb (ne()))           
     Beh (bf b e)
     
 (*
    let rec switchRestartB2 b e =
        let rec bf b e t = let (r,nb) = atB b t
                           let proc   = match atE e t with
                                        |(None, ne) -> switchRestartB2 (nb()) (ne())
                                        |(Some newB, ne) -> Beh (bf2 (nb()) newB (ne()))
                           (r, fun () -> proc)           
        and bf2 pb newb e t = let (r,nb) = atB newb t
//                              let (_,ne) = atE e t
                              (r, fun () -> Beh (bf pb (e)))           
        Beh (bf b e)
   *)


    let mkBullet maxLifeTime t0 x0 v0 elemListB meteorVelocity = 
        let integrate = integrateGenB vectorNumClass
        let rec bg t0 x0 e0 = 
            let x0' = e0 t0 x0
            let x = integrate (pureB v0) t0 x0'
            let boxE = (whileE ((pureB (inBoxPred >> not)) <$> x)) --> (fun _ x -> adaptToBox x)
            Disc (x, boxE, bg)
        let x = discontinuityE (bg t0 x0 (fun _ x -> adaptToBox x))
        let ageE = (whenE (timeB .-. (pureB t0) .>. (pureB maxLifeTime)) --> (noneB()))
        let x' = memoB (untilB ((pureB Some) <$> x) ageE)
        let proc ((Meteor (x, msize)) , t0) = 
                                         if msize = MeteorSize.Small 
                                         then pureB []
                                         else   let msize' = MeteorSize.smaller msize
                                                let v1 = Vector.rot meteorVelocity (randAngle()) 
                                                let v2 = Vector.rot meteorVelocity (randAngle()) 
                                                let r1() = (mkBreakableMeteor t0 x v1 msize' elemListB)
                                                let r2() = (mkBreakableMeteor t0 x v2 msize' elemListB)
                                                (pureB [r1 ; r2])
        let x'' = memoB ((optionizeB Bullet) <$> x')
//        let hitE =  memoE (whenAnyE (tronB "hit 1" (colBulletMeteor <$> elemListB <$> x'')))
        let hitE =  memoE (whenAnyE ((colBulletMeteor <$> elemListB <$> x'')))
        let x''' =  (memoB (untilB x'' (hitE --> (noneB()))) )
//        let hitE' =  memoE (whenAnyE (tronB "hit 21" (colBulletMeteor <$> elemListB <$> x''')))
        let hitE' =  memoE (whenAnyE ( (colBulletMeteor <$> elemListB <$> x''')))

        let newMeteorG = snapshotE hitE' timeB =>> proc
//        let bulletB =  (pureB (fun bullet meteorG -> DynColElem (bullet, meteorG))) <$> ((optionizeB B) <$> x''') <$> (switchRestartB2 (tronB "empty" (pureB [])) newMeteorG)  
        let bulletB =  (pureB (fun bullet meteorG -> DynColElem (bullet, meteorG))) <$> ((optionizeB B) <$> x''') <$> (untilB ( (pureB [])) newMeteorG)  
        tronB "bullet" bulletB
        
    // ship movement
    let mkShipDynamics2 frictionB angleB thrustB t0 x0 v0 =
        let (.*) (a:Behavior<Vector>) b = (pureB (*)) <$> a <$> b 
        let (.-) (a:Behavior<Vector>) b = (pureB (-)) <$> a <$> b 
        
        let integrate = integrateGenB vectorNumClass
        let x' = aliasB x0
        let v' = aliasB v0
        let acc =  ((pureB Vector.rot) <$> thrustB <$> angleB) .-  ((fst v') .* frictionB) 
        let v = bindAliasB (integrate acc t0 v0) v'
        let x = bindAliasB (integrate v t0 x0) x'
        (pureB (fun x angle -> Some (Ship (x, angle)))) <$> x <$> angleB


    let mkShipDynamics frictionB angleB thrustB t0 x0 v0 =
        let (.*) (a:Behavior<Vector>) b = (pureB (*)) <$> a <$> b 
        let (.-) (a:Behavior<Vector>) b = (pureB (-)) <$> a <$> b 
        
        let integrate = integrateGenB vectorNumClass
        
        let rec bg t0 (x0, v0) e0 = 
            let (x0', v0') = e0 t0 (x0, v0)
            let x' = aliasB x0'
            let v' = aliasB v0'
            let acc =  ((pureB Vector.rot) <$> thrustB <$> angleB) .-  ((fst v') .* frictionB) 
            let v = bindAliasB (integrate acc t0 v0') v'
            let x = bindAliasB (integrate v t0 x0') x'
            let xv = coupleB() <$> x <$> v
            let boxE = (whileE ((pureB (fst >> inBoxPred >> not)) <$> xv)) --> (fun _ (x,v) -> (adaptToBox x), v )
            Disc (xv, boxE, bg)

        let x = discontinuityE (bg t0 (x0, v0) (fun _ (x,v) -> (adaptToBox x), v ))
        let x' = (pureB fst) <$> x
        memoB <| ((pureB (fun x angle -> Some (Ship (x, angle)))) <$> x' <$> angleB)

        

        
    // firing ship
    // firingCommandB : bool event : true when firing button is pressed
    let mkFiringShip shipB firingCommandB bulletVelocity bulletMaxLifeTime elemListB meteorVelocity =
        let fireE = whenE firingCommandB 
//        let colF = colBulletMeteor <$> elemListB
        let proc (ship, t0) = 
             match ship with
            |Some (Ship (x, angle)) -> let v0 = Vector.rot (Vector.unit) angle * bulletVelocity
                                       let x' = x +  (Vector.rot (Vector.unit) angle) * 0.1
                                       //printf "angle buller %A\n" angle
                                       let r = ((mkBullet bulletMaxLifeTime t0 x' v0 elemListB meteorVelocity))
                                       pureB [fun () -> r]
            |None -> pureB  []
        let newBulletG = snapshotBehaviorOnlyE fireE (coupleB() <$> (shipB) <$> timeB) =>> proc
        (pureB (fun ship bulletG -> DynColElem (ship, bulletG))) <$> ((optionizeB S) <$> (shipB)) <$> (switchRestartB2 (pureB []) newBulletG)  
        
        
      
      // type 'a DynColElem = DynColElem of ('a option * (unit->'a DynColElem Behavior) list)
        
        // Elem DynColElem behavior
            
    let rec mouseLeftButtonB = Beh (fun _ ->( Mouse.GetState().LeftButton.Equals(ButtonState.Pressed), fun () -> mouseLeftButtonB))
    let rec mouseRightButtonB = Beh (fun _ -> ( Mouse.GetState().RightButton.Equals(ButtonState.Pressed), fun () -> mouseRightButtonB))
    let rec mouseMiddleButtonB = Beh (fun _ -> ( Mouse.GetState().MiddleButton.Equals(ButtonState.Pressed), fun () -> mouseMiddleButtonB))
    let rec keyboardInputG key = Beh (fun _ -> ( Keyboard.GetState().IsKeyDown(key), fun () -> keyboardInputG key))
    let rec fireCommandB = keyboardInputG Keys.Space
    let rec thrustCommandB = keyboardInputG Keys.A

    let game (game:Game) = 
        let randX() = 2.0 * rand.NextDouble() - 1.0
        let randVector() = Vector(randX(), randX())
        let angleB =  mkVelocityAngleB mouseRightButtonB mouseLeftButtonB 0.0 1.0 2.0
        let thrustB = (pureB (fun b -> if b then (Vector (1.0, 0.0)) else (Vector (0.0, 0.0)))) <$> thrustCommandB
        let ship = mkShipDynamics (pureB 0.1) angleB thrustB 0.0 Vector.zero Vector.zero
        let ship' = (optionizeB S) <$> ship
        let ship'' = let i = createId()
                     (optionizeB (fun item -> IdItem (i, item))) <$> ship'
        //let rB = (pureB (fun ship  -> DynColElem (ship, []))) <$> (ship'')
        (pureB (fun x-> catOption [x])) <$> ship''
        


(*  
    let game (game:Game) = 
        let randX() = 2.0 * rand.NextDouble() - 1.0
        let randVector() = Vector(randX(), randX())
        let angleB =  mkVelocityAngleB mouseRightButtonB mouseLeftButtonB 0.0 1.0 2.0
        let thrustB = (pureB (fun b -> if b then (Vector (1.0, 0.0)) else (Vector (0.0, 0.0)))) <$> thrustCommandB
        let ship = mkShipDynamics (pureB 0.1) angleB thrustB 0.0 Vector.zero Vector.zero
        let col' = aliasB ([])
        let meteorVelocity = 0.02
        let firingShip = mkFiringShip ship fireCommandB 1.0 2.0 (fst col') ((Vector.rot Vector.unit (randAngle())) * meteorVelocity)
        let meteors = List.map (fun _ -> mkBreakableMeteor 0.0 (randVector()) ((Vector.rot Vector.unit (randAngle())) * meteorVelocity) MeteorSize.Big (fst col'))
                                [1] //;2;3;4]
        let initElems = firingShip :: meteors
        let col = bindAliasB (dyncolB2 initElems NoneE) col'
        seqB angleB col


  // 'a DynColElem Behavior list -> 'a DynColElem Behavior list Event -> 'a list Behavior
  // mkFiringShip shipB firingCommandE bulletVelocity bulletMaxLifeTime elemListB


 let mkVelocityAngleB rightTurnE leftTurnE (angle:float) increment = 
        stepAccumB angle ((rightTurnE --> (fun a -> a - increment)) .|. (leftTurnE --> (fun a -> a + increment)))
        

    // ship movement
    let mkShipDynamics frictionB angleB thrustB t0 x0 v0 =

    let saw slope max t0 =
        let rec bf t0 i0 t = 
            let i = i0 + slope * (t-t0)
            if i>=max
            then (i, fun() -> Beh (bf t 0.0))
            else (i, fun() -> Beh (bf t i))
        Beh (bf t0 0.0)
        
        
 // 'a option Behavior list -> 'a option Behavior Event -> 'a list Behavior
 
    let col () = 
        let colB' = aliasB []
        let makeItem value =
            let ieE = whenE (((pureB List.length) <$> (fst colB')) .=. (pureB 3)) --> (noneB()) 
            let iB' = ((pureB Some) <$> oneB)  
            let iB = untilB ((pureB Some) <$> (pureB value)) ieE 
            iB
        let genE = whenE ((saw 1.0 5.0 0.0) .>=. (pureB 5.0)) --> [makeItem 1.0]
        let genE' = whenE (((pureB List.length) <$> (fst colB')) .=. (pureB 3)) --> [makeItem 2.0; makeItem 3.0]
        bindAliasB (dyncolB [] (genE .|. genE')) colB'
        
    
    let ls = Seq.toList (seq{for i in 1..50 -> (float)i})

    //let b = saw 1.0 5.0 0.0
    let b = col()

    runList b ls
    
           let x' = aliasB 0.0
            let y' = aliasB 0.0
            let vx' = aliasB 0.0
            let vy' = aliasB 0.0
            let accx =  pureB 2.0 .* (mousePosXB .- (fst x')) .- (pureB 0.001 .* (fst vx')) 
            let accy =  pureB 2.0 .* (mousePosYB .- (fst y')) .- (pureB 0.001 .* (fst vy')) 
            
                
    
    let (.* )< (a:Behavior<_>) b = (pureB ( * )) <$> a <$> b 
    let (.-) (a:Behavior<_>) b = (pureB (-)) <$> a <$> b 
    let (.+) a b = (pureB (+)) <$> a <$> b 
    let cosB = pureB Math.Cos
    let sinB = pureB Math.Sin
    let (.<=) a b = pureB (<=) <$> a <$> b
    let (.>=) a b = pureB (>=) <$> a <$> b
  





    let v = Vector(5.0, 8.0)
    
    Vector.(+) v v
    
    v+v
    *)
    
    (*
    
    let integrate b t0 i0 = 
        let rec bf b t0 i0 t = let (r, nb) = atB b t
                               let i = i0 + r * (t-t0)
                               (i, fun() -> Beh (bf (nb()) t i))
        Beh (bf b t0 i0 )   

    let f1() = 
        let v = pureB 1.0
        let x = integrate v 0.0 0.0
        let pred x = x >= 5.0
        let evt = snapshotBehaviorOnlyE (whenE ((pureB pred) <$> timeB)) ((coupleB() <$> timeB <$> x))  =>>  
                    (fun (t, x) -> integrate v t 0.0)
        switchB x  evt



    let rec f t  = 
        let x' = aliasB 0.0
        //let x =  memoB (bindAliasB (integrate (fst x') 0.0 1.0) x')
        let x =   (bindAliasB (integrate (fst x') 0.0 1.0) x')
        let pred x = x >= 5.0
        let evt = snapshotBehaviorOnlyE (whileE ((pureB pred) <$> x)) ((coupleB() <$> timeB <$> x))  =>>  
                    (fun (t, x) -> f t)
        untilB x  evt

     let rec f t  = 
        let x' = aliasB 0.0
        //let x =  memoB (bindAliasB (integrate (fst x') 0.0 0.1) x')
        let x =   (bindAliasB (integrate (fst x') 0.0 1.0) x')
        (coupleB()) <$> x <$> x


    type Discontinuity<'a, 'b> = Disc of ('a Behavior *  (Time -> 'a -> 'b) Event * (Time -> 'a -> (Time -> 'a -> 'b) ->  Discontinuity<'a, 'b>))
    
    let rec discontinuityE (Disc (xB, predE, bg))  = 
        let evt = snapshotE predE ((coupleB() <$> timeB <$> xB))  =>>  
                        (fun (e,(t,vb)) -> let disc = bg t vb e
                                           discontinuityE disc)
        untilB xB evt

    let rec f t  = 
        let rec bg t0 x0 e = 
            let x' = aliasB (e t0 x0)
            let x =   (bindAliasB (integrate (fst x') t0 (e t0 x0)) x')
            let pred x = x >= 5.0
            let predB = whileE ((pureB pred) <$> x) --> (fun _ _ -> 1.0)
            Disc (x, predB, bg)
        discontinuityE (bg t 0.0 (fun _ _ -> 1.0))

                 
    let ls = Seq.toList (seq{for i in 1..10 -> (float)i})
    
    //let b = saw 1.0 5.0 0.0
    let b = f 0.0

    runList b ls
    

    
 let rec switchB b e =
   let bf t = let (r,nb) = atB b t
              let proc() = match atE e t with
                           |(None, ne) -> switchB (nb()) (ne())
                           |(Some newB, ne) -> switchB newB (ne())
              (r, proc)           
   Beh bf

   
 let rec switch2B b e =
   let bf t = let (r,nb) = atB b t
              let proc() = match atE (e()) t with
                           |(None, ne) -> switch2B (nb()) (ne())
                           |(Some newB, ne) -> switch2B newB ne
              (r, proc)           
   Beh bf

 let rec switchRestartB b e =
   let rec bf b e t = let (r,nb) = atB b t
                      let proc() = match atE e t with
                                   |(None, ne) -> switchRestartB (nb()) (ne())
                                   |(Some newB, ne) -> Beh (bf2 (nb()) newB (ne()))
                      (r, proc)           
   and bf2 pb newb e t = let (r,nb) = atB newb t
                         (r, fun () -> Beh (bf pb e))           
   Beh (bf b e)

    let f _ =  switchRestartB (pureB 1.0) ((someE ()) --> (pureB 2.0))

    let ls = Seq.toList (seq{for i in 1..10 -> (float)i})
    
    //let b = saw 1.0 5.0 0.0
    let b = f 0.0

    runList b ls
    *)
    

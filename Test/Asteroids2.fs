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
    
    let randX() = 2.0 * rand.NextDouble() - 1.0

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
            
    let randVector() = Vector(randX(), randX())

    
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
    let someizeB b = (pureB Some) <$> b
    
    type Jet = JetSmall|JetMedium|JetBig
    type Ship = Ship of (Vector * float * Jet option) IdentifiedObject // pos, velocity angle
    type DestroyedShip = DestroyedShip of (Vector * float * float)  // pos, velocity angle
    
    type Bullet = Bullet of Vector IdentifiedObject
    
    type MeteorSize = Big|Medium|Small
    with
        static member size s =  match s with
                                |Big -> 0.1
                                |Medium -> 2.0/30.0
                                |Small -> 2.0/40.0
        static member speedFactor s =   match s with
                                        |Big -> 1.0
                                        |Medium -> 1.5
                                        |Small -> 2.0
        static member smaller s =   match s with
                                    |Big -> Medium
                                    |Medium -> Small
                                    |Small -> Small
        static member score s =      match s with
                                    |Big -> 100
                                    |Medium -> 200
                                    |Small -> 400
                                
    type Meteor = Meteor of (Vector * MeteorSize) IdentifiedObject
    
    type GameState = {nbShips: int; ship:Ship option ; meteors:Meteor list; bullets:Bullet list; 
                      destroyedShip : DestroyedShip option;
                      shieldOn : bool;
                      score:int}
   

    type Message = 
        {
            destroyMeteor: (Meteor * Bullet) list;
            destroyShip: Meteor option
        } 
        
    let rec periodicB period = 
            let E2 = (someE ()) =>> (fun () -> periodicB period)
            let E1 = (waitE period) --> ( untilB (pureB true) E2)
            untilB (pureB false) E1


    let rec mouseLeftButtonB = Beh (fun _ ->( Mouse.GetState().LeftButton.Equals(ButtonState.Pressed), fun () -> mouseLeftButtonB))
    let rec mouseRightButtonB = Beh (fun _ -> ( Mouse.GetState().RightButton.Equals(ButtonState.Pressed), fun () -> mouseRightButtonB))
    let rec mouseMiddleButtonB = Beh (fun _ -> ( Mouse.GetState().MiddleButton.Equals(ButtonState.Pressed), fun () -> mouseMiddleButtonB))
    let rec keyboardInputG key = Beh (fun _ -> ( Keyboard.GetState().IsKeyDown(key), fun () -> keyboardInputG key))
    let rec fireCommandB = keyboardInputG Keys.Enter .||. ( keyboardInputG Keys.O) .||. (keyboardInputG Keys.Z)
    let rec thrustCommandB = keyboardInputG Keys.Space .||. (keyboardInputG Keys.Up)
    let rec leftCommandB = keyboardInputG Keys.Left .||. keyboardInputG Keys.A
    let rec rightCommandB = keyboardInputG Keys.Right .||. keyboardInputG Keys.P
    let rec shieldCommandB = keyboardInputG Keys.Z

    let rec jetB = 
        let e = (loopE (whenE (periodicB 0.025)) [JetSmall; JetMedium; JetBig]) =>> snd
        let b = stepB JetSmall e
        (pureB (fun thrust jet -> if thrust then Some jet else None)) <$> thrustCommandB <$> b

    // angle governed by keyboard    
    let mkVelocityAngleB rightTurnB leftTurnB t0 (angle:float) increment = 
        let proc b = if b then increment else 0.0
        let angleVelocity = (((pureB proc) <$> leftTurnB) .-. ((pureB proc) <$> rightTurnB))
        memoB <| (integrate angleVelocity t0 angle)
        

    let shieldB = 
        let shieldCommandE =  (whenE shieldCommandB )
        let rec proc() = printf "shield\n"  
                         memoB (untilB falseB  (shieldCommandE =>> (fun () -> untilB trueB (waitE 1.0 =>> (fun() -> untilB falseB (waitE 2.0 =>> proc))))))
        proc()

     // create a meteor
    let mkMeteor t0 x0 v0 msize shipB  = 
        let integrate = integrateGenB vectorNumClass
        let rec bg t0 (x0, v0) e0 = 
            let (x0', v0') = e0 t0 (x0, v0)
            let x' = aliasB x0
            let shipContact ship x shield = 
                match ship with
                |Some (Ship (IdItem (_, (xs, _, _)))) -> shield && Vector.length (x - xs) < 0.2 
                |None -> false
            let rec condContactE = (whenE ((pureB shipContact) <$> shipB <$> (fst x') <$> shieldB)) --> (fun x -> Vector.neg x)
            let vmeteor = stepAccumB v0' condContactE
            let x = bindAliasB (integrate vmeteor t0 x0') x'
            let boxE = (whileE ((pureB (inBoxPred >> not)) <$> x)) --> (fun _ (x, v) -> ((adaptToBox x), v))
            Disc ((coupleB() <$> x <$> vmeteor), boxE, bg)
        let x = discontinuityE (bg t0 (x0, v0) (fun _ (x, v) -> ((adaptToBox x), v)))
        let x' = memoB <| (pureB Meteor) <$> (mkIdObjB ((pureB (fun x -> (x, msize))) <$> ((pureB fst) <$> x)))
        x'
        
        
    let rec mkBreakableMeteor t0 x0 meteorVelocity msize messageB shipB = //elemListB = 
        let meteorB = mkMeteor t0 x0 ( meteorVelocity) msize shipB
        let hit (Meteor (IdItem (id, _))) {destroyMeteor = destroyMeteor} = 
            List.exists (fun ((Meteor (IdItem (idb, _))), _) -> id=idb) destroyMeteor
        let hitE = whenE ((pureB hit) <$> meteorB <$> messageB) --> (noneB())

        let meteorB' =  (pureB (fun meteor  -> DynColElem (meteor, []))) <$> (untilB (someizeB meteorB) hitE)  
        meteorB'

        
    let detectDestroyedMeteor = 
        let colPred (Meteor (IdItem (_, (xm, msize)))) (Bullet (IdItem (_, xb))) = 
            Vector.length (xm - xb) < (MeteorSize.size msize *1.0)
            //true
        let detect meteors bullets = 
            let rec proc lml b rml = 
                match rml with
                |[] -> None
                |h::t -> if colPred h b
                         then Some ((lml @ t), (h, b))
                         else proc (lml @ [h]) b t
            let proc' state b = 
                let (hits, ml) = state
                match proc [] b ml with
                |Some (nml, hit) -> (hit::hits, nml) 
                |None -> state             
            List.fold proc' ([], meteors) bullets |> fst 
        detect
        
    let detectDestroyedShip ship meteors = 
        let colPred (Ship (IdItem (_, (xs, _, _)))) (Meteor (IdItem (_, (xm, msize))))  = 
            Vector.length (xm - xs) < (MeteorSize.size msize *1.0+3.0/40.0)
            //true
        match ship with
        |Some ship -> List.tryFind (colPred ship) meteors
        |None -> None
  

        
    let mkBullet maxLifeTime t0 x0 v0 messageB  = //elemListB = 
        let integrate = integrateGenB vectorNumClass
        let rec bg t0 x0 e0 = 
            let x0' = e0 t0 x0
            let x = integrate (pureB v0) t0 x0'
            let boxE = (whileE ((pureB (inBoxPred >> not)) <$> x)) --> (fun _ x -> adaptToBox x)
            Disc (x, boxE, bg)
        let x = discontinuityE (bg t0 x0 (fun _ x -> adaptToBox x))
        let x' = (pureB Bullet) <$>  (mkIdObjB  x)
        let ageE = (whenE (timeB .-. (pureB t0) .>. (pureB maxLifeTime)) --> (noneB()))
        let hit (Bullet (IdItem (id, _))) {destroyMeteor = destroyMeteor} = 
            List.exists (fun (_,  (Bullet (IdItem (idb, _)))) -> id=idb) destroyMeteor
        let hitE = whenE ((pureB hit) <$> x' <$> messageB) --> (noneB())
        let bulletB = memoB (untilB (someizeB x') (ageE .|. hitE))
        bulletB
        


    let rec mkShipDynamics frictionB angleB thrustB t0 x0 v0 messageB stateB =
        let (.* ) (a:Behavior<Vector>) b = (pureB (*)) <$> a <$> b 
        let (.- ) (a:Behavior<Vector>) b = (pureB (-)) <$> a <$> b 
        
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
        
        let shipToCreateE = whenE ((pureB (fun m -> m.nbShips > 0 && not (isSome m.ship))) <$> stateB)
        let shipToCreateE' = snapshotBehaviorOnlyE shipToCreateE  timeB =>> 
                                (fun t0-> let b = mkShipDynamics frictionB angleB thrustB t0 Vector.zero Vector.zero messageB stateB
                                          untilB (noneB()) (waitE 2.0 --> b ))
        let destroyedShipE = whenE ((pureB (fun m -> isSome (m.destroyShip))) <$> messageB) --> (untilB (noneB()) shipToCreateE')
        let shipB = memoB <| (untilB (someizeB   ((pureB Ship) <$> (mkIdObjB ((pureB (fun x angle jet -> (x, angle, jet))) <$> x' <$> angleB <$> jetB)))) destroyedShipE)
        shipB
        
       
    // fire bullet event
    // firingCommandB : bool event : true when firing button is pressed
    let mkFire shipB firingCommandB bulletVelocity bulletMaxLifeTime messageB =
        let fireE = whenE firingCommandB 
//        let fireE = whenE (periodicB 0.1) 
//        let colF = colBulletMeteor <$> elemListB
        let proc (ship, t0) = 
             match ship with
             |Some (Ship (IdItem (_, (x, angle, _)))) -> 
                                        let v0 = Vector.rot (Vector.unit) angle * bulletVelocity
                                        let x' = x +  (Vector.rot (Vector.unit) angle) * 0.1
                                        let r = ((mkBullet bulletMaxLifeTime t0 x' v0 messageB))
                                        [r]
             |None -> []
        let newBulletE = snapshotBehaviorOnlyE fireE (coupleB() <$> (shipB) <$> timeB) =>> proc
        newBulletE
        
//        (Meteor (IdItem (_, x)))
//     let rec mkBreakableMeteor t0 x0 meteorVelocity msize messageB = //elemListB = 


    //let ifB cb tb fb = 
    
   // let noMoreMeteorsE meteorsB = 
   //     let emptyB = (pureB (List.isEmpty)) <$> meteorsB  
   //    let e = (whenE emptyB) --> (waitE 0.5)   


    let rec splitMeteor meteorVelocity messageB meteorsB shipB thereAreMeteorsE gameLevelB = 
      let rec splitMeteorProc  shipB =  
            let proc t0 (Meteor (IdItem( _,  (x, msize)))) = 
                                             if msize = MeteorSize.Small 
                                             then   []
                                             else   let msize' = MeteorSize.smaller msize
                                                    let meteorVelocity'= meteorVelocity * MeteorSize.speedFactor msize'
                                                    let v1 = Vector.rot Vector.unit (randAngle()) * meteorVelocity'
                                                    let v2 = Vector.rot Vector.unit (randAngle()) * meteorVelocity'
                                                    let r1 = (mkBreakableMeteor t0 x v1 msize' messageB shipB)
                                                    let r2 = (mkBreakableMeteor t0 x v2 msize' messageB shipB)
                                                    ([r1 ; r2])
            let meteors {destroyMeteor = destroyMeteor} t0 = 
                    List.fold (fun state (m,_) -> proc t0 m @ state) [] destroyMeteor
            let msplitB =  (((pureB meteors) <$> messageB <$> timeB))
            let newSetOfMeteors nms ms nbMeteors = 
                    if List.isEmpty nms
                    then if List.isEmpty ms
                         then let meteors = List.map (fun _ -> startB (fun t0 -> mkBreakableMeteor t0 (randVector()) ((Vector.rot Vector.unit (randAngle())) * meteorVelocity) MeteorSize.Big messageB shipB))
                                                     (Seq.toList ( seq {for i in 1 .. nbMeteors -> ()}))
                              meteors
                         else []
                    else []

            let mnewB = (pureB newSetOfMeteors) <$> msplitB <$> meteorsB <$> gameLevelB
            let mnewB' = untilB (pureB []) (waitE 2.0 --> mnewB)
//        let mE = whenE ((pureB (fun l -> not (List.isEmpty l))) <$> mnewB)
            let mE = whenE ((pureB (fun l ->  (List.isEmpty l))) <$> meteorsB)
            let nMeteorsB = untilB msplitB 
                                   (mE --> mnewB')
            
            let nMeteorsB' = untilB nMeteorsB 
                                    (thereAreMeteorsE =>> fun () -> splitMeteorProc  shipB)
        //let nMeteorsB' = nMeteorsB
            nMeteorsB'
      splitMeteorProc  shipB
       



        // detectDestroyedMeteor
    let controller meteorsB bulletsB shipB  =   
        let destroyMeteorCmdB = (pureB detectDestroyedMeteor) <$> meteorsB <$> bulletsB 
        let destroyShipCmdB = (pureB detectDestroyedShip) <$> shipB <$> meteorsB 
        (pureB (fun destroyMeteorCmd destroyShipCmd -> 
                    if isSome destroyShipCmd
                    then //printf "ppppppppppppppppppppp\n"
                         ()
                    else ()
                    {   destroyMeteor = destroyMeteorCmd; destroyShip = destroyShipCmd })) 
            <$> destroyMeteorCmdB
            <$> destroyShipCmdB
      
      // type 'a DynColElem = DynColElem of ('a option * (unit->'a DynColElem Behavior) list)
    
        // Elem DynColElem behavior
            
    let scoreB messageB = 
        let proc acc ((Meteor (IdItem (_, (_, size)))),_) = acc + (MeteorSize.score size)
        let incScoreB = (pureB (fun {destroyMeteor=destroyMeteor} -> 
                                        let r = List.fold proc 0 destroyMeteor
                                        match r with
                                        |0 -> None
                                        |inc -> Some (fun x -> x + inc))) <$> messageB
        let scoreB = stepAccumB 0 (whenAnyE incScoreB)
        scoreB

    let addShipB stateB  = 
        let minScoreB' = aliasB 2000
        let eE = whenE ((pureB (fun state minScore -> state.score >= minScore)) <$> stateB <$> (fst minScoreB'))
        let minScoreB = bindAliasB (stepAccumB 2000 (eE --> (fun x -> x+2000))) minScoreB'
        (pureB (fun s -> s/2000-1)) <$> minScoreB


 //      type DestroyedShip = DestroyedShip of (Vector * float * float)  // pos, velocity angle
    let scaleB() =  stepB 0.1 (iterE (whenE (periodicB 0.01)) [ 0.2; 0.3; 0.4; 0.7; 0.9; 1.1; 1.4; 1.7; 2.0; 0.0] =>> snd)

    

    let rec destroyedShipB messageB shipB = 
        let proc shipOption =
            //printf "calling proc\n"
            match shipOption with
            |Some (Ship (IdItem (_, (x, angle, _)))) -> 
                                let scaleB' = startB (fun _ -> scaleB())
                                let nsB = pureB (fun scale -> Some (DestroyedShip (x, angle, scale))) <$> scaleB'
                                untilB nsB (whenE ((pureB (fun scale -> scale = 0.0)) <$> scaleB') =>> (fun _ -> destroyedShipB messageB shipB))
            |None -> noneB()

        let eE = (snapshotBehaviorOnlyE (whenE ((pureB (fun m -> let r = isSome m.destroyShip
                                                                 if r 
                                                                 then //printf "true\n"
                                                                      true
                                                                 else false)) <$> messageB)) shipB) =>> proc
        untilB (noneB()) (eE)


    let game (game:Game) = 
        let standbyGame = {nbShips = 3; ship = None; meteors = []; bullets = []; score = 0; destroyedShip = None; shieldOn=false}
        let rec startGame() = 
//            let angleB =  mkVelocityAngleB (mouseRightButtonB .||. rightCommandB .||. (periodicB 0.1)) (mouseLeftButtonB.||. leftCommandB) 0.0 1.0 4.0
            let angleB =  mkVelocityAngleB (rightCommandB ) ( leftCommandB) 0.0 1.0 4.0
            let thrustB = (pureB (fun b -> if b then (Vector (1.25, 0.0)) else (Vector (0.0, 0.0)))) <$> thrustCommandB
        // meteor
            
            let stateB' = aliasB standbyGame
            let controllerB' = aliasB { destroyMeteor = []; destroyShip = None}
            let shipB = mkShipDynamics (pureB 1.0) angleB thrustB 0.0 Vector.zero Vector.zero (fst controllerB') (fst stateB')
            let meteorCol' = aliasB ([])
            let meteorVelocity = 0.2
            let meteors = List.map (fun _ -> mkBreakableMeteor 0.0 (randVector()) ((Vector.rot Vector.unit (randAngle())) * meteorVelocity) MeteorSize.Big (fst controllerB')  shipB)
                                   [] //[1;2;3;4]
            let thereAreMeteorsE = whenE ((pureB (fun l -> not (List.isEmpty l))) <$> (fst meteorCol'))
            let gameLevelB  = 
                    //printf "game level\n"
                    let e' = ((loopE thereAreMeteorsE [2;3;4;5;6;7;8;9;10;11]) =>> (fun (_, n) -> n))
                    (memoB (stepB 1 ( e')))

            let meteorCreatorE = ( (snapshotBehaviorOnlyE (someE ())  (splitMeteor meteorVelocity (fst controllerB') (fst meteorCol')  shipB thereAreMeteorsE gameLevelB) ))
            let meteorCol = bindAliasB (dyncolB2  meteors meteorCreatorE)  meteorCol'
            let delayedMeteorCol = untilB (pureB []) (waitE 2.0 --> ( meteorCol))
        // bullet
        //mkFire
            let bulletCol' = aliasB ([])
            let bulletCol = bindAliasB (dyncolB [] (mkFire shipB fireCommandB 1.0 2.0 (fst controllerB'))) bulletCol'
        
        // commands

            let controllerB = bindAliasB (controller delayedMeteorCol bulletCol shipB) controllerB'

            let createShipE = whenAnyE ((pureB (fun m -> m.ship)) <$> (fst stateB'))
            let decreaseNbShips n = printf "n = %A \n" n
                                    if n>0 then n-1 else 0
                                    n-1
            let nbShipsB = stepAccumB 3 (createShipE --> decreaseNbShips)

            let stateB = bindAliasB ((pureB (fun nbShips ship meteors bullets score destroyedShip shieldOn -> {nbShips = nbShips; ship = ship; meteors = meteors; bullets = bullets; score = score; destroyedShip = destroyedShip; shieldOn= shieldOn}))
                                                <$> ((pureB (+)) <$> nbShipsB <$>(addShipB (fst stateB')))
                                                <$> shipB 
                                                <$> delayedMeteorCol
                                                <$> bulletCol
                                                <$> (scoreB controllerB)
                                                <$> (destroyedShipB controllerB shipB)
                                                <$> shieldB) stateB'
            //let g = ((iterE (someE()) [1;2;3;4;5])) =>> (fun (_,i)->i)
            //let gg =  tronB "ggggggggggggg" (stepB (0) g)
            //let gameLevelB =  tronB "ggggggggggggg" (gameLevelB meteorCol)
            //let stateB2 = memoB (seqB gameLevelB' (seqB controllerB stateB))
            let stateB2 = memoB ((seqB controllerB stateB))
            let endGameE = whenE ((pureB (fun m -> m.nbShips = 0 && not (isSome m.ship) && not (isSome m.destroyedShip))) <$> stateB2)
            let newGameE = (whenE fireCommandB) =>> startGame
            let stateB3 = untilB stateB2 (endGameE --> untilB (pureB standbyGame) newGameE)
            seqB gameLevelB stateB3
        untilB (pureB standbyGame) ((whenE fireCommandB) =>> startGame)


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
     (*
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
    *)


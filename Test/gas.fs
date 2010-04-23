#light

// #r @"..\FReactive\obj\Debug\FReactive.dll"

namespace Xna

  module Gas =
           
    open FReactive.Misc
    open FReactive.FReactive
    open FReactive.Integration
    open FReactive.DynCol
    open FReactive.Lib
    open System
    open Vector
    
    open Microsoft.Xna.Framework
    open Microsoft.Xna.Framework.Graphics
    open Microsoft.Xna.Framework.Input;
    
    let rand = new System.Random();
    
    let randAngle() = rand.NextDouble() * 2.0*Math.PI
    
    let randX() = 2.0 * rand.NextDouble() - 1.0

    let randRange min max = min + rand.NextDouble() * (max-min)

    let randVector() = Vector(randX(), randX())
    
    let vectorNumClass = 
        {   plus = curry Vector.(+);
            minus = curry Vector.(-);
            mult = curry Vector.(*);
            div = curry Vector.(/);
            neg = Vector.neg
        }

    // some general purpose utility function    
    let (.*) a b = (pureB vectorNumClass.mult) <$> a <$> b 
    let (./) a b = (pureB vectorNumClass.div) <$> a <$> b 
    let (.+) a b = (pureB vectorNumClass.plus) <$> a <$> b 
    let (.-) a b = (pureB vectorNumClass.minus) <$> a <$> b 
    
    let mapOptionB f = pureB (mapOption f)
    let someizeBf b = (pureB Some) <$> b
    
    let delayB b v0 = 
        let rec bf b v0 t = let (r, nb) = atB b t
                            (v0, fun () -> Beh (bf (nb()) r))
        Beh (bf b v0)

    // movement


    type Box =  float * float * float * float // xmin, xmax, ymin, ymax

    let mkVelocityDir (Vector (vdirx0, vdiry0)) posB (boxB:Box Behavior) =
        let velAdapt x (xmin, xmax) v = 
            let e = (randRange -0.1 0.1)
            if (v > 0.0 && (xmax-0.01) <= x) || (v < 0.0 && x <= (xmin+0.01))
            then  -v + e
            else v

        let xB = (pureB (fun (Vector (x,_)) -> x)) <$> posB
        let yB = (pureB (fun (Vector (_,y)) -> y)) <$> posB
        let xboxB = (pureB (fun (a,b,_,_) -> (a,b))) <$> boxB
        let yboxB = (pureB (fun (_,_,a,b) -> (a,b))) <$> boxB

        let vdirxB' = aliasB vdirx0
        let vdiryB' = aliasB vdiry0
        let vdirxB = bindAliasB ((pureB velAdapt) <$> xB <$> xboxB <$> (fst vdirxB')) vdirxB'
        let vdiryB = bindAliasB ((pureB velAdapt) <$> yB <$> yboxB <$> (fst vdiryB')) vdiryB'
        let vdirB = (pureB (fun (x:float) y ->  let x' = if Math.Abs(x) < 0.1
                                                         then 0.1 * (float) (Math.Sign x)
                                                         else x
                                                Vector.norm (Vector (x',y)))) <$> vdirxB <$> vdiryB
        vdirB

    type Mol = Vector * bool  // pos, hit

    let mkMolecule t0 x0 vdir0 absVelocityB boxB hitCompf =
        let integrateWithConstraints = integrateWithConstraintsGenB vectorNumClass
        let box (xmin, xmax, ymin, ymax) (Vector (x, y))= 
                let x' = if x < xmin then xmin 
                                     else if xmax < x then xmax 
                                                      else x
                let y' = if y < ymin then ymin 
                                     else if ymax < y then ymax 
                                                      else y
                Vector (x', y')
        let boxBf = (pureB box) <$> boxB
        let xB' = aliasB x0
        let vdirB = mkVelocityDir vdir0 (fst xB') boxB
        let vB = vdirB .* absVelocityB
        let xB = bindAliasB (integrateWithConstraints boxBf vB t0 x0) xB'
        let previousVB = delayB vB Vector.zero
        coupleB () <$> xB <$> ((pureB hitCompf) <$> previousVB <$> vB)
        
    let mkCountCollision molsB = 
        let hitPred (_, hit) = hit
        let hitCount mols = List.fold (fun acc m -> acc + if hitPred m then 1 else 0) 0 mols 
        (pureB hitCount) <$> molsB


    let mkBoxPos x0 leftMolsB rightMolsB =
        let xB' = aliasB x0 
        let leftCounterB = mkCountCollision leftMolsB   //|> tronB "left"
        let rightCounterB = mkCountCollision rightMolsB   //|> tronB "right"
        let proc x lh rh =  let x' = x + ((float) lh)/1000.0 - ((float) rh)/1000.0 
                            let x'' = if x' > 0.9 then  0.9 else x'
                            let x''' = if x'' < -0.9 then  -0.9 else x''
                            x'''
        let xB  = bindAliasB ((pureB proc) <$>  (fst xB')  <$> leftCounterB <$> rightCounterB) xB'
        xB
         
    let mkBoxes xB = 
        let leftBoxB = (pureB (fun x -> (-1.0, x, -1.0, 1.0))) <$> xB
        let rightBoxB = (pureB (fun x -> (x, 1.0, -1.0, 1.0))) <$> xB
        (leftBoxB, rightBoxB)

    let mkMolecules t0 n (xmin, xmax, ymin, ymax) absVelocityB boxB compf =
        let proc i  =   let pos = Vector ((randRange xmin xmax), (randRange ymin  ymax))
                        someizeBf <| mkMolecule t0 pos (Vector.rot Vector.unit (randAngle())) absVelocityB boxB compf
        let mols = List.map proc (List.init n id)
        dyncolB mols (NoneE)

    let rec keyboardInputG key = Beh (fun _ -> ( Keyboard.GetState().IsKeyDown(key), fun () -> keyboardInputG key))
    let rec leftHeatB =  (keyboardInputG Keys.A)
    let rec leftCoolB =  (keyboardInputG Keys.Q)
    let rec rightHeatB =  (keyboardInputG Keys.P)
    let rec rightCoolB =  (keyboardInputG Keys.M)

    let mkVelocity t0 heatB coolB  = 
        let proc b = if b then 1.0 else 0.0
        let heatFlowB = (((pureB proc) <$> heatB) .-. ((pureB proc) <$> coolB))
        let velB = (integrate heatFlowB t0 0.3)
        let bound x = if x >= 2.0 then 2.0
                                  else if x <= 0.1 then 0.1 
                                                   else x
        let velB' = (pureB bound) <$> velB                                           
        velB'  |> memoB
        
    type State = 
        {
            x:float
            leftMols : Mol list
            rightMols : Mol list
        }

    let game (game:Game) = 
        let rec startGame t0 = 

            let leftAbsVelocityB =  mkVelocity t0 leftHeatB leftCoolB 
            let rightAbsVelocityB = mkVelocity t0 rightHeatB rightCoolB

            let x0 = 0.0
            let xB' = aliasB x0
            let (leftBoxB, rightBoxB) = mkBoxes (fst xB')
            let leftHitPred (Vector (xpv, _)) (Vector (xv, _)) = Math.Sign xpv > 0 && Math.Sign xv < 0
            let rightHitPred (Vector (xpv, _)) (Vector (xv, _)) = Math.Sign xpv < 0 && Math.Sign xv > 0
            let leftMolsB = mkMolecules t0 250 (-0.9, x0, -0.9, 0.9) leftAbsVelocityB leftBoxB leftHitPred
            let rightMolsB = mkMolecules t0 250 (x0, 0.9, -0.9, 0.9) rightAbsVelocityB rightBoxB rightHitPred
            let xB = bindAliasB (mkBoxPos x0 leftMolsB rightMolsB) xB'
            (pureB (fun x leftMols rightMols -> { x=x ; leftMols = leftMols; rightMols = rightMols})) <$> xB <$> leftMolsB <$> rightMolsB
        startGame 0.0

    
    
#light

// #r @"..\FReactive\obj\Debug\FReactive.dll"


namespace Xna

  module Bricks =
           
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



    let mousePos (game:Game) () = 
        let (xm, ym) = let ms = Mouse.GetState() in ((float)ms.X, (float)ms.Y)
        let (xw, yw, ww, hw) = let cb = game.Window.ClientBounds
                               ((float)cb.X, (float)cb.Y, (float)cb.Width, (float)cb.Height)
        let (xmo, ymo) = (2.0*(xm)/ww-1.0, 1.0-2.0*(ym)/hw)
        if (xmo < -1.0 || 1.0 < xmo || ymo < -1.0 || 1.0 < ymo) 
        then None
        else Some (xmo, ymo)

    let rot90Clockwise (Vector (x,y)) = (Vector (y, -x))
    let rot90AntiClockwise (Vector (x,y)) = (Vector (-y, x))
       
    let cross (Vector (xm, ym)) (Vector (x1, y1)) (Vector (x2, y2)) =
        let sign (x:float) = Math.Sign x
        if not (sign y1 = sign y2)
        then    let t = -y1 /(y2 - y1)
                let xcross = x1+ t *(x2-x1)
                0.0 <= xcross && xcross <= xm
        else false

    let hitCorner corner pPrev pCur radius =
        let inCircle point centre radius = (Vector.length (point-centre)) <= radius
        if not (inCircle corner pPrev radius) && (inCircle corner pCur radius)
        then let dx = Vector.norm (pPrev - corner)
             let dy = rot90AntiClockwise dx
             let adaptVel v =   let r = dx * (-(Vector.dot dx v)) + dy * (Vector.dot v dy)
                                printf "corner = %A %A\n"  dx dy
                                r
               
             Some adaptVel
        else None
      
    let invY (Vector (x,y)) = Vector (x, -y)
    let invX (Vector (x,y)) = Vector (-x, y)

    let crossBox pll plr pul pur xp x radius =
        let hitpll = hitCorner pll xp x radius
        let hitplr = hitCorner plr xp x radius
        let hitpul = hitCorner pul xp x radius
        let hitpur = hitCorner pur xp x radius
        let crossBottom =   let xp' = xp + Vector(0.0, radius)
                            let x' = x + Vector(0.0, radius)
                            cross (plr - pll) (xp'-pll) (x'-pll)
        let crossUp = let xp' = xp - Vector(0.0, radius)
                      let x' = x - Vector(0.0, radius)
                      cross ((pur - pul)) (invY (xp'-pul)) (invY (x'-pul))
        let crossRight = let xp' = xp - Vector(radius, 0.0)
                         let x' = x - Vector(radius, 0.0)
                         cross (rot90Clockwise (pur-plr)) ((rot90Clockwise (xp'-plr))) ((rot90Clockwise (x'-plr)))              
        let crossLeft = let xp' = xp + Vector(radius, 0.0)
                        let x' = x + Vector(radius, 0.0)
                        cross (rot90AntiClockwise (pur-plr) |> invX) (rot90AntiClockwise (xp'-pll) |> invX) (rot90AntiClockwise (x'-pll) |> invX)    
        let choice b v = if b then Some v else None
        let preds = [   hitpll;
                        hitplr;
                        hitpul;
                        hitpur;
                        choice crossBottom ( fun (Vector (x, y)) -> Vector (x, -y));
                        choice crossUp (fun (Vector (x, y)) -> Vector (x, -y));
                        choice crossRight (fun (Vector (x, y)) -> Vector (-x, y));
                        choice crossLeft (fun (Vector (x, y)) -> Vector (-x, y))
                    ]
        let pred = List.filter (fun x->isSome x)  preds
        match pred with
        |[] -> None
        |h::t -> h


    let brickWidth = 0.2
    let brickHeight = 0.1

    let paddleY = -0.95;
    let paddleHalfLength = 0.1;   

    let bricksCoord = 
        let xs = List.init 10 (fun i -> -1.0 + (float) i * brickWidth)
        let ys = List.init 5 (fun i -> 0.9 - (float) i * brickHeight)
        List.map (fun x -> List.map (fun y -> (x, y)) ys) xs |> List.concat

    type Brick = Brick of (int * Vector) // id, x, y

    let mkHits ballRadius bricks pBallOption ballOption  = 
        match pBallOption, ballOption with
        |Some pBall, Some ball ->
            let proc brick = let pll = brick 
                             let plr = brick + Vector(brickWidth, 0.0)
                             let pul = brick + Vector(0.0, brickHeight) 
                             let pur = brick + Vector(brickWidth, brickHeight) 
                             crossBox pll plr pul pur pBall ball ballRadius
            let r = List.map (fun (Brick (id, pos)) ->  match proc pos with
                                                        |Some f -> (Some (id, f))
                                                        |None -> None) bricks
            let r' = catOption r
            r'
            |_ -> []
       
    let mkBrick pos hitsB = 
        let id = createId()
        let isHit hits = let r = List.exists (fun (id', _) -> id = id') hits
                         r 
        let isHitE = whenE ((pureB isHit) <$> (hitsB)) --> (noneB())
        let brickB = untilB  (pureB (Some (Brick (id, pos)))) isHitE
        brickB

    let mkBricks hitsB = 
        let brickBs = List.map (fun pos -> mkBrick (Vector pos) hitsB) bricksCoord
        dyncolB brickBs NoneE
         
    let mkVelocity v0 hitsB ballB xPaddleB ballRadius = 
        let applyB = (pureB (fun f b -> f b))
        let hitWallx (Vector (x, y))  = 
            match (x <= -1.0 || x>= 1.0) with
            |(false) -> None
            |(true) -> Some (fun v -> (Vector.rot v (randRange -0.01 0.01)) |> invX)
        let hitWally (Vector (x, y)) xPaddle= 
            match ((y-ballRadius <= paddleY && xPaddle-paddleHalfLength <= x && x <= xPaddle+paddleHalfLength) || y>= 1.0) with
            |(false) -> None
            |(true) -> Some (fun v -> (Vector.rot v (randRange -0.01 0.01)) |> invY)
        let hitBricks hits =
            List.fold (fun acc (_, f) ->
                            match acc with
                            |None -> Some f
                            |Some acc -> Some (acc >> f))None hits
        let hitWallxE = whenAnyE ((pureB hitWallx) <$> ballB)
        let hitWallyE = whenAnyE ((pureB hitWally) <$> ballB <$> xPaddleB)
        let hitBrickE = whenAnyE ((pureB hitBricks) <$> hitsB)
        let comp a b  = a >> b
        let hitE =  orE comp (orE comp hitWallxE hitWallyE) hitBrickE
        let vB = stepAccumB v0 hitE
        let adjust (Vector (x, y)) = let y' =   if  Math.Abs(y) <= 0.2 
                                                then 0.2 * ((float) (Math.Sign(y)))
                                                else y
                                     Vector( x,y')       
        ((pureB adjust) <$> vB)  

     
    let mkPaddle x0 (game:Game) =
        let mousePos = mousePos game
        let rec mousePosEvt = Evt (fun _ -> (mousePos(), fun() -> mousePosEvt))
        let mousePosB  = stepB (0.0, 0.0) mousePosEvt
        let mousePosXB =  pureB  (fun x -> if x - paddleHalfLength < -1.0 
                                           then  (-1.0 )
                                           else if x - paddleHalfLength > 1.0
                                                then 1.0 
                                                else x)
                              <$> (pureB fst <$> mousePosB)
        mousePosXB

    type State = 
        { 
            ball:Vector option
            bricks:Brick list
            xpaddle : float
            nbBalls:int
        }

    let rec keyboardInputG key = Beh (fun _ -> ( Keyboard.GetState().IsKeyDown(key), fun () -> keyboardInputG key))
    let rec startCommandB =  (keyboardInputG Keys.Enter .||.  keyboardInputG Keys.Space)

    let ballRadius = 0.05

    let rec mkBall xPaddleB hitsB ballRadius x0 t0  = 
                let integrate = integrateGenB vectorNumClass
                let xB' = aliasB x0
                let v0 = (Vector.rot Vector.unit (Math.PI/4.0)) * 1.5
                let velB = mkVelocity v0 hitsB (fst xB') xPaddleB ballRadius
                let xB = bindAliasB (integrate velB t0 x0 ) xB' 
                let xpB = delayB xB ( x0)
                let ballOutE = whenE ((pureB (fun (Vector(x, y)) -> y <= -1.0  )) <$> xB)
                let ballB = (coupleB()) <$>  (someizeBf xB) <$> (someizeBf xpB)  //|> tronB "balls option"  
                           
                let ballB' = untilB (ballB)
                                    (ballOutE --> untilB ((coupleB()) <$> (noneB()) <$> (noneB())) 
                                                         (waitE 2.0 =>> ( fun () -> startB ( mkBall xPaddleB hitsB ballRadius x0))))
                ballB' 

    


    let rec game (theGgame:Game) = 
        let noGame =  {   ball=Some (Vector( 0.0, 0.0))
                          bricks=[]
                          xpaddle = 0.0
                          nbBalls = 3}
        
        let rec startGame t0 = 
            let x0 = (Vector.zero)

            let xPaddleB = mkPaddle 0.0 theGgame
            let hitsB' = aliasB []
            let ballB =  mkBall xPaddleB (fst hitsB') ballRadius x0 t0 |> memoB
            let xB =  (pureB fst) <$> ballB   |>memoB      //|> tronB ""
            let xpB = (pureB snd) <$> ballB    

            let bricksB' = aliasB []
            let hitsB = bindAliasB ((pureB (mkHits ballRadius)) <$> (fst bricksB') <$> xpB <$> xB )  hitsB'
            let bricksB =  bindAliasB (mkBricks hitsB) bricksB'
            let gameB = (pureB (fun ball bricks xpaddle -> 
                                    {   ball=ball
                                        bricks=bricks
                                        xpaddle = xpaddle
                                        nbBalls = 0 })) <$> xB <$> bricksB <$> xPaddleB
            gameB
        let rec proc () = 
            let  mkGame t0 = let stateB = (startGame t0) 
                             // let endGameE = whenE ((pureB (fun state -> state.ball = None)) <$>  stateB)
                             stateB
            untilB (pureB noGame)
                   (whenE (startCommandB) --> (startB mkGame))
        let stateB = proc()  |> memoB
        let decrBallNbE = whenE ((pureB (fun state -> state.ball=None)) <$> stateB) --> (fun x -> x-1) 
        let nbBallsB = stepAccumB 3 decrBallNbE  // |> tronB "balls " 
        untilB stateB (whenE (nbBallsB .<=. (pureB 0)) =>> fun () -> game(theGgame))
        


        (*



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
















    type HitDir = VertHit|HorHit

    let hitBrick (xb, yb) (xp,yp) (x, y) = 
        let xor a b = (a || b) && not (a && b) 
        let xmin = xb
        let xmax = xb + brickWidth
        let ymin = yb
        let ymax = yb + brickHeight

        let inPred (xb, yb) (x,y) = (xb <= xmin && x <= xmax && ymin <= y && x <= ymax)
        let cross  xmin xmax yb xp yp x y = xmin <= xp && xp <= xmax && xmin <= x && x <= xmax &&  (yp <= yb && yb < y) 
        let cross' b v = if b then Some v else None
        if  not (inPred (xb, yb) (xp, yp)) && (inPred (xb, yb) (x, y)) 
        then let preds = 
                let crossP = cross 
                List.map (uncurry cross')
                        [(cross xmin xmax ymin xp yp x y, VertHit);
                         (cross xmin xmax -ymax xp -yp x -y, VertHit);
                         (cross ymin ymax xmin yp xp y x, HorHit);
                         (cross ymin ymax -xmax yp -xp y -x, HorHit)]
             let r = List.filter isSome preds
             match r with
             |[] -> None
             |h::_ -> h
        else None

        *)
#light
(*
#I @"C:\Program Files\Microsoft XNA\XNA Game Studio\v3.1\References\Windows\x86"

#r "Microsoft.Xna.Framework.dll"
#r "Microsoft.Xna.Framework.Game.dll"
*)

namespace Xna
 module Main =
  open System
  open FReactive.Misc
  open FReactive.FReactive
  open FReactive.Integration
  open FReactive.Lib
 
  open Microsoft.Xna.Framework
  open Microsoft.Xna.Framework.Graphics
  open  Microsoft.Xna.Framework.Input;

  open Game
  open Paddle
  open DynCol
  open Asteroids
  open Vector

  type  XnaTest<'a>(game : Game -> 'a Behavior) as this =
    inherit Game()

  //let mutable cameraPos : Vector3

    let mutable cameraView : Matrix = Matrix.Identity
    let mutable cameraProjection : Matrix = Matrix.Identity

    let mutable  graphics : GraphicsDeviceManager = null


    let mutable effect : Graphics.Effect =null


    let mutable vertexDeclaration : Graphics.VertexDeclaration = null

    let mutable behavior : unit-> 'a Behavior = fun () -> game this
    let mutable time : float = 0.0

    let rand = System.Random()

    (*let n_verts = 2
    let random_vert _ =
        Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
    let vertex = Array.init n_verts random_vert
    *)

    do
        graphics <- new GraphicsDeviceManager(this)

    override this.Initialize() =
        base.Initialize()
        this.IsMouseVisible <- true
        let gd = graphics.GraphicsDevice
        effect <- new Graphics.BasicEffect(gd, null)
        let elts = Graphics.VertexPositionColor.VertexElements
        vertexDeclaration <- new Graphics.VertexDeclaration(gd, elts)
    
        let cameraPos = new Vector3((float32)0.0, (float32)0.0, (float32)5.0);

        cameraView <- Matrix.CreateLookAt(cameraPos, Vector3.Zero, Vector3.Up) ;
        cameraProjection <- Matrix.CreatePerspectiveFieldOfView(
                                    MathHelper.PiOver4,
                                    (float32) this.Window.ClientBounds.Width /(float32)this.Window.ClientBounds.Height,
                                    (float32)1.0, (float32)100.0) 
    with
    
        member this.Graphics with get() = graphics
        member this.Effect with get() = effect
        member this.VertexDeclaration with get() = vertexDeclaration
        member this.Behavior with get() = behavior and set(value) = behavior <- value
        member this.Time with get() = time and set(value) = time <- value

        member g.getMousePos() = 
            let (xm, ym) = let ms = Mouse.GetState() in ((float)ms.X, (float)ms.Y)
            let (xw, yw, ww, hw) =  let cb = g.Window.ClientBounds
                                    ((float)cb.X, (float)cb.Y, (float)cb.Width, (float)cb.Height)
            let (xmo, ymo) = (2.0*(xm)/ww-1.0, 1.0-2.0*(ym)/hw)
            //printf "%A\n" (xm, ym, xmo, ymo,xw, yw, ww, hw)
            if (xmo < -1.0 || 1.0 < xmo || ymo < -1.0 || 1.0 < ymo) 
            then None
            else Some  (xmo, ymo)



  type XnaSpring(game : Game -> (float*float) Behavior) as this =
   inherit XnaTest<(float*float)>(game)

    override this.Draw gameTime =
//        printf "%A\n" gameTime.ElapsedGameTime
        let gd = base.Graphics.GraphicsDevice
        gd.VertexDeclaration <- base.VertexDeclaration
        gd.Clear Graphics.Color.Black
   
        let n_verts = 2
        let random_vert _ =
            Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
        let vertex = Array.init n_verts random_vert

        do  let ((x,y), nb) = atB (base.Behavior())  (base.Time)
            base.Behavior <- nb
            base.Time <- base.Time + (1.0/60.0)
            vertex.[1].Position <- Vector3((float32)x, (float32)y, (float32) 0.0)
   
   
        base.Effect.Begin()
        for pass in base.Effect.CurrentTechnique.Passes do
            pass.Begin()
            let prim = PrimitiveType.LineList
            gd.DrawUserPrimitives(prim, vertex, 0, n_verts/2)
            pass.End()
        base.Effect.End()


  type XnaPaddle(game : Game -> Paddle.State Behavior) as this =
   inherit XnaTest<Paddle.State>(game)
    override this.Draw gameTime =
        let gd = base.Graphics.GraphicsDevice
        gd.VertexDeclaration <- base.VertexDeclaration
        gd.Clear Graphics.Color.Black
   
        let n_verts = 2
        let random_vert _ =
            Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
        let vertex = Array.init n_verts random_vert

        do  let (state, nb) = atB (base.Behavior()) (base.Time)
            base.Behavior <- nb
            base.Time <- base.Time + (1.0/60.0)
            match state with
            |End ->  printf "End\n" 
                     ()
            |State  {xball=x; yball=y ; xpaddle = xp} as s->
                    //printf "%A\n" s
                    vertex.[0].Position <- Vector3((float32)(xp-0.1), (float32)(-0.95), (float32) 0.0)
                    vertex.[1].Position <- Vector3((float32)(xp+0.1), (float32)(-0.95), (float32) 0.0)
                    base.Effect.Begin()
                    for pass in base.Effect.CurrentTechnique.Passes do
                        pass.Begin()
                        let prim = PrimitiveType.LineList
                        gd.DrawUserPrimitives(prim, vertex, 0, 1)
                        vertex.[0].Position <- Vector3((float32)x, (float32)y, (float32) 0.0)
                        gd.DrawUserPrimitives(PrimitiveType.PointList, vertex, 0, 1)
                        pass.End()
                    base.Effect.End()


  type XnaDynCol(game : Game -> (float*float) list Behavior) as this =
   inherit XnaTest<(float*float) list>(game)
    override this.Draw gameTime =
        let gd = base.Graphics.GraphicsDevice
        gd.VertexDeclaration <- base.VertexDeclaration
        gd.Clear Graphics.Color.Black
   

        do  let (pointList, nb) = atB (base.Behavior()) (base.Time)
            base.Behavior <- nb
            base.Time <- base.Time + (1.0/60.0)
            
            
            let n_verts = List.length pointList
            let random_vert _ =
                Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
            let vertex = Array.init n_verts random_vert
            List.iteri (fun i (x,y) -> vertex.[i].Position <- Vector3((float32)x, (float32)y, (float32) 0.0)) pointList
            if n_verts > 0 
            then    base.Effect.Begin()
                    for pass in base.Effect.CurrentTechnique.Passes do
                                pass.Begin()
                                gd.DrawUserPrimitives(PrimitiveType.PointList, vertex, 0, n_verts)
                                pass.End()
                    base.Effect.End()


  // asteroids

  let drawShip (gd:GraphicsDevice) scale = 
//        let pts = [(.0, .0);]
        let pts = List.map (fun (x,y) -> (x/40.0, y/40.0)) [(-2.0, 2.0);(4.0, 0.0);(-2.0, -2.0);(0.0, 0.0);(-2.0, 2.0)]
        let n_verts = List.length pts
        let random_vert _ =  Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
        let vertex = Array.init n_verts random_vert

        let jet_n_verts = 3
        let jet_random_vert _ =  Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
        let jet_vertex = Array.init jet_n_verts jet_random_vert

        let iter f pts (vertex: VertexPositionColor[]) = 
                List.iteri (fun i (x,y) -> let (x', y') = f x y
                                           vertex.[i].Position <- Vector3((float32)x', (float32)y', (float32) 0.0)) pts
        let draw (gd:GraphicsDevice) x y angle jet = 
            //printf "ship %A\n" angle 
            let cosa = Math.Cos angle                 
            let sina = Math.Sin angle 
            let f x' y' = x + (x' * cosa - y' * sina) * scale, y + (x' * sina + y' * cosa) * scale           
            iter f pts vertex
            gd.DrawUserPrimitives(PrimitiveType.LineStrip, vertex, 0, n_verts-1)
            // jet
            match jet with
            |Some s ->  let l = match s with
                                |JetSmall -> -2.0
                                |JetMedium -> -3.0
                                |JetBig -> -4.0
                        let jetpts = List.map (fun (x,y) -> (x/40.0, y/40.0)) [(-1.0, 1.0);(l, 0.0);(-1.0, -1.0)]
                        iter f jetpts jet_vertex
                        gd.DrawUserPrimitives(PrimitiveType.LineStrip, jet_vertex, 0, jet_n_verts-1)
            |None -> ()
        draw gd       
  
  let drawShip' (gd:GraphicsDevice) (Ship (_, Vector(x, y), angle, jet)) = drawShip gd 1.0 x y angle jet
  
  
  let drawExplodingShip (gd:GraphicsDevice)  = 
        let pts = List.map (fun ((x,y), (x1, y1)) -> ((x/40.0, y/40.0), (x1/40.0, y1/40.0)))
                                                           [((-2.0, 2.0), (4.0, 0.0));
                                                            ((4.0, 0.0), (-2.0, -2.0));
                                                            ((-2.0, -2.0), (0.0, 0.0));
                                                            ((0.0, 0.0), (-2.0, 2.0))]
        let dirs = List.map (fun (x,y) -> (x/40.0, y/40.0)) [(1.0, 1.0); (1.0, -1.0); (-1.0, -1.0); (-1.0, 1.0)]

        let n_verts = List.length pts * 2
        let random_vert _ =  Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
        let vertex = Array.init n_verts random_vert

        let iter f pts (vertex: VertexPositionColor[]) = 
                List.iteri (fun i (x,y) -> let (x', y') = f x y
                                           vertex.[i].Position <- Vector3((float32)x', (float32)y', (float32) 0.0)) pts
        let draw (gd:GraphicsDevice) x y angle scale = 
            if scale > 0.0
            then 
                let pts' = List.map2 (fun ((x,y), (x1, y1)) (dirx, diry) -> ((x+dirx*scale, y+diry*scale), (x1+dirx*scale, y1+diry*scale))) pts dirs
                let pts'' = List.fold (fun acc (a,b) -> acc @ [a;b] ) [] pts'
                //printf "ship %A\n" angle 
                let cosa = Math.Cos angle                 
                let sina = Math.Sin angle 
                let f x' y' = x + (x' * cosa - y' * sina) , y + (x' * sina + y' * cosa)            
                iter f pts'' vertex
                gd.DrawUserPrimitives(PrimitiveType.LineList, vertex, 0, n_verts/2)
        draw gd       
  
  let drawExplodingShip' (gd:GraphicsDevice) (DestroyedShip ((Vector(x, y), angle, scale))) = drawExplodingShip gd x y angle scale
  

  let drawMeteor (gd:GraphicsDevice)  = 
//        let pts = [(.0, .0);]
        let angles = Seq.toList (seq{for i in 1 .. 11 -> Math.PI*2.0*((float)i)/10.0})
        let ptsB = List.map (fun a-> (Vector.rot Vector.unit a) * (MeteorSize.size MeteorSize.Big)) angles
        let ptsM = List.map (fun a-> (Vector.rot Vector.unit a) * (MeteorSize.size MeteorSize.Medium)) angles
        let ptsS = List.map (fun a-> (Vector.rot Vector.unit a) * (MeteorSize.size MeteorSize.Small)) angles
        let n_verts = List.length ptsB
        let random_vert _ =  Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
        let vertex = Array.init n_verts random_vert
        let iter f size = List.iteri (fun i (Vector(x,y)) -> let (x', y') = f x y
                                                             vertex.[i].Position <- Vector3((float32)x', (float32)y', (float32) 0.0))        
                                     (match size with
                                      |MeteorSize.Big -> ptsB
                                      |MeteorSize.Medium -> ptsM
                                      |MeteorSize.Small -> ptsS)
        let draw (gd:GraphicsDevice) x y size = 
            let f x' y' = x + x', y + y'          
            iter f size
            gd.DrawUserPrimitives(PrimitiveType.LineStrip, vertex, 0, n_verts-1)
        draw  gd   

  let drawMeteor' (gd:GraphicsDevice) (Meteor (_, Vector(x, y), msize)) = drawMeteor gd x y msize

  let drawShield (gd:GraphicsDevice) = 
        let angles = Seq.toList (seq{for i in 1 .. 31 -> Math.PI*2.0*((float)i)/30.0})
        let pts = List.map (fun a-> (Vector.rot Vector.unit a) * 0.12) angles
        let n_verts = List.length pts
        let random_vert _ =  Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
        let vertex = Array.init n_verts random_vert
        let iter f  = List.iteri (fun i (Vector(x,y)) -> let (x', y') = f x y
                                                         vertex.[i].Position <- Vector3((float32)x', (float32)y', (float32) 0.0))       
                                     
        let draw (gd:GraphicsDevice) x y  = 
            let f x' y' = x + x', y + y'          
            iter f pts
            gd.DrawUserPrimitives(PrimitiveType.PointList, vertex, 0, n_verts)
        draw  gd   
 
  let drawShield' (gd:GraphicsDevice) shieldOn ship = 
    match (shieldOn, ship) with
    |(true, Some (Ship (_, Vector(x, y), _, _))) -> drawShield gd x y 
    |_ ->()


  let drawBullet (gd:GraphicsDevice) = 
//        let pts = [(.0, .0);]
        let n_verts = 1
        let random_vert _ =  Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
        let vertex = Array.init n_verts random_vert
        let draw (gd:GraphicsDevice) x y =  
             vertex.[0].Position <- Vector3((float32)x, (float32)y, (float32) 0.0)
             gd.DrawUserPrimitives(PrimitiveType.PointList, vertex, 0, n_verts)
        draw gd                 

  let drawBullet' (gd:GraphicsDevice) (Bullet (_, Vector(x, y))) =  drawBullet gd x y
 
  let drawRemainingShips (gd:GraphicsDevice) n = 
    let xoffset = seq { for i in 0 .. n-1 -> (float) i * 0.05 }
    Seq.iter (fun i -> drawShip gd 0.2 (-0.95 + i ) 0.9 (Math.PI/2.0) None) xoffset

        (*
  let draw gd elem = 
    match elem with
    |S (Ship (Vector (x,y), angle)) -> drawShip gd x y angle
    |B (Bullet (Vector (x,y))) -> drawBullet gd x y
    |M (Meteor ((Vector (x,y)), size)) -> drawMeteor gd x y size
    |_ -> () 
    *)
  let dtm = ref 0.0
  let n = ref 0.0

  type XnaMeteor(game : Game -> GameState Behavior) as this =
   inherit XnaTest<GameState>(game)
   

     override this.Draw gameTime =
        let gd = base.Graphics.GraphicsDevice
        gd.VertexDeclaration <- base.VertexDeclaration
        gd.Clear Graphics.Color.Black
   
        do  let t0 = DateTime.Now;
            let nb = (base.Behavior())
            let ({nbShips = nbShips; ship=ship; meteors=meteors; bullets=bullets; score=score
                  destroyedShip = destroyedShip;
                  shieldOn = shieldOn}, nb) = atB nb (base.Time)
            let t1 = DateTime.Now;
            let dt = (float) (t1-t0).Milliseconds / 1000.0
            dtm := (!dtm * !n + (float)dt)/(!n + 1.0)
            n := !n + 1.0
            let r = (dt * 60.0)
            //if r>0.1 then printf "time = %A %A \n" (!dtm * 60.0) (r)
            base.Behavior <- nb
            base.Time <- base.Time + (1.0/60.0)
            base.Effect.Begin()
            for pass in base.Effect.CurrentTechnique.Passes do
                                pass.Begin()
                                match ship with
                                |Some ship' -> drawShip' gd ship' 
                                |None -> ()
                                match destroyedShip with
                                |Some ship' ->  //printf "destroyed meteor %A\n" ship'
                                                drawExplodingShip' gd ship' 
                                |None -> ()
                                List.iter (fun e -> drawMeteor' gd e) meteors
                                List.iter (fun e -> drawBullet' gd e) bullets
                                drawRemainingShips gd nbShips
                                drawShield' gd shieldOn ship
                                //printf "score = %A\n" score
                                //printf "nbShips = %A\n" nbShips
                                //gd.DrawUserPrimitives(PrimitiveType.LineList, vertex, 0, n_verts/2)
                                pass.End()
            base.Effect.End()

  open Gas
  
  let drawMolecule (gd:GraphicsDevice)  = 

        let angles = Seq.toList (seq{for i in 1 .. 11 -> Math.PI*2.0*((float)i)/10.0})
        let pts = List.map (fun a-> (Vector.rot Vector.unit a) * 0.02) angles
        let n_verts = List.length pts
        let random_vert _ =  Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
        let vertex = Array.init n_verts random_vert
        let iter f      = List.iteri (fun i (Vector(x,y)) -> let (x', y') = f x y
                                                             vertex.[i].Position <- Vector3((float32)x', (float32)y', (float32) 0.0))        
                                     pts
        let draw (gd:GraphicsDevice) x y  = 
            let f x' y' = x + x', y + y'          
            iter f 
            gd.DrawUserPrimitives(PrimitiveType.LineStrip, vertex, 0, n_verts-1)
        draw  gd   

  let drawMolecule' (gd:GraphicsDevice) (Vector(x, y)) = drawMolecule gd x y 
  
  let drawWall (gd:GraphicsDevice) = 
        let angles = Seq.toList (seq{for i in 1 .. 11 -> Math.PI*2.0*((float)i)/10.0})
        let pts = List.map (fun a-> (Vector.rot Vector.unit a) * 0.02) angles
        let n_verts = List.length pts
        let random_vert _ =  Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
        let vertex = Array.init n_verts random_vert
        let iter f      = List.iteri (fun i (Vector(x,y)) -> let (x', y') = f x y
                                                             vertex.[i].Position <- Vector3((float32)x', (float32)y', (float32) 0.0))        
                                     pts
        let draw (gd:GraphicsDevice) x  = 
            let vertex = Array.init 2 random_vert
            vertex.[0].Position <- Vector3((float32)x, (float32)(-1.0), (float32) 0.0) 
            vertex.[1].Position <- Vector3((float32)x, (float32)(1.0), (float32) 0.0) 
            gd.DrawUserPrimitives(PrimitiveType.LineStrip, vertex, 0, 1)
        draw gd   

  let drawWall' (gd:GraphicsDevice) x = drawWall gd x
          
  type XnaGas(game : Game -> Gas.State Behavior) as this =
   inherit XnaTest<Gas.State>(game)
   

     override this.Draw gameTime =
        let gd = base.Graphics.GraphicsDevice
        gd.VertexDeclaration <- base.VertexDeclaration
        gd.Clear Graphics.Color.Black
   
        do  let t0 = DateTime.Now;
            let nb = (base.Behavior())
            let ({
                    x=x
                    leftMols = leftMols
                    rightMols = rightMols
                 }, nb) = atB nb (base.Time)
            base.Behavior <- nb
            base.Time <- base.Time + (1.0/60.0)
            base.Effect.Begin()
            for pass in base.Effect.CurrentTechnique.Passes do
                                pass.Begin()
                                drawWall' gd x
                                List.iter (fun (pos, _) -> drawMolecule' gd pos) leftMols
                                List.iter (fun (pos, _) -> drawMolecule' gd pos) rightMols
                                pass.End()
            base.Effect.End()



  open Bricks

  
  let drawBrick (gd:GraphicsDevice)  = 
        let ds = [(0.0, 0.0); (0.2, 0.0); (0.2, 0.1); (0.0, 0.1); (0.0, 0.0)]
        let n_verts = List.length ds
        let random_vert _ =  Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
        let vertex = Array.init n_verts random_vert
        let f x y = List.iteri (fun i (dx, dy) -> let (x', y') = (x+dx, y+dy)
                                                  vertex.[i].Position <- Vector3((float32)x', (float32)y', (float32) 0.0))        
                                     ds
        let draw (gd:GraphicsDevice) x y  = 
            f x y
            gd.DrawUserPrimitives(PrimitiveType.LineStrip, vertex, 0, n_verts-1)
        draw  gd   

  let drawBrick' (gd:GraphicsDevice) (Brick (_, (Vector(x, y)))) = drawBrick gd x y 
          
  let drawBall (gd:GraphicsDevice)  = 

        let angles = Seq.toList (seq{for i in 1 .. 11 -> Math.PI*2.0*((float)i)/10.0})
        let pts = List.map (fun a-> (Vector.rot Vector.unit a) * Bricks.ballRadius) angles
        let n_verts = List.length pts
        let random_vert _ =  Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
        let vertex = Array.init n_verts random_vert
        let iter f      = List.iteri (fun i (Vector(x,y)) -> let (x', y') = f x y
                                                             vertex.[i].Position <- Vector3((float32)x', (float32)y', (float32) 0.0))        
                                     pts
        let draw (gd:GraphicsDevice) x y  = 
            let f x' y' = x + x', y + y'          
            iter f 
            gd.DrawUserPrimitives(PrimitiveType.LineStrip, vertex, 0, n_verts-1)
        draw  gd   

  let drawBall' (gd:GraphicsDevice) (Vector(x, y)) = drawBall gd x y 

  let drawPaddle (gd:GraphicsDevice)  = 
        let random_vert _ =  Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
        let vertex = Array.init 2 random_vert
        let draw (gd:GraphicsDevice) x  = 
                    vertex.[0].Position <- Vector3((float32)(x-paddleHalfLength), (float32)(paddleY), (float32) 0.0)
                    vertex.[1].Position <- Vector3((float32)(x+paddleHalfLength), (float32)(paddleY), (float32) 0.0)
                    gd.DrawUserPrimitives(PrimitiveType.LineStrip, vertex, 0, 1)
        draw  gd   

  let drawPaddle' (gd:GraphicsDevice) x = drawPaddle gd x
          
  type XnaBricks(game : Game -> Bricks.State Behavior) as this =
   inherit XnaTest<Bricks.State>(game)
         
     override this.Draw gameTime =
        let gd = base.Graphics.GraphicsDevice
        gd.VertexDeclaration <- base.VertexDeclaration
        gd.Clear Graphics.Color.Black
   
        do  let t0 = DateTime.Now;
            let nb = (base.Behavior())
            let ({ball=ballOption
                  bricks=bricks
                  xpaddle = xpaddle}, nb) = atB nb (base.Time)
            base.Behavior <- nb
            base.Time <- base.Time + (1.0/60.0)
            base.Effect.Begin()
            for pass in base.Effect.CurrentTechnique.Passes do
                                pass.Begin()
                                List.iter (drawBrick' gd ) bricks
                                match ballOption with
                                |Some ball -> drawBall' gd ball
                                |None -> ()
                                drawPaddle' gd xpaddle
                                pass.End()
            base.Effect.End()

  do use game = new XnaBricks(Bricks.game)
//  do use game = new XnaGas(Gas.game)
//  do use game = new XnaMeteor(Asteroids.game)
//  do use game = new XnaDynCol(DynCol.game)
  //do use game = new XnaPaddle(Paddle.game)
//  do use game = new XnaSpring(Game.game)
     game.Run()


     (*
                 let n_verts = 2
            let random_vert _ =
                Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
            let vertex = Array.init n_verts random_vert
            vertex.[0].Position <- Vector3((float32)x, (float32)y, (float32) 0.0)
            let x2 = x+0.1 * Math.Cos angle
            let y2 = y+0.1 * Math.Sin angle
            printf "%A\n" (Vector.length (Vector(x2,y2) - Vector (x,y)))
            printf "%A %A\n" (x2,y2)  (x,y)
            vertex.[1].Position <- Vector3((float32)x2, (float32)y2, (float32) 0.0)

            if (Mouse.GetState().MiddleButton = ButtonState.Pressed)
            then 
                printf "\n"
            else ()
            if n_verts > 0 

                      (*
            let (x,y) = match base.getMousePos() with
                        |Some v -> v
                        |None -> (0.0, 0.0)
            *)

     *)



(*
type XnaTest() as this =
  inherit Game()


  let mutable graphics : GraphicsDeviceManager = null


  let mutable effect : Graphics.Effect = null


  let mutable vertexDeclaration : Graphics.VertexDeclaration = null


  let rand = System.Random()
  let n_verts = 2
  let random_vert _ =
    Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
  let vertex = Array.init n_verts random_vert

  do
    graphics <- new GraphicsDeviceManager(this)

  override this.Initialize() =
    base.Initialize()
    this.IsMouseVisible <- true
    let gd = graphics.GraphicsDevice
    effect <- new Graphics.BasicEffect(gd, null)
    let elts = Graphics.VertexPositionColor.VertexElements
    vertexDeclaration <- new Graphics.VertexDeclaration(gd, elts)

  override this.Draw gameTime =
    printf "%A\n" gameTime.ElapsedGameTime
    let gd = graphics.GraphicsDevice
    gd.VertexDeclaration <- vertexDeclaration
    gd.Clear Graphics.Color.Black
   
    do match this.getMousePos() with
       |None -> ()
       |Some (x,y) -> vertex.[1].Position <- Vector3((float32)x, (float32)y, (float32) 0.0)
   
         // The mouse x and y positions are returned relative to the
            // upper-left corner of the game window.
           

    //vertex.[1].Position <- Vector3((float32)current_mouse.X, (float32)current_mouse.Y, (float32) 0.0)
   
    effect.Begin()
    for pass in effect.CurrentTechnique.Passes do
      pass.Begin()
      let prim = PrimitiveType.LineList
      gd.DrawUserPrimitives(prim, vertex, 0, n_verts/2)
      pass.End()
    effect.End()

  with
    member g.getMousePos() = 
        let (xm, ym) = let ms = Mouse.GetState() in ((float)ms.X, (float)ms.Y)
        let (xw, yw, ww, hw) = let cb = g.Window.ClientBounds
                               ((float)cb.X, (float)cb.Y, (float)cb.Width, (float)cb.Height)
        let (xmo, ymo) = (2.0*(xm)/ww-1.0, 1.0-2.0*(ym)/hw)
        printf "%A\n" (xm, ym, xmo, ymo,xw, yw, ww, hw)
        if (xmo < -1.0 || 1.0 < xmo || ymo < -1.0 || 1.0 < ymo) 
        then None
        else Some  (xmo, ymo)

(*  override this.Draw gameTime =
    let gd = graphics.GraphicsDevice
    gd.VertexDeclaration <- vertexDeclaration
    gd.Clear Graphics.Color.Black
    for i=0 to n_verts - 1 do
      let x() = 2. * rand.NextDouble() - 1. |> float32
      vertex.[i].Position <- Vector3(x(), x(), x())
    effect.Begin()
    for pass in effect.CurrentTechnique.Passes do
      pass.Begin()
      let prim = PrimitiveType.LineList
      gd.DrawUserPrimitives(prim, vertex, 0, n_verts/2)
      pass.End()
    effect.End()
    *)

do
  use game = new XnaTest()
  game.Run()
  *)
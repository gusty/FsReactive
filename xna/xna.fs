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

  

  let mousePos (game:Game) () = 
        let (xm, ym) = let ms = Mouse.GetState() in ((float)ms.X, (float)ms.Y)
        let (xw, yw, ww, hw) = let cb = game.Window.ClientBounds
                               ((float)cb.X, (float)cb.Y, (float)cb.Width, (float)cb.Height)
        let (xmo, ymo) = (2.0*(xm)/ww-1.0, 1.0-2.0*(ym)/hw)
        if (xmo < -1.0 || 1.0 < xmo || ymo < -1.0 || 1.0 < ymo) 
        then None
        else Some (xmo, ymo)

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
            if (xmo < -1.0 || 1.0 < xmo || ymo < -1.0 || 1.0 < ymo) 
            then None
            else Some  (xmo, ymo)
            
  type  XnaTest2(game : Game -> (GraphicsDevice -> unit) Behavior) as this =
    inherit Game()

    let mutable cameraView : Matrix = Matrix.Identity
    let mutable cameraProjection : Matrix = Matrix.Identity

    let mutable  graphics : GraphicsDeviceManager = null

    let mutable effect : Graphics.Effect = null

    let mutable vertexDeclaration : Graphics.VertexDeclaration = null

    let mutable behavior : unit-> (GraphicsDevice -> unit) Behavior = fun () -> game this
    let mutable time : float = 0.0

    let rand = System.Random()

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
 
    override this.Draw gameTime =
        let (gd:GraphicsDevice) = graphics.GraphicsDevice
        gd.VertexDeclaration <- this.VertexDeclaration 
        gd.Clear Graphics.Color.Black
   
        do  let t0 = DateTime.Now;
            let nb = (this.Behavior())
            let (renderf, nb) = atB nb (this.Time)
            this.Behavior <- nb
            this.Time <- this.Time + (1.0/60.0)
            effect.Begin()
            for pass in effect.CurrentTechnique.Passes do
                     pass.Begin()
                     renderf gd
                     pass.End()
            effect.End()
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
            if (xmo < -1.0 || 1.0 < xmo || ymo < -1.0 || 1.0 < ymo) 
            then None
            else Some  (xmo, ymo)
    end

 (*
  type XnaBricks(game : Game -> (GraphicsDevice -> unit) Behavior) as this =
   inherit XnaTest<(GraphicsDevice -> unit)>(game)
         
     override this.Draw gameTime =
        let gd = base.Graphics.GraphicsDevice
        gd.VertexDeclaration <- base.VertexDeclaration
        gd.Clear Graphics.Color.Black
   
        do  let t0 = DateTime.Now;
            let nb = (base.Behavior())
            let (renderf, nb) = atB nb (base.Time)
            base.Behavior <- nb
            base.Time <- base.Time + (1.0/60.0)
            base.Effect.Begin()
            for pass in base.Effect.CurrentTechnique.Passes do
                                pass.Begin()
                                renderf gd
                                pass.End()
            base.Effect.End()


            *)

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

(*
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

*)

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

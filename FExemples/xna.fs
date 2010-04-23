#light
(*
#I @"C:\Program Files\Microsoft XNA\XNA Game Studio\v3.1\References\Windows\x86"

#r "Microsoft.Xna.Framework.dll"
#r "Microsoft.Xna.Framework.Game.dll"
*)


 type Time = float
 type 'a Behavior = Beh of (Time -> ('a * (unit -> 'a Behavior)))
 
 
 let atB (Beh bf) = bf

 let rec run b n  =
    match n with
    |0 -> 0
    |n -> let (r, nb) = atB b 0.0
          run (nb()) (n-1)


 let rec pureB f = Beh (fun t -> (f, fun() -> pureB f))

 let rec (<$>) (Beh f) (Beh baf) = 
    let rec bf t = let (r,nb) = baf t
                   let (rf, nbf) = f t
                   (rf r, fun () -> (nbf()) <$> (nb()))
    Beh bf


 let rec oneB = Beh (fun _ -> (1, fun() -> oneB))
 let incrB v = 
    let rec bf v t = (v+1, fun() -> Beh (bf (v+1)))
    Beh (bf v)

 let b = (pureB (+)) <$> oneB  <$> (incrB 0) 

 run b 20000000



 
 type Time = float
 type 'a Behavior = Beh of (Time -> ('a))
 
 
 let atB (Beh bf) = bf

 let rec run b n  =
    match n with
    |0 -> 0
    |n -> let r = atB b 0.0
          run b (n-1)


 let rec pureB f = Beh (fun _ -> f)

 let rec (<$>) (Beh f) (Beh baf) = 
    let rec bf t = let (r) = f  t
                   let (rf) = baf t
                   (r rf)
    Beh bf


 let rec oneB = Beh (fun _ -> 1)
 let incrB v = 
    let rr = ref v
    let rec bf t = rr := !rr + 1
                   !rr
    Beh (bf )
    
 let b' = (pureB (+)) <$> oneB  <$> (incrB 0) 

 run b' 20000000

 
 type Time = float
 type 'a Behavior = Beh of (Time -> ('a * (unit -> 'a Behavior)))
 
 
 let atB (Beh bf) = bf

 let rec run b n  =
    match n with
    |0 -> 0
    |n -> let (r, nb) = atB b 0.0
          run (nb()) (n-1)


 let rec pureB f = Beh (fun t -> (f, fun() -> pureB f))

 let rec (<$>) (Beh f) (Beh baf) = 
    let rec bf (Beh f) (Beh baf) t = 
                    let (r,nb) = baf t
                    let (rf, nbf) = f t
                    (rf r, fun () -> Beh (bf (nbf()) (nb())))
    Beh (bf  (Beh f) (Beh baf))


 let rec oneB = Beh (fun _ -> (1, fun() -> oneB))

 let b' = (pureB (+)) <$> oneB  <$> oneB 

 run b' 20000000




 
 type Time = float
 type 'a Behavior = Beh of (Time -> ('a * Lazy<'a Behavior>))
 
 
 let atB (Beh bf) = bf

 let rec run b n  =
    match n with
    |0 -> 0
    |n -> let (r, nb) = atB b 0.0
          run (nb.Force()) (n-1)

 let rec oneB = Beh (fun _ -> (1, lazy(oneB)))


 run oneB 10000000

open Microsoft.Xna.Framework


type XnaTest() as this =
  inherit Game()


  let mutable graphics : GraphicsDeviceManager = null


  let mutable effect : Graphics.Effect = null


  let mutable vertexDeclaration : Graphics.VertexDeclaration = null


  let rand = System.Random()
  let n_verts = 3
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
    let gd = graphics.GraphicsDevice
    gd.VertexDeclaration <- vertexDeclaration
    gd.Clear Graphics.Color.Black
    for i=0 to n_verts - 1 do
      let x() = 2. * rand.NextDouble() - 1. |> float32
      vertex.[i].Position <- Vector3(x(), x(), x())
    effect.Begin()
    for pass in effect.CurrentTechnique.Passes do
      pass.Begin()
      let prim = PrimitiveType.TriangleList
      gd.DrawUserPrimitives(prim, vertex, 0, n_verts)
      pass.End()
    effect.End()

do
  use game = new XnaTest()
  game.Run()

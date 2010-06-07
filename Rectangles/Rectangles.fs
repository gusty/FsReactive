#light

namespace Rectangles

 module Main = 
  open System
  open FsReactive.Misc
  open FsReactive.FsReactive
  open FsReactive.Integration
  open FsReactive.Lib
  open Common.Vector
  open Xna.Main

  open Microsoft.Xna.Framework
  open Microsoft.Xna.Framework.Graphics
  open Microsoft.Xna.Framework.Input
  

  let (.*) a b = (pureB (*)) <.> a <.> b 
  let (.-) a b = (pureB (-)) <.> a <.> b 
  let (.+) a b = (pureB (+)) <.> a <.> b 
  let cosB = pureB Math.Cos
  let sinB = pureB Math.Sin
  let (.<=) a b = pureB (<=) <.> a <.> b
  let (.>=) a b = pureB (>=) <.> a <.> b
  
  type Rect = Rect of (Vector * Vector)

  type State =  { 
    rectangles : Rect list
    currentRec : Rect option
    }

    
        
  let rec mouseLeftButtonB = let rec  b = (Beh (fun _ ->( Mouse.GetState().LeftButton.Equals(ButtonState.Pressed), fun () -> b))) 
                             b |> memoB
  let rec mouseRightButtonB = let rec b = (Beh (fun _ -> ( Mouse.GetState().RightButton.Equals(ButtonState.Pressed), fun () -> b))) 
                              b |> memoB

  let mainGame (game:Game) = 

        let mousePos = mousePos game
        let rec mousePosEvt = Evt (fun _ -> (mousePos(), fun() -> mousePosEvt))
        let mousePosB  = stepB (0.0, 0.0) mousePosEvt |> memoB
        let mousePosXB = pureB fst <.> mousePosB
        let mousePosYB = pureB snd <.> mousePosB
        let rect = Rect (Vector (-0.5, -0.5), Vector (0.5, 0.5))
        let leftClickE() = whenE mouseLeftButtonB //|> memoE
        let rightClickE() = whenE mouseRightButtonB  //|> memoE
//        let rec rectB = untilB (noneB()) (leftClickE =>> (fun _ -> rectB' ))
        let leftClickE' = leftClickE()
        let rightClickE' = rightClickE() 
       
        let rec rectB = untilB (noneB()) (snapshotBehaviorOnlyE (leftClickE') mousePosB =>> (fun (x, y) -> mkRect x y ))
        and mkRect x y = 
                let movingRecB = (pureB  (fun (x, y) -> Rect (Vector (-x, -y), Vector (x, y))))
                                 <.> mousePosB
                untilB (someizeBf movingRecB) (((leftClickE') (*.|. (rightClickE()) *) ) =>> (fun _ -> rectB ))
//        let rectB' =  memoB (rectB |> tronB "rect = " )
        let rectB' =  memoB (rectB  )
        let stepProc rect rects = 
            match rect with
            |Some rect -> rect :: rects
            |None -> rects
        let newRectE = snapshotE    (rightClickE')  rectB' =>> (fun (_, rect) -> printf "%A \n" rect
                                                                                 stepProc rect)
        let rectsB = stepAccumB [] newRectE
        (pureB (fun rects rect -> 
                    { rectangles = rects
                      currentRec = rect
                    })) <.> rectsB <.> rectB'


  let drawRectangle (Rect ((Vector (x0, y0)), (Vector (x1, y1)))) (gd:GraphicsDevice) = 
        let n_verts = 5
        let random_vert _ = Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
        let vertex = Array.init n_verts random_vert
        vertex.[0].Position <- Vector3((float32)x0, (float32)y0, (float32) 0.0)
        vertex.[1].Position <- Vector3((float32)x0, (float32)y1, (float32) 0.0)
        vertex.[2].Position <- Vector3((float32)x1, (float32)y1, (float32) 0.0)
        vertex.[3].Position <- Vector3((float32)x1, (float32)y0, (float32) 0.0)
        vertex.[4].Position <- Vector3((float32)x0, (float32)y0, (float32) 0.0)
        gd.DrawUserPrimitives(PrimitiveType.LineStrip, vertex, 0, n_verts-1)


  let renderer state (gd:GraphicsDevice) = 
        match (state.currentRec, state.rectangles) with
        |(Some r), rects -> drawRectangle r gd 
                            List.iter (fun r -> drawRectangle r gd) rects
        |None, rects -> List.iter (fun r -> drawRectangle r gd) rects

  let renderedGame (game:Game) = 
        let stateB = mainGame game
        (pureB renderer) <.> stateB 

  do use game = new XnaTest2(renderedGame)
     game.Run()



        (*
        let mkVelocity t0 v0 accB hitE =
            let rec proc t0 v0 e0 = 
                let v0' = e0 t0 v0
                let v = integrate accB t0 v0'
                let fE = hitE --> (fun _ v -> -v)
                Disc (v, fE, proc)
            discontinuityE (proc t0 v0 (fun _ v -> -v))

        let rec sys t0 x0 vx0 mousePosXB = 
            let x' = aliasB x0
            let vx' = aliasB x0
            let hitE = whenE (condxf <.> (fst x'))
            let accxB =  pureB 1.0 .* (mousePosXB .- (fst x')) .- (pureB 0.05 .* (fst vx')) 
            let vx = bindAliasB (mkVelocity t0 vx0 accxB hitE) vx'
                            
            let x =  (bindAliasB (integrate vx t0 x0 ) x') 
            x 
        coupleB() <.> (sys 0.0 0.5 0.0  mousePosXB) <.> (sys 0.0 0.5 0.0 mousePosYB) // |>  tronB "x=" 

        *)
   (*     
    let mkMovement t0 x0 velocityB =
        let integrate = integrateGenB vectorNumClass
        let rec proc t0 x0 e0 = 
            let x0' = e0 t0 x0
            let x = integrate velocityB t0 x0'
            let boxE = (whileE ((pureB (inBoxPred >> not)) <.> x)) --> (fun _ x -> adaptToBox x)
            Disc (x, boxE, proc)
        discontinuityE (proc t0 x0 (fun _ x -> adaptToBox x))

 type Discontinuity<'a, 'b> = Disc of ('a Behavior *  (Time -> 'a -> 'b) Event * (Time -> 'a -> (Time -> 'a -> 'b) ->  Discontinuity<'a, 'b>))
    
 let rec discontinuityE (Disc (xB, predE, bg))  = 
        let evt = snapshotE predE ((coupleB() <.> timeB <.> xB))  =>>  
                        (fun (e,(t,vb)) -> let disc = bg t vb e
                                           discontinuityE disc)
        untilB xB evt

        *) 
#light

namespace Spring

 module Game = 
  open System
  open FReactive.Misc
  open FReactive.FReactive
  open FReactive.Integration
  open FReactive.Lib
  open Common.Random
  open Xna.Main

  open Microsoft.Xna.Framework
  open Microsoft.Xna.Framework.Graphics
  open Microsoft.Xna.Framework.Input
 
  let (.*) a b = (pureB (*)) <$> a <$> b 
  let (.-) a b = (pureB (-)) <$> a <$> b 
  let (.+) a b = (pureB (+)) <$> a <$> b 
  let cosB = pureB Math.Cos
  let sinB = pureB Math.Sin
  let (.<=) a b = pureB (<=) <$> a <$> b
  let (.>=) a b = pureB (>=) <$> a <$> b
  
  let mainGame (game:Game) = 
        let condxf = pureB (fun x -> x <= (-1.0) || x >= 1.0)
        let condyf = pureB (fun y -> y <= (-1.0) || y >= 1.0)
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
            let vx = bindAliasB (integrate accx t0 vx0 ) vx'
            let rec condxE = snapshotE (whenE (condxf <$> (fst x')))
                                       (pureB triple <$> timeB <$> (fst x') <$> vx)
                                =>> (fun (_, (t0, x0, vx0)) -> sys t0 x0 (-vx0) mousePosXB)
                            
            let x = switchB (bindAliasB (integrate vx t0 x0 ) x') condxE
           // let x = (bindAliasB (integrate vx t0 x0 ) x') 
            x 
        coupleB() <$> (sys 0.0 0.5 0.0  mousePosXB) <$> (sys 0.0 0.5 0.0 mousePosYB) |>  tronB "x=" 



  let renderer (x, y) (gd:GraphicsDevice) = 
        let n_verts = 2
        let random_vert _ = Graphics.VertexPositionColor(Vector3(0.f, 0.f, 0.f), Graphics.Color.White)
        let vertex = Array.init n_verts random_vert
        vertex.[1].Position <- Vector3((float32)x, (float32)y, (float32) 0.0)
        gd.DrawUserPrimitives(PrimitiveType.LineList, vertex, 0, n_verts/2)


  let renderedGame (game:Game) = 
        let stateB = mainGame game
        (pureB renderer) <$> stateB 

  do use game = new XnaTest2(renderedGame)
     game.Run() 
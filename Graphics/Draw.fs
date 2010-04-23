namespace Graphic

#light
open System.Windows.Forms
open Microsoft.FSharp.Control
open Microsoft.FSharp.Control

open Microsoft.FSharp.Control
open System.Drawing
open System.Threading
open System
open Picture

open FReactive.FReactive

 module Draw = 

  let createIDraw (e:Graphics) height =
//    let height = e.ClipBounds.Height
//    printf "%f \n" height
//    let width = e.ClipBounds.Width
    let drawPointF p c = ()
    let drawLineF (p1:float*float) (p2:float*float)  c =
        match (p1,p2,c) with
        | (x1,y1), (x2,y2), Color (r,g,b) ->
                    let nc = Color.FromArgb(r,g,b)
                    let pen = new Pen (nc)
                    let p1 = new System.Drawing.PointF ((float32) x1, height-(float32) y1)
                    let p2 = new System.Drawing.PointF ((float32) x2, height-(float32) y2)
                    e.DrawLine(pen, p1, p2)
    let drawEllipseF (c:float*float) (d:float*float) (a:float) (b:float) col =
        match (c, d, a, b, col) with
        | (xc,yc), (_,_), a_axis, b_axis, Color (r,g,b) ->
                    let nc = Color.FromArgb(r,g,b)
                    let brush = new SolidBrush(nc)
                    let p1 = new System.Drawing.PointF ((float32) (xc-a_axis/2.0),height-(float32) (yc+b_axis/2.0))
                    let sizeP = new System.Drawing.PointF ((float32) a_axis, (float32) b_axis)
                    let size = new System.Drawing.SizeF (sizeP)
                    let rect = new System.Drawing.RectangleF(p1, size)
                    e.FillEllipse(brush, rect)
    let drawPolygonF (ps:(float*float) list) col =
        match col with
        | Color (r,g,b) ->
                    let nc = Color.FromArgb(r,g,b)
                    let brush = new SolidBrush(nc)
                    let mapper = function (x,y) -> new System.Drawing.PointF ((float32)x,height-(float32)y)
                    let nps = List.to_array (List.map mapper ps)
                    e.FillPolygon(brush, nps)
    in
    { new IDraw
        with
           member s.drawPoint p col  = drawPointF p col
           member s.drawEllipse c d a b col = drawEllipseF c d a b col
           member s.drawLine p1 p2 col = drawLineF p1 p2 col
           member s.drawPolygon l col = drawPolygonF l col
    }


  type MyForm =
    inherit Form 
        new() = {}
        member x.init() = base.SetStyle(ControlStyles.AllPaintingInWmPaint |||
                                        ControlStyles.UserPaint |||
                                        ControlStyles.DoubleBuffer,true)
  
  
 
//form.ClientRectangle

//form.Show()
(*
let draw   = let time=ref 0.0
             let draw() =   //printf "tick %s \n" (any_to_string !time);
                            let pict = runOne pictB !time
                            let rect = form.ClientRectangle
                            let backBuffer=new Bitmap(rect.Width,rect.Height);
                            let g = Graphics.FromImage(backBuffer);
                            let idraw = createIDraw g (float32 rect.Height)
                            let scaleFactor = (float (Math.Min(rect.Width, rect.Height)))/200.0
                            let pict2 = (pict:>IShapeOp<Picture>).scale {x= 0.0; y=0.0} scaleFactor
                            let translation = {x= (float rect.Width)*0.5; y=(float rect.Height)*0.5}
                            let pict3 = (pict2:>IShapeOp<Picture>).translate translation
                            g.FillRectangle(new SolidBrush(Color.White), rect)
                            drawPicture idraw pict3;
                            g.Dispose();
                            let gg = form.CreateGraphics();
                            gg.DrawImageUnscaled(backBuffer,0,0);
                            gg.Dispose()
                            backBuffer.Dispose()
                            time := (!time) + 0.1;
                            //printf "after %s \n" (any_to_string pict);
             draw
*)
  let draw (form:Form) pictB = 
             let bf = ref pictB
             let draw t =   //printf "tick %s \n" (any_to_string !time);
                            let (pict, nb) = atB !bf t
                            bf := nb()
                            let rect = form.ClientRectangle
                            let backBuffer=new Bitmap(rect.Width,rect.Height);
                            let g = Graphics.FromImage(backBuffer);
                            let idraw = createIDraw g (float32 rect.Height)
                            let scaleFactor = (float (Math.Min(rect.Width, rect.Height)))/200.0
                            let pict2 = (pict:>IShapeOp<Picture>).scale {x= 0.0; y=0.0} scaleFactor
                            let translation = {x= (float rect.Width)*0.5; y=(float rect.Height)*0.5}
                            let pict3 = (pict2:>IShapeOp<Picture>).translate translation
                            g.FillRectangle(new SolidBrush(Color.White), rect)
                            drawPicture idraw pict3;
                            g.Dispose();
                            let gg = form.CreateGraphics();
                            gg.DrawImageUnscaled(backBuffer,0,0);
                            gg.Dispose()
                            backBuffer.Dispose()
                            //printf "after %s \n" (any_to_string pict);
             draw
              
  let run (form:Form) delay pictB = 
    let t0 = ref DateTime.Now
    let fDraw = draw form pictB
    let handler _ = 
        let now = DateTime.Now
        let t = (now - !t0).TotalSeconds
        t0 := now
        //printfn "timer"
        fDraw t 
    let timer = new System.Windows.Forms.Timer()
    timer.Interval <- delay
    timer.Tick.Add handler
    form.Closed.Add (fun _ -> printfn "Stopping timer"
                              timer.Stop()
                              timer.Dispose()
                              form.Dispose())
    form.Show();
    timer.Start()
    
       
        
 
           
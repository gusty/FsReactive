#light

namespace Graphic

open FReactive.FReactive
open Microsoft.FSharp.Math



module PictureBis =

 type Color = Color of int * int * int  // RGB

 type Location = {x:float; y:float; theta:float}
 with 
    static member make x y theta = {x=x; y=y; theta=theta}
    member s.translate dx dy = {x=s.x+dx; y=s.y+dy; theta=s.theta}
    member s.moveX dx = s.translate dx 0.0
    member s.moveY dy = s.translate 0.0 dy
    member s.rotate theta = {s with theta = s.theta+theta}
 
 type 'a Thing = Thing of (Location * 'a)
 with
    member s.location = (function (Thing (l,_)) -> l) s
    member s.thing = (function (Thing (_,s)) -> s) s


 let s = Thing ((Location.make 1.1 1.1 1.1),3)

 type Square = Square of float
 type Rectangle = Rectangle of float * float
 type Circle = Circle of float
 type Ellipse = Ellipse of (float * float)
 type Polygon = Polygon of (float * float) list

 type 'a Colored = Colored of (Color * 'a)

 type Shape =
    |R of Rectangle
    |S of Square
    |C of Circle
    |E of Ellipse
    |P of Polygon

let d = Circle 7.0



module Picture =
 
 
 type Color = Color of int * int * int  // RGB


 type IShapeOp<'a> = interface
        abstract translate : Vector -> 'a
        abstract rotate : Vector -> float -> 'a
        abstract scale : Vector -> float -> 'a
      end
 and  Vector = {x:float; y:float}
              with
                interface IShapeOp<Vector> with

                    member x.translate v =
                        match (x, v) with
                        |{x=x;y=y}, {x=dx;y=dy} -> {x=x+dx; y=y+dy}

                    member x.rotate p alpha =
                        match (x, p) with
                        |{x=x;y=y}, {x=cx;y=cy} ->  let dx = x-cx
                                                    let dy = y-cy
                                                    let nx = cx + dx * (System.Math.Cos alpha) - dy * (System.Math.Sin alpha)
                                                    let ny = cy + dx * (System.Math.Sin alpha) + dy * (System.Math.Cos alpha)   
                                                    {x=nx; y=ny}
                    member x.scale p f =match (x, p) with
                                        |{x=x;y=y}, {x=cx;y=cy}  -> let dx = (x-cx)*f
                                                                    let dy = (y-cy)*f
                                                                    {x=cx+dx; y=cy+dy}                
                end
              end

 type Point = Vector

 type Edge = {p1:Vector;  p2:Vector}
    with
        interface IShapeOp<Edge> with

            member x.translate v = match x with
                                   |{p1=p1; p2=p2} ->   let p1i = p1 :> IShapeOp<Vector>
                                                        let p2i = p2 :> IShapeOp<Vector>
                                                        {p1=p1i.translate(v); p2=p2i.translate(v)}


            member x.rotate p alpha =   match x with
                                        |{p1=p1; p2=p2} ->  let p1i = p1 :> IShapeOp<Vector>
                                                            let p2i = p2 :> IShapeOp<Vector>
                                                            {p1=p1i.rotate p alpha; p2=(p2i.rotate p alpha)}

            member x.scale p f =    match x with
                                    |{p1=p1; p2=p2} ->  let p1i = p1 :> IShapeOp<Vector>
                                                        let p2i = p2 :> IShapeOp<Vector>
                                                        {p1=p1i.scale p f; p2=(p2i.scale p f)}

        end
    end


 type Polygon = {vertices : Vector list}
    with
        interface IShapeOp<Polygon> with
            member x.translate v =
                let {vertices = l} = x
                let mapper x = (x:>IShapeOp<Vector>).translate v
                {vertices = List.map mapper l}

            member x.rotate p alpha =
                let {vertices = l} = x
                let mapper x = (x:>IShapeOp<Vector>).rotate p alpha
                {vertices = List.map mapper l}
 
             member x.scale p f =
                let {vertices = l} = x
                let mapper x = (x:>IShapeOp<Vector>).scale p f
                {vertices = List.map mapper l}
        end
    end
 
 type Ellipse = {centre:Vector; dir:Vector; a:float; b:float}
    with
        interface IShapeOp<Ellipse> with
            member x.translate v =
                let {centre=c; dir=d; a=a; b=b} = x
                {x with centre = ((c:>IShapeOp<Vector>).translate v)}

            member x.rotate p alpha =
                let {centre=c; dir=d; a=a; b=b} = x
                { x with centre = (c:>IShapeOp<Vector>).rotate p alpha;
                         dir = (d:>IShapeOp<Vector>).rotate {x=0.0;y=0.0}  alpha }
 
             member x.scale p f =
                let {centre=c; dir=d; a=a; b=b} = x
                { x with centre = (c:>IShapeOp<Vector>).scale p f;
                         dir = (d:>IShapeOp<Vector>).scale {x=0.0;y=0.0}  f; 
                         a = a*f;
                         b = b*f}
        end
    end

 type Circle = {centre:Vector; radius:float}
    with
        interface IShapeOp<Circle> with
            member x.translate v =
                let {centre=c} = x
                {x with centre = (c:>IShapeOp<Vector>).translate v }

            member x.rotate p alpha =
                let {centre=c} = x
                {x with centre = (c:>IShapeOp<Vector>).rotate p alpha }

            member x.scale p f =
                let {centre=c; radius=radius} = x
                {x with centre = (c:>IShapeOp<Vector>).scale p f;
                        radius = radius * f}
        end
    end



 type Shape =    Vector of Vector
                 | Edge of Edge
                 | Polygon of Polygon
                 | Ellipse of Ellipse
                 | Circle of Circle
                 with
                    interface IShapeOp< Shape > with

                        member x.translate v =
                            match x with
                            |Vector x -> Vector ((x:>IShapeOp<Vector>).translate v)
                            |Edge x -> Edge ((x:>IShapeOp<Edge>).translate v)
                            |Polygon x -> Polygon ((x:>IShapeOp<Polygon>).translate v)
                            |Ellipse x -> Ellipse ((x:>IShapeOp<Ellipse>).translate v)
                            |Circle x -> Circle ((x:>IShapeOp<Circle>).translate v)

                        member x.rotate p alpha =
                            match x with
                            |Vector x -> Vector ((x:>IShapeOp<Vector>).rotate p alpha)
                            |Edge x -> Edge ((x:>IShapeOp<Edge>).rotate p alpha)
                            |Polygon x -> Polygon ((x:>IShapeOp<Polygon>).rotate p alpha)
                            |Ellipse x -> Ellipse ((x:>IShapeOp<Ellipse>).rotate p alpha)
                            |Circle x -> Circle ((x:>IShapeOp<Circle>).rotate p alpha)

                        member x.scale p f =
                            match x with
                            |Vector x -> Vector ((x:>IShapeOp<Vector>).scale p f)
                            |Edge x -> Edge ((x:>IShapeOp<Edge>).scale p f)
                            |Polygon x -> Polygon ((x:>IShapeOp<Polygon>).scale p f)
                            |Ellipse x -> Ellipse ((x:>IShapeOp<Ellipse>).scale p f)
                            |Circle x -> Circle ((x:>IShapeOp<Circle>).scale p f)
                    end
                 end
    

//type Region<'a> when 'a :> IShapeOp<'a> = {shape:'a ; color:Color}
//type Over<'a, 'b> when 'a :> IShapeOp<'a> and 'b :> IShapeOp<'b> = {first:'a ; second:'b}
 type Picture = | EmptyPicture
                | ColoredPicture of Shape * Color
                | Over of Picture * Picture
    with
        interface IShapeOp< Picture > with
            member x.translate v =
                match x with
                |EmptyPicture -> EmptyPicture
                |ColoredPicture (shape, color) -> ColoredPicture ((shape:>IShapeOp<Shape>).translate v , color)
                |Over (p1, p2) -> Over ((p1:>IShapeOp<Picture>).translate v, (p2:>IShapeOp<Picture>).translate v)

            member x.rotate p alpha =
                match x with
                |EmptyPicture -> EmptyPicture
                |ColoredPicture (shape, color) -> ColoredPicture ((shape:>IShapeOp<Shape>).rotate p alpha , color)
                |Over (p1, p2) -> Over ((p1:>IShapeOp<Picture>).rotate p alpha, (p2:>IShapeOp<Picture>).rotate p alpha)

            member x.scale p f =
                match x with
                |EmptyPicture -> EmptyPicture
                |ColoredPicture (shape, color) -> ColoredPicture ((shape:>IShapeOp<Shape>).scale p f , color)
                |Over (p1, p2) -> Over ((p1:>IShapeOp<Picture>).scale p f, (p2:>IShapeOp<Picture>).scale p f)
        end
    end
   
   
   

 type IDraw = interface 
    abstract member drawPoint : (float * float) -> Color -> unit
    abstract member drawEllipse : (float * float) -> (float * float) -> float -> float -> Color -> unit
    abstract member drawLine : (float * float) -> (float * float)  -> Color-> unit
    abstract member drawPolygon : (float * float) list -> Color-> unit
 end   
               

 let drawShape (idraw:IDraw) (shape:Shape)  =
    match shape with
        |Vector {x=x; y=y} -> idraw.drawPoint (x,y)
        |Edge  {p1={x=xp1; y=yp1} ; p2={x=xp2; y=yp2}} ->  idraw.drawLine (xp1,yp1) (xp2, yp2)
        |Polygon {vertices = l} -> let rec proc l = match l with
                                                    |[] -> []
                                                    |{x=x;y=y}::t -> (x,y) :: proc t
                                   idraw.drawPolygon (proc l)
        |Ellipse  {centre={x=xc; y=yc} ; dir={x=xd; y=yd} ; a=a ; b=b} -> idraw.drawEllipse (xc, yc) (xd, yd) a b
        |Circle  {centre={x=xc; y=yc} ; radius=r} -> idraw.drawEllipse (xc, yc) (1.0, 0.0) r r

 let rec drawPicture (idraw:IDraw) (picture:Picture) =
    match picture with
    |EmptyPicture -> ()
    |ColoredPicture (shape, color) -> drawShape idraw shape color
    |Over (p1, p2) -> drawPicture idraw p2;
                      drawPicture idraw p1;
 



/// Interfaces is an example of object-oriented-style polymorphism

/// Example Symes et al., Expert F# 4.0 pp. 136: Define an abstract shape interface

/// We define an interface (abstract type) and 2 implementations
open System.Drawing
type IShape =
    abstract Contains : Point -> bool
    abstract BoundingBox : Rectangle

let circle(center : Point, radius : int) =
    { new IShape with
          member x.Contains(p : Point) =
              let dx = float32 (p.X - center.X)
              let dy = float32 (p.Y - center.Y)
              sqrt(dx * dx + dy * dy) <= float32 radius
          member x.BoundingBox =
              Rectangle(
                  center.X - radius, center.Y - radius,
                  2 * radius + 1, 2 * radius + 1)}

let square (center : Point, side : int) =
    { new IShape with
          member x.Contains(p : Point) =
              let dx = p.X - center.X
              let dy = p.Y - center.Y
              abs(dx) < side / 2 && abs(dy) < side / 2
          member x.BoundingBox =
              Rectangle(center.X - side, center.Y - side, side * 2, side * 2)}

/// We declare 2 values
let bigCircle = circle(Point(0, 0), 100)
bigCircle.BoundingBox
bigCircle.Contains(Point(70, 70))
bigCircle.Contains(Point(71, 71))

let smallSquare = square(Point(1, 1), 1)
smallSquare.BoundingBox
smallSquare.Contains(Point(0,0))

/// The 2 different implementations are collected into a single list
let shapeList = [bigCircle; smallSquare]

/// Actions on the IShape list need only know the interface
shapeList |> List.map (fun (s:IShape) -> s.BoundingBox)

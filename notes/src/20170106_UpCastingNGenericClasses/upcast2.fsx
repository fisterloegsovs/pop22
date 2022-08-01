[<AbstractClass>]
type shape() =
  abstract member area : unit -> float
  abstract member vertices : unit -> (float * float) list

type rectangle(tl, br) =
  inherit shape()
  member val topLeft = tl with get, set
  member val bottomRight = br with get, set
  override this.vertices () =
    let (x1,y1) = tl
    let (x2,y2) = br
    [tl; (x2, y1); br; (x1, y2)]
  override this.area () =
    let (x1,y1) = tl
    let (x2,y2) = br
    (x2-x1)*(y2-y1)
  override this.ToString () =
    sprintf "(%A, %A)" this.topLeft this.bottomRight

type circle(c,r) =
  inherit shape()
  member val center = c with get, set
  member val radius = r with get, set
  override this.vertices () =
    let t = [0.0 .. (2.0*System.Math.PI/(10.0*this.radius)) .. 2.0*System.Math.PI]
    let x = List.map cos t
    let y = List.map sin t
    List.zip x y
  override this.area () =
    2.0*System.Math.PI*this.radius**2.0
  override this.ToString () =
    sprintf "(%A, %A)" this.center this.radius

let aCircle = new circle((0.0,0.0), 1.0)
let aRectangle = new rectangle((0.0,0.0), (1.0,2.0))

let shapeLst = [aCircle :> shape; aRectangle :> shape]
List.iter (fun (s : shape) -> printfn "aShape: %A\n  %A" s (s.vertices ())) shapeLst

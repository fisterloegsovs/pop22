(*
   Initial reference solution by Mads Obitsø,
   made when the assignment was still in draft.
   Your mileage may vary.
*)

#r "nuget:DIKU.Canvas, 1.0.1"
open Canvas

let add (v1 : float * float) (v2 : float * float) : (float * float) =
    let x0,y0 = v1
    let x1,y1 = v2
    x0+x1, y0+y1

let mul (v1 : float * float) (scalar : float) : (float * float) =
    let x0,y0 = v1
    x0*scalar, y0*scalar

let dot (v1 : float * float) (v2 : float * float) : float  =
    let x0,y0 = v1
    let x1,y1 = v2
    x0*x1 + y0*y1

let rot (v : float * float) (angle : float) : (float * float) =
    let x,y = v
    let cosA = System.Math.Cos angle
    let sinA = System.Math.Sin angle
    let rotX = (x * cosA) - (y * sinA)
    let rotY = (x * sinA) + (y * cosA)
    rotX,rotY


let toInt (v : float * float) : int * int =
    let x,y = v
    int x, int y


let toFloat (v : int * int) : float * float =
    let x,y = v
    float x, float y
    
let setVector (canvas:canvas) (color:color) (v : int * int) (displacement : int * int) : unit =
    let vF = toFloat v
    let dF = toFloat displacement
    let vecToDraw = add vF dF |> toInt
    setLine canvas black displacement vecToDraw

let draw (width : int) (height : int) : canvas =
    let canvas = create width height
    let mid = (width / 2, height / 2)
    // let midF = toFloat mid
    
    let initialVec = (width/2, 0) |> toFloat
    let tau = System.Math.PI * 2.0
    for i = 0 to 35 do
        let vec = rot initialVec (tau / 36.0 * float i) |> toInt
        setVector canvas black vec mid
    canvas

// let c = draw 400 400
// show c "Wow spokey"

// Optional task
let drawExtended (width : int) (height : int) (state : 's) : canvas =
    let canvas = create width height
    let mid = (width / 2, height / 2)
    
    let initialVec = (width/2, 0) |> toFloat
    let tau = System.Math.PI * 2.0
    for i = 0 to 35 do
        let vec = rot initialVec ((tau / 36.0 * float i) + state) |> toInt
        setVector canvas black vec mid
    canvas

let react (state: 's) (k:key) : 's option =
    match getKey k with
        | LeftArrow  -> Some (state - 0.01)
        | RightArrow -> Some (state + 0.01)
        | _          -> None

runApp "Spokey" 600 600 drawExtended react (0.0)

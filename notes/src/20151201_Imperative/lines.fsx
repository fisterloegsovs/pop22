/// Open a window using winforms in Mono. The program opens a window
/// and draws a sequence of lines, and the lines are changed cyclicly
///
/// How to compile:
/// <code>
/// fsharpc lines.fsx
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/12/01

type element =
  | Empty
  | Line of (float*float)*(float*float)

let modulo n m = ((n % m) + m) % m

let getIndex (lines : element [], head : int, i : int) =
  modulo (head + i) lines.Length

let getFirstPoint = function
  | Line (p1, p2) -> p1
  | _ -> failwith "The point pair is empty"

let getSecondPoint = function
  | Line (p1, p2) -> p2
  | _ -> failwith "The point pair is empty"

let getPairOrRandom (lines : element [], head : int, aWidth : int, aHeight : int ) =
  let pair = (lines).[head]
  if pair = Empty then
    let rnd = System.Random ()
    (((float aWidth)*(rnd.NextDouble ()), (float aHeight)*(rnd.NextDouble ())), ((float aWidth)*(rnd.NextDouble ()), (float aHeight)*(rnd.NextDouble ())))
  else
    (getFirstPoint pair, getSecondPoint pair)

let pushLine (lines : element [] ref, head : int ref, line : (float*float)*(float*float)) =
  head := getIndex (!lines, !head, +1)
  (!lines).[!head] <- (Line line)

let normalize ((x,y) : float*float, newLen : float) =
  let len = sqrt (x**2.0 + y**2.0)
  (x*newLen/len, y*newLen/len)

let updatePoints (aWidth : int, aHeight : int, (x,y) : float*float, dir : float*float, newLength : float) =
  let mutable (dx,dy) = dir
  let rnd = System.Random ()
  while (x + dx < 0.0) || (x + dx > (float aWidth) - 1.0) || (y + dy < 0.0) || (y + dy > (float aHeight) - 1.0) do
    dx <- 2.0 * (rnd.NextDouble ()) - 1.0
    dy <- 2.0 * (rnd.NextDouble ()) - 1.0
    let (t1, t2) = normalize ((dx, dy), newLength)
    dx <- t1
    dy <- t2
  ((x + dx, y + dy), (dx, dy))

let display (aTitle : string, aWidth : int, aHeight : int, forgroundColor : System.Drawing.Color, nLines : int, stepSize : float, updateInterval : int) =

    let draw (g : System.Drawing.Graphics, lines : element [] ref, head : int ref, p1Direction : (float*float) ref, p2Direction : (float*float) ref) =
      let drawLines (g : System.Drawing.Graphics, lines : element [], head : int, aColor : System.Drawing.Color) = 
        let aPen = new System.Drawing.Pen (aColor)
        for i = 0 to lines.Length - 1 do
          let current = getIndex (lines, head, -i)
          let pair = lines.[current]
          if not (pair = Empty) then
            let (x1,y1) = getFirstPoint pair
            let (x2,y2) = getSecondPoint pair
            let pArray = [|System.Drawing.Point (int (round x1), int (round y1)); System.Drawing.Point (int (round x2), int (round y2))|]
            g.DrawLines (aPen, pArray)

      // get current line
      let (p1, p2) = getPairOrRandom (!lines, !head, aWidth, aHeight)
      // calculate a new line and add it to the front of the list
      let (t1, t1Dir) = updatePoints (aWidth, aHeight, p1, !p1Direction, stepSize)
      let (t2, t2Dir) = updatePoints (aWidth, aHeight, p2, !p2Direction, stepSize)
      pushLine (lines, head, (t1, t2))
      // directions change, when an end-point hits the window edge
      p1Direction := t1Dir
      p2Direction := t2Dir
      // draw all lines
      drawLines(g, !lines, !head, forgroundColor)

    let sz = new System.Drawing.Size (aWidth, aHeight)
    // setup a circular list
    let linesCircArray = ref (Array.init nLines (fun i -> Empty))
    let head = ref 0; // Points to the head
    // setup initial directions
    let p1Direction = ref (1.0, 1.0) 
    let p2Direction = ref (1.0, 0.0)

    p1Direction := normalize (!p1Direction, stepSize)
    p2Direction := normalize (!p2Direction, stepSize)

    // setup forms and timer
    let panel = new System.Windows.Forms.Panel (Dock = System.Windows.Forms.DockStyle.Fill)
    let timer = new System.Windows.Forms.Timer (Interval = updateInterval, Enabled = true)  
    let winSize = System.Drawing.Size (aWidth, aHeight)
    let win = new System.Windows.Forms.Form (Text = aTitle, ClientSize = winSize, MaximizeBox = false, MinimizeBox = false)

    // give control to winforms
    panel.Paint.Add (fun e -> draw (e.Graphics, linesCircArray, head, p1Direction, p2Direction))
    timer.Tick.Add (fun e -> panel.Refresh ())
    win.Controls.Add panel
    System.Windows.Forms.Application.Run win

let width = 400;
let height = 300;
let forgroundColor = System.Drawing.Color.Black
let nLines = 40 // Number of lines
let speed = 3.0
let updateInterval = 10 // milliseconds

display ("A bitmap", width, height, forgroundColor, nLines, speed, updateInterval)

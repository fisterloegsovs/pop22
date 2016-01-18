/// An example of model-view-control paradigm
/// Author: Jon Sporring.
/// Date: 2016/01/19

type IClient =
  abstract update : unit -> unit

/// A model containing 2 states: str, which is any string, and count,
/// which counts how many times the string has been updated
type model (aTitle) =
  let mutable title = aTitle
  let mutable theStr = ""
  let mutable theCounter = 0
  let mutable (clients : IClient list) = []
  member this.add c =
    clients <- c :: clients
  member this.update () =
    List.iter (fun (c : IClient) -> c.update ()) clients
  member this.str
    with get () = theStr
    and set aVal =
      printfn "\n%s - storing states and updating clients" title
      theStr <- aVal
      theCounter <- theCounter+1
      this.update ()
  member this.count with get () = theCounter

/// Monitor the model's str state
type normalView (aModel : model, aTitle) =
  let m = aModel
  let title = aTitle
  interface IClient with
    member this.update () =
      printfn "%s - Here is an update: \"%s\"" title m.str

/// Monitor the model's str state but process it before it is displayed
type reverseView (aModel : model, aTitle) =
  let m = aModel
  let title = aTitle
  interface IClient with
    member this.update () =
      printfn "%s - Here is an update: \"%s\"" title (System.String (Array.rev (m.str.ToCharArray ())))

/// Monitor the model's count state
type countView (aModel : model, aTitle) =
  let m = aModel
  let title = aTitle
  interface IClient with
    member this.update () =
      printfn "%s - Here is an update: %d" title m.count

/// Allow for changing the model's str state
type control (aModel : model, aTitle) =
  let m = aModel
  let title = aTitle
  interface IClient with
    member this.update () =
      printfn "%s - I don't care about updates" title
  member this.getInput () =
    printf "%s - Type some text: " title
    m.str <- System.Console.ReadLine ()
    
/// First we create objects for the model, views, and controls, and register them w.r.t. each other
let myModel = model ("myModel")
let view1 = normalView (myModel, "view1")
let view2 = reverseView (myModel, "view2")
let view3 = countView (myModel, "view3")
let control1 = control (myModel, "control1")
myModel.add (view1)
myModel.add (view2)
myModel.add (view3)
myModel.add (control1)

/// First we create objects for the model, views, and controls, and register them w.r.t. each other
while true do
  control1.getInput ()


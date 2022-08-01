let btnUp = new Button("Increment", Visible=true)
let btnDown = new Button("Decrement", Visible=true)
let lbl = new Label(Text=" Count: 0", Visible=true)

Event.merge
    (btnUp.Clicked |> Event.map (fun _ -> +1))
    (btnDown.Clicked |> Event.map (fun _ -> -1))
  |> Event.scan (+) 0
  |> Event.map (sprintf " Count: %d")
  |> Event.add (fun s -> lbl.Text <- s)

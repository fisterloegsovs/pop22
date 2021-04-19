type item = Pause of int | Subject of string * int | SubItems of string * item list
let itemTitle (m : item) : string =
  match m with
    Pause _ -> "Pause"
    | Subject (s,_) -> s
    | SubItems (s,_) -> s
 //
let aMeeting = SubItems ("Møde i kantineforeningen",[
    Subject ("Godkendelse af dagsorden", 5); 
    SubItems ("Indkøb af nye automater", [
        Subject ("Kravspecifikation", 15);
        Subject ("Valg af frivillig indkøber", 10)
    ])])
printfn "Dagens møde: %s" (itemTitle aMeeting)
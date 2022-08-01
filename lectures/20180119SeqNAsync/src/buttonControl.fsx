open System.Windows.Forms
open System.Drawing
open System

// Global states
let worker = new ComponentModel.BackgroundWorker()
let mutable run = false;

let s = Seq.initInfinite (fun i -> i+2)
let sieve n = Seq.filter (fun v -> v%n <> 0)
let primes = Seq.unfold (fun s -> let h = Seq.head s in Some(h, sieve h (Seq.tail s))) s

let mutable ithPrime = (0,2); // present prime number to display

let restartFct _ =
  run <- true;
  if not worker.IsBusy then worker.RunWorkerAsync()

let stopFct _ =
  run <- false

let workFct _ =
  ithPrime <- (1 + fst ithPrime, Seq.item (1+fst ithPrime) primes)

let text () =
  "prime(" + (string)(fst ithPrime) + ") = " + (string)(snd ithPrime)
  
// Create forms and controls
let win = new Form ()
let primeText = new Label()  
let restartButton = new Button ()
let quitButton = new Button ()

// Link forms and controls
win.Controls.Add primeText
win.Controls.Add restartButton
win.Controls.Add quitButton

// Set display details
win.ClientSize <- Size (200, 70)
primeText.Size <- Size (100, 30)
primeText.Location <- Point (10,0)
primeText.Anchor <- AnchorStyles.Top + AnchorStyles.Left
primeText.Text <- sprintf "No prime calculated"
restartButton.Size <- Size (75,30)
restartButton.Location <- Point (10, 35)
restartButton.Text <- "Start"
restartButton.Click.Add restartFct
quitButton.Size <- Size (75,30)
quitButton.Location <- Point (110, 35)
quitButton.Anchor <- AnchorStyles.Bottom + AnchorStyles.Right
quitButton.Text <- "Stop"
quitButton.Click.Add stopFct

// Link worker to window
worker.DoWork.Add workFct
worker.RunWorkerCompleted.Add (fun _ -> primeText.Text <- text (); if run then worker.RunWorkerAsync())

Application.Run win

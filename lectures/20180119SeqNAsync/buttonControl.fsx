open System.Windows.Forms
open System.Drawing
open System

// Global states
let worker = new ComponentModel.BackgroundWorker()
let mutable ithPrime = (0,2); // present prime number to display
let mutable run = false;

// Calculate primes using eratosthenes sieve
let workFct _ =
  let s = Seq.initInfinite (fun i -> i+2)
  let sieve n = Seq.filter (fun v -> v%n <> 0)
  let primes = Seq.unfold (fun s -> let h = Seq.head s in Some(h, sieve h (Seq.tail s))) s
  ithPrime <- (1 + fst ithPrime, Seq.item (1+fst ithPrime) primes)

let startFct _ =
  printfn "Starting"
  run <- true;
  if not worker.IsBusy then worker.RunWorkerAsync()

let stopFct _ =
  printfn "Stopping"
  run <- false

let text () =
  "prime(" + (string)(fst ithPrime) + ") = " + (string)(snd ithPrime)
  
// Create forms and controls
let win = new Form ()
let primeText = new Label()  
let startButton = new Button ()
let stopButton = new Button ()

// Link forms and controls
win.Controls.Add primeText
win.Controls.Add startButton
win.Controls.Add stopButton

// Set display details
win.ClientSize <- Size (200, 70)
primeText.Size <- Size (100, 30)
primeText.Location <- Point (10,0)
primeText.Anchor <- AnchorStyles.Top + AnchorStyles.Left
primeText.Text <- "No prime calculated"
let update () = 
  primeText.Text <- (string)run + ": prime(" + (string)(fst ithPrime) + ") = " + (string)(snd ithPrime)
startButton.Size <- Size (75,30)
startButton.Location <- Point (10, 35)
startButton.Text <- "Start"
startButton.Click.Add startFct
stopButton.Size <- Size (75,30)
stopButton.Location <- Point (110, 35)
stopButton.Anchor <- AnchorStyles.Bottom + AnchorStyles.Right
stopButton.Text <- "Stop"
stopButton.Click.Add stopFct

// Link worker to window
worker.DoWork.Add workFct
worker.RunWorkerCompleted.Add (fun _ -> update(); if run then worker.RunWorkerAsync())

Application.Run win

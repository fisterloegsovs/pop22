// Note: Winform libraries only available in mono32 (mac at least)
open System.Windows.Forms
open System.Drawing
open System

// Global states
let worker = new System.ComponentModel.BackgroundWorker()
let mutable run = false; // Number of asynchronous processes started
let mutable ithPrime = (0,2); // present prime number to display

// Calculate primes using eratosthenes sieve
let s = Seq.initInfinite (fun i -> i+2)
let sieve n = Seq.filter (fun v -> v%n <> 0)
let primes = Seq.unfold (fun s -> let h = Seq.head s in Some(h, sieve h (Seq.tail s))) s

let iterFct () =
  if run && not worker.IsBusy then
    worker.RunWorkerAsync()

let startFct _ =
  if not run then
    printfn "Starting"
    run <- true
  iterFct ()

let stopFct _ =
  if run then
    printfn "Stopping"
    run <- false

let workFct _ =
  ithPrime <- (1 + fst ithPrime, Seq.item (1+fst ithPrime) primes)

// Create forms and controls
let win = new Form ()
let vbox = new FlowLayoutPanel()
let hbox = new FlowLayoutPanel()
let primeText = new Label()  
let stateText = new Label()  
let startButton = new Button ()
let stopButton = new Button ()

// Link forms and controls
win.Controls.Add vbox
vbox.Controls.Add primeText
vbox.Controls.Add stateText
vbox.Controls.Add hbox
hbox.Controls.Add startButton
hbox.Controls.Add stopButton

// Set display details
win.ClientSize <- Size (200, 100)

let update () = 
  stateText.Text <- "State: " + (string)run
  primeText.Text <- "prime(" + (string)(fst ithPrime) + ") = " + (string)(snd ithPrime)
update ()
startButton.Text <- "Start"
stopButton.Text <- "Stop"

startButton.Click.Add startFct
stopButton.Click.Add stopFct


worker.DoWork.Add workFct
worker.RunWorkerCompleted.Add (fun _ -> update (); if run then iterFct ())

Application.Run win

// Something is up with fsharp, forms and backgroundworker:
// If this the following sequence is used: 
//   start-stop-start-stop-stop-start-stop-stop-stop
// then the program outputs:
// Starting (compCount=0)
// Stopping (compCount=1)
// Starting (compCount=0)
// Stopping (compCount=1)
// Stopping (compCount=1)
// Starting (compCount=0)
// Stopping (compCount=1)
// Stopping (compCount=2)
// Stopping (compCount=1)

open System.Windows.Forms
open System.Drawing
open System

// Global states
let worker = new ComponentModel.BackgroundWorker()
worker.WorkerSupportsCancellation <- true
let mutable ithPrime = (0,2); // present prime number to display
let compCount = ref 0; // Number of asynchronous processes started

// Calculate primes using eratosthenes sieve
let workFct (update : unit -> unit) _ =
  let s = Seq.initInfinite (fun i -> i+2)
  let sieve n = Seq.filter (fun v -> v%n <> 0)
  let primes = Seq.unfold (fun s -> let h = Seq.head s in Some(h, sieve h (Seq.tail s))) s
  lock compCount (fun () -> compCount := !compCount + 1)
  while not worker.CancellationPending do
    lock ithPrime (fun () -> ithPrime <- (1 + fst ithPrime, Seq.item (1+fst ithPrime) primes))
    update ()

let startFct update _ =
  if !compCount = 0 && not worker.IsBusy then 
    printfn "Starting (compCount=%d)" !compCount
    worker.DoWork.Add (workFct update)
    worker.RunWorkerAsync ()

let stopFct _ =
  if !compCount > 0 then
    printfn "Stopping (compCount=%d)" !compCount
    lock compCount (fun () -> compCount := !compCount - 1)
    worker.CancelAsync ()

// Create forms and controls
let win = new Form ()
let primeText = new Label()  
let stateText = new Label()  
let startButton = new Button ()
let stopButton = new Button ()

// Link forms and controls
win.Controls.Add primeText
win.Controls.Add stateText
win.Controls.Add startButton
win.Controls.Add stopButton

// Set display details
win.ClientSize <- Size (200, 90)
primeText.Size <- Size (100, 20)
stateText.Size <- Size (100, 20)
startButton.Size <- Size (75,30)
stopButton.Size <- Size (75,30)

primeText.Location <- Point (10,10)
stateText.Location <- Point (10,30)
startButton.Location <- Point (10, 50)
stopButton.Location <- Point (110, 50)

let update () = 
  stateText.Text <- "State: " + (string)!compCount
  primeText.Text <- "prime(" + (string)(fst ithPrime) + ") = " + (string)(snd ithPrime)
update ()
startButton.Text <- "Start"
stopButton.Text <- "Stop"

startButton.Click.Add (startFct update)
stopButton.Click.Add stopFct

Application.Run win

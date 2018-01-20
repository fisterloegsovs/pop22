open System.Windows.Forms
open System.Drawing
open System

// Global states
let mutable cancellationSource = new System.Threading.CancellationTokenSource() // placeholder, not used
let mutable ithPrime = (0,2); // present prime number to display
let mutable compCount = 0; // Number of asynchronous processes started

// Calculate primes using eratosthenes sieve
let comp (update : unit -> unit) = async {
    printfn "Starting"
    use! holder = Async.OnCancel(fun () ->     
        printfn "Stopping"
        compCount <- compCount - 1)
    compCount <- compCount + 1
    let s = Seq.initInfinite (fun i -> i+2)
    let sieve n = Seq.filter (fun v -> v%n <> 0)
    let primes = Seq.unfold (fun s -> let h = Seq.head s in Some(h, sieve h (Seq.tail s))) s
    while true do
        ithPrime <- (1 + fst ithPrime, Seq.item (1+fst ithPrime) primes)
        update()
        do! Control.Async.SwitchToThreadPool () // Allow for other async processes and cancel
}

let startFct (update : unit -> unit) _ =  
    if compCount = 0 then 
        cancellationSource <- new System.Threading.CancellationTokenSource()
        Control.Async.Start (comp update, cancellationSource.Token)

let stopFct _ =
    if compCount > 0 then 
        cancellationSource.Cancel()
        cancellationSource.Dispose()

// Create forms and controls
let win = new Form ()
let primeText = new Label ()
let stateText = new Label ()
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
  stateText.Text <- "State: " + (string)compCount
  primeText.Text <- "prime(" + (string)(fst ithPrime) + ") = " + (string)(snd ithPrime)
update ()
startButton.Text <- "Start"
stopButton.Text <- "Stop"

startButton.Click.Add (startFct update)
stopButton.Click.Add stopFct

Application.Run win

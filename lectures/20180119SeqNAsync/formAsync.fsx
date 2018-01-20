// Note: Winform libraries only available in mono32 (mac at least)
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
  stateText.Text <- "State: " + (string)compCount
  primeText.Text <- "prime(" + (string)(fst ithPrime) + ") = " + (string)(snd ithPrime)
update ()
startButton.Text <- "Start"
stopButton.Text <- "Stop"

startButton.Click.Add (startFct update)
stopButton.Click.Add stopFct

Application.Run win

let cancellationSource = new System.Threading.CancellationTokenSource()

let comp = async {
  for i = 1 to 100 do
    printfn "%d" i;
    do! Control.Async.Sleep 100;
  }

Control.Async.Start (comp, cancellationSource.Token) // allow cancellation
System.Threading.Thread.Sleep(200) // do nothing in 200ms
cancellationSource.Cancel() // cancel comp

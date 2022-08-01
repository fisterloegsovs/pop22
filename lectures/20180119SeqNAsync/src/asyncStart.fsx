let comp = async {
  for i = 1 to 100 do
    printfn "%d" i;
    do! Control.Async.Sleep 100;
  };;

Control.Async.RunSynchronously comp;; // await result
Control.Async.Start comp;; // do not await result

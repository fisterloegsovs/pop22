let rec getAFileName () =
  printfn "%A" (System.IO.Directory.GetFiles ".")
  printf "Enter a filename: "
  let filename = System.Console.ReadLine ()
  if System.IO.File.Exists filename then
    filename
  else
    getAFileName ()

let filename = getAFileName ()
printfn "The user typed %A" filename

let getAFileName () =
  let mutable filename = Unchecked.defaultof<string>
  let mutable fileExists = false
  while not(fileExists) do
    printfn "%A" (System.IO.Directory.GetFiles ".")
    printf "Enter a filename: "
    filename <- System.Console.ReadLine()
    fileExists <- System.IO.File.Exists filename
  filename

let filename = getAFileName ()
printfn "You typed: %s" filename

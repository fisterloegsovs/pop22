let rec documentFileName () =
        System.Console.Write("Filename: ")
        let filename = System.Console.ReadLine()
        if not(System.IO.File.Exists filename) then
            System.Console.WriteLine("File does not exist")
            documentFileName()
        else filename

let x = System.IO.Directory.GetFiles(".")
printfn "Directory contains: %A" x
documentFileName () |> ignore

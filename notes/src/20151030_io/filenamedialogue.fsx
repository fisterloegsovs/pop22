/// A demonstration of filename dialogue with user.
///
/// How to compile: <code>fsharpc --doc:filenamedialogue.xml filenamedialogue.fsx</code>
///
/// Author: Jon Sporring.
/// Date: 2015/10/27

/// <summary>Dialogue for asking the user to input the name of an existing file.</summary>
/// <returns>A valid filename.</returns>
let rec documentFileName () =
        System.Console.Write("Filename: ")
        let filename = System.Console.ReadLine()
        if not(System.IO.File.Exists filename) then
            System.Console.WriteLine("File does not exist")
            documentFileName()
        else filename

/// Print the content of the current directory, and ask the user to input an existing filename.
let x = System.IO.Directory.GetFiles(".")
printfn "Directory contains: %A" x
documentFileName () |> ignore

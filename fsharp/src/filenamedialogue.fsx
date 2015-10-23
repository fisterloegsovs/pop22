let x = System.IO.Directory.GetFiles(".");;

let rec documentFileName () =
        System.Console.Write("Filename: ")
        let filename = System.Console.ReadLine()
        if not(File.Exists filename) then
            System.Console.WriteLine("File does not exist")
            documentPath()
        else filename

module CatStreaming
open System.IO

let readBytes (count:int) (buffer:byte[]) (fs:FileStream) : int =
    fs.Read(buffer, 0, count)

let writeBytes (count:int) (buffer:byte[]) (fs:FileStream) : unit =
    fs.Write(buffer, 0, count)

let readAndWriteBytes (buffersize:int) (buffer:byte[]) (ifs:FileStream) (ofs:FileStream) =
    // Recursion seems a bit stupid when the argument is unit
    // but hey, it works and makes me feel smart
    let rec inner () =
        let readBytes = readBytes buffersize buffer ifs
        match readBytes with
            | 0 -> ofs.Flush()
            | _ ->
                writeBytes readBytes buffer ofs |> inner
    inner ()

let openFileRead (filename:string) : FileStream =
    try
        File.OpenRead filename
    with
        _ -> raise (System.IO.FileNotFoundException())

let openFilesRead (filenames : string list) : FileStream option list =
    List.map (fun filename -> try openFileRead filename |> Some with _ -> None) filenames

let openFileWrite (filename:string) : FileStream option =
    try
        File.Open(filename, FileMode.Create) |> Some
    with
        _ ->
            sprintf "cat: Could not create or truncate file \"%s\"" filename
            |> System.Console.Error.WriteLine
            None

let countAndOutputErrors (errors:int) (filename:string) (filestream:FileStream Option) : int =
    match filestream with
        | None ->
            sprintf "cat: The file \"%s\" does not exist or is not readable" filename
            |> System.Console.Error.WriteLine
            errors+1
        | Some _ -> errors

let catWithBufferSize (buffersize:int) (filenames:string[]) : int = 
    match Array.length filenames with
        | 0 -> System.Console.Error.WriteLine "cat: no output file"; 255
        | 1 ->
            // Create an empty file with filenames.[0]
            match openFileWrite filenames.[0] with
                | None -> 1
                | Some ofs -> writeBytes 0 [||] ofs; 0
        | len ->
            // Create the buffer we use
            let buffer = Array.init buffersize (fun _ -> 0uy)
            // Grab all the input filenames and the output filename
            let infiles = filenames.[..len-2] |> List.ofArray
            let outfile = filenames.[len-1]
            // Open all the input files, obtaining a FileStream for each
            let inputFileStreams = openFilesRead infiles
            let outputFileStream = openFileWrite outfile
            if List.exists ((=) None) inputFileStreams then
                List.fold2 countAndOutputErrors 0 infiles inputFileStreams
            else
                match outputFileStream with
                // Something went wrong when opening the output file.
                // Error has already been reported on stderr, so just exit with exit status 1
                | None -> 1
                | Some ofs ->
                    // For each input filestream
                    // while filestream has bytes
                    //     read n bytes from filestream into buffer, where 0 <= n <= buffersize
                    //     write n bytes to outputFileStream                    
                    List.map Option.get inputFileStreams
                    |> List.iter (fun ifs -> readAndWriteBytes buffersize buffer ifs ofs)
                    0 // exit status
            

let cat = catWithBufferSize 32

let rec askForProgrammingLanguage () =
    printfn "Please enter the name of a programming language:"
    let a = System.Console.ReadLine()
    match a with
        // If the user enters quit we exit the recursive loop and end the program
        | "quit" -> ()
        // If the user enters fsharp, we tell them it's cool and recursively call
        // to ask for another input
        | "fsharp" ->
            printfn "fsharp is cool"
            askForProgrammingLanguage ()
        | _ ->
            // We only recognize F# as a cool language
            // Tell the user we know nothing Jon Snow and
            // recursively ask for another language
            printfn "I don't know %A" a
            askForProgrammingLanguage ()

askForProgrammingLanguage ()

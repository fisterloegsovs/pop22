(*
    F# solution to OOPD course assignment 2
*)


exception InvalidInputParameter

type TicTacToe(?verbose0) =

    // Default value for verbose parameter
    // verbose controls if a select set of methods report to terminal
    let verbose = defaultArg verbose0 false
    let mutable moves = 0

    let mutable board = [|[|' ';' ';' '|];
                          [|' ';' ';' '|];
                          [|' ';' ';' '|]|]

    member this.newBoard =
        moves <- 0
        board <- [|[|' ';' ';' '|];
                   [|' ';' ';' '|];
                   [|' ';' ';' '|]|]
        if (verbose) then printfn "Resetting game\n"

    member this.printBoard =
        if (verbose) then
            if (this.isGameOver) then printfn "Game Over"
            else printfn "Game still on with %d move(s) left" (9-moves)
        printfn "#---#"
        printfn "|%s|" (System.String.Concat(board.[0]))
        printfn "|%s|" (System.String.Concat(board.[1]))
        printfn "|%s|" (System.String.Concat(board.[2]))
        printfn "#---#\n"

    member this.get row col =
        if (row >= 0 && row < 3 && col >= 0 && col < 3) then
            board.[row].[col]
        else
            raise InvalidInputParameter

    member this.set(row, col, value) =
        if (row >= 0 && row < 3 && col >= 0 && col < 3 && (value = 'o' || value = 'x')) then
            if (board.[row].[col] = ' ') then
                board.[row].[col] <- value
                moves <- moves + 1
            else
                raise InvalidInputParameter
        else
            raise InvalidInputParameter

    member this.isGameOver =
        if (moves = 9) then true
        else
            let mutable isOver = false

            // Check row and cols for win criteria
            for i in 0 .. 2 do
                isOver <- isOver || ((board.[i].[0] = 'o' || board.[i].[0] = 'x') && board.[i].[0] = board.[i].[1] && board.[i].[1] = board.[i].[2])
                isOver <- isOver || ((board.[0].[i] = 'o' || board.[0].[i] = 'x') && board.[0].[i] = board.[1].[i] && board.[1].[i] = board.[2].[i])

            // check two diagonals for win criteria
            isOver <- isOver || ((board.[0].[0] = 'o' || board.[0].[0] = 'x') && board.[0].[0] = board.[1].[1] && board.[1].[1] = board.[2].[2])
            isOver <- isOver || ((board.[2].[0] = 'o' || board.[2].[0] = 'x') && board.[2].[0] = board.[1].[1] && board.[1].[1] = board.[0].[2])
            isOver


[<EntryPoint>]
let main _ =
    let ttt = new TicTacToe(true)
    ttt.set(1,1,'x')
    ttt.printBoard
    ttt.set(0,0,'o')
    ttt.printBoard
    ttt.set(2,0,'x')
    ttt.printBoard
    ttt.set(0,2,'o')
    ttt.printBoard
    ttt.set(0,1,'x')
    ttt.printBoard
    ttt.set(2,1,'o')
    ttt.printBoard
    ttt.set(1,2,'x')
    ttt.printBoard
    ttt.set(1,0,'o')
    ttt.printBoard
    ttt.set(2,2,'x')
    ttt.printBoard
    ttt.newBoard
    ttt.set(0,0,'x')
    ttt.set(1,1,'x')
    ttt.printBoard
    ttt.set(2,2,'x')
    ttt.printBoard
    ttt.newBoard
    ttt.set(0,1,'x')
    ttt.set(1,1,'x')
    ttt.printBoard
    ttt.set(2,1,'x')
    ttt.printBoard
    ttt.newBoard
    ttt.set(0,0,'o')
    ttt.set(0,1,'o')
    ttt.printBoard
    ttt.set(0,2,'o')
    ttt.printBoard
    0
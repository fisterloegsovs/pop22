#r "nuget:DIKU.Canvas"
open Canvas

let rnd = System.Random()

type Piece = Red | Green | Blue | Purple | Yellow

type Board = (Piece option)[,]
type GameState =
    | GameOver
    | GameRunning of Board


let rec initBoard (board:Board) n =
    match n with
        | 0 -> ()
        | _ ->
            let x = rnd.Next 4
            let y = rnd.Next 4
            match board.[x,y] with
                | None ->
                    board.[x,y] <- Some Red
                    initBoard board (n - 1)
                | Some piece -> initBoard board n


let nextPiece = function
    | Red -> Green
    | Green -> Blue
    | Blue -> Purple
    | Purple -> Yellow
    | Yellow -> Yellow

// Intended to use when moving a piece into an empty place
let swap (board:Board) (p1 : int * int) (p2 : int * int) =
    let p1x,p1y = p1
    let p2x,p2y = p2
    let tmp = board.[p2x,p2y]
    board.[p2x,p2y] <- board.[p1x,p1y]
    board.[p1x,p1y] <- tmp

// Merge two pieces
// Removes p2
let merge (board:Board) (p1 : int * int) (p2 : int * int) =
    let p1x,p1y = p1
    let p2x,p2y = p2
    let piece = match (board.[p1x,p1y]) with
        | Some p -> p
        | None -> failwith "No piece existed"
    let newPiece = nextPiece piece
    board.[p2x,p2y] <- None
    board.[p1x,p1y] <- Some newPiece


let rec findUpNeighbor (board:Board) col row =
    printfn "col: %A, row: %A" col row
    if (row > 3 || row < 1) then None
    else match board.[col,row-1] with
        | None -> findUpNeighbor board col (row-1)
        | Some p -> Some (col, row-1)

let rec findDownNeighbor (board:Board) col row =
    printfn "col: %A, row: %A" col row
    if (row >= 3 || row < 0) then None
    else match board.[col,row+1] with
        | None -> findDownNeighbor board col (row+1)
        | Some p -> Some (col, row+1)


let rec findRightNeighbor (board:Board) col row =
    printfn "col: %A, row: %A" col row
    if (col >= 3 || col < 0) then None
    else match board.[col+1,row] with
        | None -> findRightNeighbor board (col+1) row
        | Some p -> Some (col+1, row)

let rec findLeftNeighbor (board:Board) col row =
    printfn "col: %A, row: %A" col row
    if (col > 3 || col < 1) then None
    else match board.[col-1,row] with
        | None -> findLeftNeighbor board (col-1) row
        | Some p -> Some (col-1, row)

let moveLeft (board:Board) =
    let moveLeft' (board:Board) col row =
        // Start from the left side
        // We have 3 cases
        // 1. None with right-neighbor = None ; we do nothing
        match board.[col,row] with
            | None ->
                match findRightNeighbor board col row with
                    | None -> ()
                    // 2. None with right-neighbor = Some p ; we swap
                    | Some neighbor -> swap board (col,row) neighbor
        // 3. Some p
            | Some p ->
                // Do we have a neighbor we can merge? 
                match findRightNeighbor board col row with
                    | None -> () // Nope
                    | Some (ncol,nrow) ->
                        if (board.[col,row] = board.[ncol,nrow]) then
                            merge  board (col,row) (ncol,nrow)
                        else ()

    for col in [0;1;2;3] do
        for row in [0;1;2;3] do
            moveLeft' board col row
            moveLeft' board col row            



let moveRight (board:Board) =
    let moveRight' (board:Board) col row =
        // Start from the left side
        // We have 3 cases
        // 1. None with right-neighbor = None ; we do nothing
        match board.[col,row] with
            | None ->
                match findLeftNeighbor board col row with
                    | None -> ()
                    // 2. None with right-neighbor = Some p ; we swap
                    | Some neighbor -> swap board (col,row) neighbor
        // 3. Some p
            | Some p ->
                // Do we have a neighbor we can merge? 
                match findLeftNeighbor board col row with
                    | None -> () // Nope
                    | Some (ncol,nrow) ->
                        if (board.[col,row] = board.[ncol,nrow]) then
                            merge  board (col,row) (ncol,nrow)
                        else ()

    for col in [3;2;1;0] do
        for row in [0;1;2;3] do
            moveRight' board col row
            moveRight' board col row            


let moveUp (board:Board) =
    let moveUp' (board:Board) col row =
        // Start from the left side
        // We have 3 cases
        // 1. None with right-neighbor = None ; we do nothing
        match board.[col,row] with
            | None ->
                match findDownNeighbor board col row with
                    | None -> ()
                    // 2. None with right-neighbor = Some p ; we swap
                    | Some neighbor -> swap board (col,row) neighbor
        // 3. Some p
            | Some p ->
                // Do we have a neighbor we can merge? 
                match findDownNeighbor board col row with
                    | None -> () // Nope
                    | Some (ncol,nrow) ->
                        if (board.[col,row] = board.[ncol,nrow]) then
                            merge  board (col,row) (ncol,nrow)
                        else ()

    for row in [0;1;2;3] do
        for col in [0;1;2;3] do
            moveUp' board col row
            moveUp' board col row            

let moveDown (board:Board) =
    let moveDown' (board:Board) col row =
        // Start from the left side
        // We have 3 cases
        // 1. None with right-neighbor = None ; we do nothing
        match board.[col,row] with
            | None ->
                match findUpNeighbor board col row with
                    | None -> ()
                    // 2. None with right-neighbor = Some p ; we swap
                    | Some neighbor -> swap board (col,row) neighbor
        // 3. Some p
            | Some p ->
                // Do we have a neighbor we can merge? 
                match findUpNeighbor board col row with
                    | None -> () // Nope
                    | Some (ncol,nrow) ->
                        if (board.[col,row] = board.[ncol,nrow]) then
                            merge  board (col,row) (ncol,nrow)
                        else ()

    for row in [3;2;1;0] do
        for col in [0;1;2;3] do
            moveDown' board col row
            moveDown' board col row            

let getColor = function
    | Red -> red
    | Green -> green
    | Blue -> blue
    | Purple -> fromRgb (220,160,220)
    | Yellow -> fromRgb (250, 200, 175)
    | _ -> red

let draw w h (s:GameState) =
    let b = match s with
        | GameRunning b -> b
        | GameOver -> failwith "Game is over"
    let fourth = w / 4    
    let grid bm c =
        do setFillBox bm c (0,0) (5,h)
        do setFillBox bm c (0,0) (w,5)
        do setFillBox bm c (w-5, 0) (w,h)
        do setFillBox bm c (0,h-5) (w,h)

        for i in [1;2;3] do
            let x = i * fourth
            do setFillBox bm c (x,0) (x+5,h)
            do setFillBox bm c (0,x) (w,x+5)
    let bm = create w h
    // Draw the tiles
    Array2D.iteri (fun x y v ->
                   match v with
                     | None -> ()
                     | Some c ->
                             do setFillBox bm (getColor c) (x * fourth, y * fourth) ((x+1)*fourth, (y+1)*fourth)) b
    // Overlay a grid
    grid bm black
    bm



let react (s:GameState) k =
    match s with
        | GameOver -> None
        | GameRunning board ->
            match getKey k with
                | LeftArrow ->
                    moveLeft board
                    initBoard board 1
                    Some (GameRunning board)
                | RightArrow ->
                    moveRight board
                    initBoard board 1                    
                    Some (GameRunning board)
                | DownArrow ->
                    moveDown board
                    initBoard board 1                    
                    Some (GameRunning board)
                | UpArrow ->
                    moveUp board
                    initBoard board 1                    
                    Some (GameRunning board)
                // | Space ->
                //     initBoard board 1
                //     Some (GameRunning board)
                | _ -> None
        


// Main game board is Array2D of Piece option
let board : Board = Array2D.init 4 4 (fun i j -> None)

initBoard board 2
printf "%A" board

do runApp "2048" 800 800 draw react (GameRunning (board))

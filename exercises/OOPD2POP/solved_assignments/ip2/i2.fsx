(*
    F# solution to IP course individual assignment 2
*)

#load "uge2_kollegier.fsx"

exception Empty

let sqrt = System.Math.Sqrt

let min x y =
    if x < y then x
    else y

let distance (x0, y0) (x1, y1) = sqrt((x0-x1)*(x0-x1) + (y0-y1)*(y0-y1))

let rec closestDistance p (qs : List<_>) =
    if (qs.Length = 0) then raise Empty
    let d = (distance p (qs.Item(0)))
    let rec notALoop = function
        | []       -> d
        | pp::qss  -> min (distance p pp) (notALoop qss)
    notALoop qs

let rec closestPOI p (qs : List<_>) =
    if (qs.Length = 0) then raise Empty
    let rec notALoop2 best (sublist : List<_>) =
        if (sublist.Length = 0) then best
        else
            let x::xs = sublist
            let (xP,_) = x
            let (bP,_) = best
            if ((distance p (bP)) > (distance p (xP))) then (notALoop2 x xs)
            else (notALoop2 best xs)
    let (_, result) = (notALoop2 (qs.Item(0)) qs)
    result

[<EntryPoint>]
let main _ =
    printfn "Result: %f" (closestDistance (0.0, 0.0) [(1.0, 2.0);(2.0, 1.0);(-2.0, 2.0);(1.0, 1.0)])
    printfn "Result: %s" (closestPOI (1000.0,1000.0) Uge2Kol.kollegier)
    0
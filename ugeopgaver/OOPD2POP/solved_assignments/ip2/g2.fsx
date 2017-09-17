(*
    F# solution to IP course group assignment 2
*)

#load "uge2_distance.fsx"
#load "uge2_cphmarathon2014.fsx"
#load "uge2_dhl2014.fsx"

let rec totalDistance (rute : List<_>) =
    if (rute.Length < 2) then 0.0
    else
        let p::ps = rute
        (Uge2Dist.distance p (ps.Item(0))) + (totalDistance ps)

let rec speeds (rute : List<_>) =
    if (rute.Length < 2) then []
    else
        let p::ps = rute
        let (p1, t1) = p
        let (p2, t2) = ps.Item(0)
        (1000.0*60.0*60.0*(Uge2Dist.distance p1 p2)/float(t2-t1))::speeds(ps)

exception Empty

let rec averageAllHelper (liste : List<_>) =
    if (liste = []) then (0.0, 0)
    else
        let x::xs = liste
        let (a, b) = averageAllHelper xs
        (x + float(a), b + 1)

let averageAll (liste : List<_>) =
    if (liste = []) then raise Empty
    else
        let (a, b) = averageAllHelper liste
        a / float(b)

let averageSpeed (rute : List<_>) = averageAll (speeds rute)

[<EntryPoint>]
let main _ =
    let storebaelt = [((55.336145, 10.990714), 0);((55.349420, 11.095690), 1500000);((55.336145, 10.990714), 3600000)];
    printfn "Total maratonlængde: %f" (totalDistance Uge2Mart.cphMarathon2014)
    printfn "time(0) = %f, time(1) = %f" ((speeds storebaelt).Item(0)) ((speeds storebaelt).Item(1))
    printfn "average of [0.0;2.0;3.0] = %f" (averageAll [0.0;2.0;3.0;1.0])
    printfn "dhl average speed = %f" (averageSpeed Uge2Dhl.dhl2014)
    printfn "storebælt average speed = %f" (averageSpeed storebaelt)
    0
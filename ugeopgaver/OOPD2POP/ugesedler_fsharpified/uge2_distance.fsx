module Uge2Dist

(* Is the latitude and longitude of a coordinate within bounds. *)

let coordinateOK lati longi = abs(lati) <= 90.0 && abs(longi) <= 180.0

(* Distance in kilometers on the surface of earth between two
  geographical coordinates.
 
  Both coordinates are given as pair of a latitude and a longitude
  represented as real numbers.
 
  distance : (real * real) * (real * real) -> real
 *)

exception Domain

let sin = System.Math.Sin
let cos = System.Math.Cos
let atan2 = System.Math.Atan2
let sqrt = System.Math.Sqrt

let distance (lati0, longi0) (lati1, longi1) =
    if not ((coordinateOK lati0 longi0) && (coordinateOK lati1 longi1))
    then raise Domain
    else 
        let deg2rad(deg) = deg * (System.Math.PI/180.0)
        let earth_radius = 6371.0
        let dlati = deg2rad(lati1-lati0)
        let dlongi = deg2rad(longi1-longi0)
        let a = sin(dlati / 2.0) * sin(dlati / 2.0) +
                cos(deg2rad lati0) * cos (deg2rad lati1) *
                sin(dlongi / 2.0) * sin(dlongi / 2.0)
        let c = 2.0 * atan2(sqrt(a), sqrt(1.0-a))
        earth_radius * c

(* Example usage:
let diku_up1 = (55.702028, 12.561144)
let peterskirken = (41.902139, 12.453336)
printfn "distance %f" (distance diku_up1 peterskirken)
> distance 1534.497475
*)


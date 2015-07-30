(* Is the latitude and longitude of a coordinate within bounds. *)
fun coordinateOK (lati, longi) = abs(lati) <= 90.0 andalso
                                 abs(longi) <= 180.0

(* Distance in kilometers on the surface of earth between two
 * geographical coordinates.
 *
 * Both coordinates are given as pair of a latitude and a longitude
 * represented as real numbers.
 *
 * distance : (real * real) * (real * real) -> real
 *)
fun distance ((lati0, longi0), (lati1, longi1)) =
  if not (coordinateOK (lati0, longi0) andalso coordinateOK (lati1, longi1))
  then raise Domain
  else
    let fun deg2rad(deg) = deg * (Math.pi/180.0)
        val earth_radius = 6371.0
        val dlati = deg2rad(lati1-lati0)
        val dlongi = deg2rad(longi1-longi0)
        val a = Math.sin(dlati/2.0) * Math.sin(dlati/2.0) +
                Math.cos(deg2rad(lati0)) * Math.cos(deg2rad(lati1)) *
                Math.sin(dlongi/2.0) * Math.sin(dlongi/2.0)
        val c = 2.0 * Math.atan2(Math.sqrt(a), Math.sqrt(1.0-a));
    in
        earth_radius * c
    end;

(* Example usage:

val diku_up1 = (55.702028, 12.561144)
val peterskirken = (41.902139, 12.453336);

- distance (diku_up1, peterskirken);
> val it = 1534.49747481 : real

*)

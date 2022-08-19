let rec bubble (xs:int list) =
  match xs with
    | [] -> []
    | [x] -> [x]
    | x::y::ys -> if x < y then x :: bubble (y::ys)
                  else y :: bubble (x::ys)

let bsort xs = List.fold (fun acc _ -> bubble acc) xs xs

let xs = [7;55;34;23;5;42;32;34;8]
let ys = bsort xs
do printf "%A\n" ys

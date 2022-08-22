let rec insert xs y =
  match xs with
    | [] -> [y]
    | x::xs' -> if y < x then y :: xs
                else x :: insert xs' y

let isort xs = List.fold (fun acc x -> insert acc x) [] xs

let xs = [7;55;34;23;5;42;32;34;8]
do printf "%A\n" (isort xs)

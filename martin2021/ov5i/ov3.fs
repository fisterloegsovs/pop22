
let isTable (t: 'a list list) =
  not (List.isEmpty t) &&
  let len = t.[0].Length
  in len > 0 && List.forall (fun (r: 'a list) -> r.Length = len) t

let v1 = isTable [[1;2;3];[4;5;6]]

let v2 = not (isTable [[1;2;3];[4;5;6;7]])

let firstColumn (t: 'a list list) : 'a list =
  List.map List.head t

let v3 = firstColumn [[1;2;3];[4;5;6];[7;8;9]] = [1;4;7]

let dropFirstColumn (t: 'a list list) : 'a list list =
  List.map List.tail t

let v4 = dropFirstColumn [[1;2;3];[4;5;6];[7;8;9]] = [[2;3];[5;6];[8;9]]

let rec transpose (t: 'a list list) : 'a list list =
  if t.Length <= 0 || t.[0].Length <= 0 then []
  else firstColumn t :: transpose (dropFirstColumn t)

let v5 = transpose [[1;2;3];[4;5;6]] = [[1;4];[2;5];[3;6]]

let t = [[1;2;3;23];[4;5;6;78];[4;8;6;9]]
let v6 = transpose (transpose t) = t

let res = v1 && v2 && v3 && v4 && v5 && v6
do printf "%b\n" res

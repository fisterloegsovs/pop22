let rec merge lt xs ys =
  match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x::xs, y::ys ->
      if lt x y then
        x :: merge lt xs (y::ys)
      else y :: merge lt (x::xs) ys

let rec msort lt xs =
  let sz = List.length xs
  if sz < 2 then xs
  else let n = sz / 2
       let ys = xs.[0..n-1]
       let zs = xs.[n..sz-1]
       in merge lt (msort lt ys)
                   (msort lt zs)

let t1 = [34;7;34;23;2;73;2;36;86;12;11;4;54;35]

do printf "%A\n" t1

do printf "%A\n" (msort (<) t1)

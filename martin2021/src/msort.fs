let rec merge xs ys =
  match xs, ys with
    | [], _ -> ys
    | _, [] -> xs
    | x::xs, y::ys -> if x<y then x::merge xs (y::ys)
                      else y::merge (x::xs) ys

let rec msort xs =
  let sz = List.length xs
  if sz < 2 then xs
  else let n = sz / 2
       let ys = xs.[0..n-1]
       let zs = xs.[n..sz-1]
       in merge (msort ys) (msort zs)

let t1 = [34;7;34;23;2;73;2;36;86;12;11;4;54;35]

do printf "%A\n" t1

do printf "%A\n" (msort t1)

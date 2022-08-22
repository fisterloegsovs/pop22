let partition y xs =
  List.foldBack (fun x (xs,es,ys) -> if x < y then (x::xs,es,ys)
                                     else if x > y then (xs,es,x::ys)
                                     else (xs,x::es,ys)) xs ([],[],[])

let rec qsort xs =
  if List.isEmpty xs then xs
  else let pivot = xs.[0]
       let (xs, es, ys) = partition pivot xs
       in qsort xs @ es @ qsort ys


let t1 = [34;7;34;23;2;73;2;36;86;12;11;4;54;35]

do printf "%A\n" t1

do printf "%A\n" (qsort t1)

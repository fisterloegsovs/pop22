let rec length xs =
  match xs with [] -> 0
              | _ :: ys -> 1 + length ys

let length2 xs =
  let rec len acc xs =
    match xs with [] -> acc
                | _ :: ys -> len (acc+1) ys
  in len 0 xs

let xs = [34;23;56;76;23]

do printf "%d\n" (length xs)
do printf "%d\n" (length2 xs)

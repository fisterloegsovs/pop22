module Pegs

type t = P of int list list

let pr_disk m i =
  let d = 2*i+1
  let p = m-i
  let e = if i = 0 then "|" else "="
  in String.replicate p " " + String.replicate d e + String.replicate p " "

let headDisk xs = if List.isEmpty xs then 0
                  else List.head xs

let tailOrNil xs = if List.isEmpty xs then [] else List.tail xs

let toString (P ps) =
  let m = 1 + List.fold max 0 (List.concat ps)
  let rec pr_rps n rps =
    if n <= 0 then ""
    else pr_rps (n-1) (List.map tailOrNil rps) + "\n" +
         String.concat " " (List.map (pr_disk m) (List.map headDisk rps))
  in pr_rps m (List.map List.rev ps) + "\n" +
     String.replicate (3*(2*m+1)+3) "-" + "\n"

let move src tgt (P pegs) =
  if src < 0 || src > 2 || tgt < 0 || tgt > 2 then
    failwith "invalid move"
  else
  let elem = List.head (pegs.[src])
  let pegs = List.mapi (fun n l ->
                           if n = src then
                              List.tail l
                           else if n = tgt then
                              (if List.isEmpty l || List.head l > elem then elem :: l
                               else failwith "invalid move")
                           else l) pegs
  in P pegs

let init i =
  P (List.init i (fun x -> x+1) :: [[];[]])

module App =
  let pegsRef = ref (init 3)

  let mv src tgt =
    let pegs =
      try move src tgt (!pegsRef) with _ -> (!pegsRef)
    do printf "%s\n" (toString pegs)
    pegsRef := pegs

  let reset n =
    pegsRef := init n
    printf "%s\n" (toString (!pegsRef))

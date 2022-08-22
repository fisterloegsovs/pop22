
let pr_disk m i =
  let d = 2*i+1
  let p = m-i
  let e = if i = 0 then "|" else "o"
  in String.replicate p " " + String.replicate d e + String.replicate p " "

let headDisk xs = if List.isEmpty xs then 0
                  else List.head xs

let tailOrNil xs = if List.isEmpty xs then [] else List.tail xs

let pr_pegs ps =
  let m = 1 + List.fold max 0 (List.concat ps)
  let rec pr_rps rps =
    if List.forall List.isEmpty rps then ""
    else pr_rps (List.map tailOrNil rps) + "\n" +
         String.concat " " (List.map (pr_disk m) (List.map headDisk rps))
  in pr_rps (List.map List.rev ps) + "\n" +
     String.replicate (3*(2*m+1)+3) "-" + "\n"

let mv src tgt (pegs:int list list) =
  let elem = List.head (pegs.[src])
  in List.mapi (fun n l -> if n = src then
                              List.tail l
                           else if n = tgt then
                              elem :: l
                           else l) pegs

let rec hanoi m src aux tgt pegs =
  if m = 0 then pegs
  else
    let pegs = hanoi (m-1) src tgt aux pegs
    let pegs = mv src tgt pegs
    do printf "%s" (pr_pegs pegs)
    let pegs = hanoi (m-1) aux src tgt pegs
    in pegs

let play i =
  let pegs = List.init i (fun x -> x+1) :: [[];[]]
  do printf "%s" (pr_pegs pegs)
  hanoi i 0 1 2 pegs

let res = play 6

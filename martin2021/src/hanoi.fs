let rec hanoi n src aux tgt pegs =
  if n = 0 then pegs
  else let pegs = hanoi (n-1) src tgt aux pegs
       let pegs = Pegs.move src tgt pegs
       do printf "%s" (Pegs.toString pegs)
       let pegs = hanoi (n-1) aux src tgt pegs
       in pegs

let play i =
  let pegs = Pegs.init i
  do printf "%s" (Pegs.toString pegs)
  hanoi i 0 1 2 pegs

let res = play 10

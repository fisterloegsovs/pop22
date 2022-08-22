let headOr0 (lst: int list) : int =
  match lst with
    | [] -> 0       // her returnerer vi 0
    | x :: _ -> x   // variablen x bindes til listehovedet
                    // _ er et "wild-card"

do printf "%A\n" (headOr0 [3;6;2])

do printf "%A\n" (headOr0 [])

let ssort xs = Set.toList (Set.ofList xs)

// funktionen fjerner dubletter

let res = ssort [2;2;3;1;5] = [1;2;3;5]
do printf "%b\n" res

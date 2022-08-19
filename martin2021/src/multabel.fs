let a = Array2D.init 5 5 (fun r c -> (r+1) * (c+1))
let prA (a : int[,]) =
  for r in [0..Array2D.length1 a - 1] do    // 1  2  3  4  5
    for c in [0..Array2D.length2 a - 1] do  // 2  4  6  8 10
      printf "%2d " (a.[r,c])               // 3  6  9 12 15
    printf "\n"                             // 4  8 12 16 20
do prA a                                    // 5 10 15 20 25

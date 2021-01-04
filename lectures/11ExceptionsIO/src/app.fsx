module myMod = 
  let g (x:float) : float = sqrt x

printfn "%A" (myMod.f 3.0)
printfn "%A" (myMod.g 3.0);

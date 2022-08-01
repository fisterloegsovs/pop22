let rec float2cfrac x =
    let qi = floor x
    let ri = x - qi
    if abs ri < 0.0000001 then [int qi] 
    else int qi :: float2cfrac (1.0 / ri)

module test
let trunc v =
  if v < 0 then 0
  elif v > 255 then 255
  else v

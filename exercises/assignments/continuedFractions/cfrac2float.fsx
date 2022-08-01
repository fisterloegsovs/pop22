let rec cfrac2float (lst : int list) : float =
  match lst with
      [] -> 0.0
      | q::[] -> float q
      | q::rst -> float q + 1.0/(cfrac2float rst)

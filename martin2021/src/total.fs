  type department = string
  type costs = (department * float) list
  let total (costs:costs) : float =
    List.fold (fun acc (_,f) -> acc+f) 0.0 costs

do printfn "%f" (total [("cars",3.4);("beers",2.2)])

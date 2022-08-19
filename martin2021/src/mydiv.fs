let mydiv a b : int option =
  try Some (a / b) with
      :? System.DivideByZeroException -> None

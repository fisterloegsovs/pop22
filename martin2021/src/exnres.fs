type 'a result = Ok of 'a | Error of string

let mydiv a b : int result =
  try Ok (a / b) with
    :? System.DivideByZeroException -> Error "div"

let (>>=) a f =
  match a with Ok v -> f v
             | Error s -> Error s

do printfn "%A" (mydiv 8 3 >>= (fun x -> Ok(float(x)+1.0)))

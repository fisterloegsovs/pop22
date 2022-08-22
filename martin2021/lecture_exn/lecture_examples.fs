
exception MyError
exception MyArgExn of int
let e1 : exn = MyError
let e2 : exn = MyArgExn 5;;






let isMyArgExn =
  match e2 with MyArgExn _ -> "yes"
              | _ -> "no";;







exception MyArgExn of int;;
"hello " + raise (MyArgExn 42);;






let mydiv a b : int option =
  try Some (a / b) with
      :? System.DivideByZeroException -> None;;

mydiv 8 0;;






let toFahrenheit c =
  if c < -273.15 then invalidArg "c" "below absolute zero"
  else 9.0/5.0*float(c)+32.0;;

toFahrenheit -300.0;;








let (>>=) x y = Option.bind y x;;

mydiv 8 3 >>= (fun x -> Some(float(x)+1.0));;






type 'a result = Ok of 'a | Error of string

let mydiv a b : int result =
  try Ok (a / b) with
    :? System.DivideByZeroException -> Error "div"

let (>>=) a f =
  match a with Ok v -> f v
             | Error s -> Error s;;

do printfn "%A" (mydiv 8 0 >>= (fun x -> Ok(float(x)+1.0)));;

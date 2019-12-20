type complex (a : float, b : float) =
  let _u = (a, b)
  new (a : float) = complex(a, 0.0)
  member u.re = fst _u
  member u.im = snd _u
  static member (+) (u : complex, v : complex) : complex = 
    complex(u.re + v.re, u.im + v.im)
  static member (+) (u : complex, v : float) : complex = 
    complex(u.re + v, u.im)
  static member (+) (u : float, v : complex) : complex = 
    v + u
  override u.ToString () =
    if u.im >= 0.0 then
      sprintf "(%g + i %g)" u.re u.im
    else
      sprintf "(%g - i %g)" u.re (- u.im)

let x = complex(1.0, 2.0)
let y = complex(2.5)
printfn "%A + %A = %A" x y (x+y)

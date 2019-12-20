type complex (a : float, b : float) =
  let _u = (a, b)
  member u.re = fst _u
  member u.im = snd _u
  member u.add (v : complex) : complex = 
    complex(u.re + v.re, u.im + v.im)
  override u.ToString () =
    if u.im >= 0.0 then
      sprintf "(%g + i %g)" u.re u.im
    else
      sprintf "(%g - i %g)" u.re (- u.im)

let x = complex(1.0, 2.0)
let y = complex(2.5,-1.2)
let z = x.add(y)
printfn "%A + %A = %A" x y z


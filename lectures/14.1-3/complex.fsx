type complex (a : float, b : float) =
  let _re = a
  let _im = b
  member u.re = a
  member u.im = b
  member u.conjugate (a : complex) : complex = 
    new complex(u.re, -u.im)
  member u.add (v : complex) : complex = 
    new complex(u.re + v.re, u.im + v.im)
  member u.mult (v : complex) : complex =
    new complex(u.re * v.re - u.im * v.im, u.re * v.im + u.im * v.re)
  member u.reci () : complex =
    let len = u.re*u.re+u.im*u.im
    new complex(u.re/len, u.im/len)
  override u.ToString () =
    if u.im >= 0.0 then
      sprintf "(%g + i %g)" u.re u.im
    else
      sprintf "(%g - i %g)" u.re (- u.im)

let x = new complex(1.0, 2.0)
let y = new complex(2.5,-1.2)
let z = x.add(y)
printfn "%A + %A = %A" x y z

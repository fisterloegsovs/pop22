type complex (a : float, b : float) =
  let _u = (a, b)
  member this.re = fst _u
  member this.im = snd _u
  member this.add (v : complex) : complex = 
    complex(this.re + v.re, this.im + v.im)
  override this.ToString () =
    if this.im >= 0.0 then
      sprintf "(%g + i %g)" this.re this.im
    else
      sprintf "(%g - i %g)" this.re (- this.im)

let x = complex(1.0, 2.0)
let y = complex(2.5,-1.2)
let z = x.add(y)
printfn "%A + %A = %A" x y z


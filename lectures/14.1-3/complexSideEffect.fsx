type complex (a : float, b : float) =
  let mutable u = (a, b)
  member this.re = fst u
  member this.im = snd u
  member this.add (v : complex) = 
    u <- (this.re + v.re, this.im + v.im)
  override this.ToString () =
    if this.im >= 0.0 then
      sprintf "(%g + i %g)" this.re this.im
    else
      sprintf "(%g - i %g)" this.re (- this.im)
  member this.copy () =
    complex(this.re, this.im)

let x = complex(1.0, 2.0)
let y = complex(2.5,-1.2)
let z = x.copy()
z.add(y)
printfn "%A + %A = %A" x y z


type complex (a : float, b : float) =
  let mutable u = (a, b)
  member this.re = fst u
  member this.im = snd u
  member this.conjugate () = 
    u <- (this.re, -this.im)
  member this.add (v : complex) = 
    u <- (this.re + v.re, this.im + v.im)
  member this.mult (v : complex) =
    u <- (this.re * v.re - this.im * v.im, this.re * v.im + this.im * v.re)
  member this.reci () =
    let len = this.re*this.re+this.im*this.im
    u <- (this.re/len, this.im/len)
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

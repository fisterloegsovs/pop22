type complex (a : float, b : float) =
  let _re = a
  let _im = b
  member this.re = a
  member this.im = b
  member this.conjugate (a : complex) : complex = 
    new complex(this.re, -this.im)
  member this.add (v : complex) : complex = 
    new complex(this.re + v.re, this.im + v.im)
  member this.mult (v : complex) : complex =
    new complex(this.re * v.re - this.im * v.im, this.re * v.im + this.im * v.re)
  member this.reci () : complex =
    let len = this.re*this.re+this.im*this.im
    new complex(this.re/len, this.im/len)
  override this.ToString () =
    if this.im >= 0.0 then
      sprintf "(%g + i %g)" this.re this.im
    else
      sprintf "(%g - i %g)" this.re (- this.im)

let x = new complex(1.0, 2.0)
let y = new complex(2.5,-1.2)
let z = x.add(y)
printfn "%A + %A = %A" x y z

type complex (a : float, b : float) =
  let _u = (a, b)
  member u.re = fst _u
  member u.im = snd _u
  static member (+) (u : complex, v : complex) : complex = 
    complex(u.re + v.re, u.im + v.im)
  override u.ToString () =
    if u.im >= 0.0 then
      sprintf "(%g + i %g)" u.re u.im
    else
      sprintf "(%g - i %g)" u.re (- u.im)
  override u.Equals obj =
    match obj with
      :? complex as v -> u.re = v.re && u.im = v.im
      | _ -> false
  override u.GetHashCode() = hash _u  
  interface System.IComparable with
    member u.CompareTo obj =
      match obj with
        :? complex as v -> compare u.re v.re
        | _ -> invalidArg "obj" "cannot compare values of different types"

let x = complex(1.0, 2.0)
let y = complex(2.5,-1.2)
let lst = [y;x]
printfn "List.sort %A = %A" lst (List.sort lst)


let rec even x =
  if x = 0 then true else odd (x-1)
and odd x =
  if x = 0 then false else even (x-1)

let t = not(odd 34) && even 36 && not(odd 0) &&
        even 0 && not(even 1) && odd 1

do printf "%b\n" t

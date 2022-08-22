
type 'a stream = Stream of 'a * (unit -> 'a stream)

let rec smap f =
  function Stream (v,g) -> Stream(f v,fun () -> smap f (g()))

let rec nat : int stream =
  Stream (0, fun () -> smap (fun x -> x + 1) nat)

let get : 'a stream -> 'a * 'a stream =
  function Stream (v,g) -> (v,g())

let rec getN (n: int) (s: 'a stream) : 'a list * 'a stream =
  if n <= 0 then ([],s)
  else
    let (v,s) = get s
    let (vs,s) = getN (n-1) s
    in (v::vs,s)


do printfn "%A" (fst(getN 12 nat))

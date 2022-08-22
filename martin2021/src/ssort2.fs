let maxInt = System.Int32.MaxValue
let rec select (xs:int list) (m,ys) =
  match xs with
    | [] -> (m,ys)
    | x::xs ->
      match x < m with
        | false -> select xs (m,x::ys)
        | _ ->
          match m <> maxInt with
            | true -> select xs (x,m::ys)
            | false -> select xs (x,ys)
      else
let rec ssort =
  function [] -> []
         | xs -> let (m,xs) = select xs (maxInt,[]) in m :: ssort xs

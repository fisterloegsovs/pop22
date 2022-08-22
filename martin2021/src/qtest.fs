module Q = Queue

let q = List.fold (fun q v -> Q.insert q v) (Q.empty()) [0..5000]

let rec loop q =
  match Q.remove q with
    | None -> 0
    | Some (v,q) -> v + loop q

let a = loop q

do printfn "sum(queue) = %d" a

module AssocList

// Naive implementation of finite maps: good spec, bad code
type map<'k, 'v> = ('k * 'v) list // association list
type 'k set = map<'k, unit>

let empty = ConsList.nil
let insert = ConsList.cons
let delete key = ConsList.rev << ConsList.filterl (fun (k, v) -> not (key = k))
let lookup key = ConsList.recurse (fun (k, v) al f -> if key = k then Some v else f al) None
let exists key = Option.exists (fun _ -> true) << lookup key
let keys al = ConsList.map fst al
let values al = ConsList.map snd al
let isEmpty al = ConsList.recurse (fun _ _ _ -> false) true

let add = ConsList.append
let subtract s1 s2 = ConsList.fold delete s1 (keys s2)
let intersect s1 s2 = ConsList.filter (fun (k, _) -> exists k s2) s1
// association lists mapping strings to values of type ’a
type 'a alist = (string * 'a) list

let add (m:'a alist) (s:string) (v:'a) : 'a alist =
  (s,v)::m

let rec look (m:'a alist) (s:string) : 'a option =
  match m with
    [] -> None
  | h::t -> if fst h = s then Some(snd h)
            else look t s

let empty () : 'a alist = [];;

// m = [A -> 3; B -> 8]
let m0 = add (empty()) "A" 3
let m = add m0 "B" 8;;

let v = look m "A";;

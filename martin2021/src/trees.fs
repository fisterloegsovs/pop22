
type 'a t = L | T of 'a t * 'a * 'a t

let E e = T(L,e,L)
let t = T(T(T(E 8,4,E 9),2,E 5),1,T(E 6,3,E 7))

let rec preorder (t: 'a t) : 'a list =
  match t with     // visit node before children
    | L -> []
    | T(l,e,r) -> [e] @ preorder l @ preorder r

do printfn "preorder t     = %A" (preorder t)

// Efficient version -- avoid @
let rec preorder_acc (acc:'a list) (t: 'a t) : 'a list =
  match t with     // node before children
    | L -> acc
    | T(l,e,r) -> e :: preorder_acc (preorder_acc acc r) l

do printfn "preorder_acc t = %A" (preorder_acc [] t)

let rec postorder (t: 'a t) : 'a list =
  match t with     // visit node after children
    | L -> []
    | T(l,e,r) -> postorder l @ postorder r @ [e]

do printfn "postorder t     = %A" (postorder t)

// Efficient version -- avoid @
let rec postorder_acc (acc:'a list) (t: 'a t) : 'a list =
  match t with     // node after children
    | L -> acc
    | T(l,e,r) -> postorder_acc (postorder_acc (e::acc) r) l

do printfn "postorder_acc t = %A" (postorder_acc [] t)

let rec inorder (t: 'a t) : 'a list =
  match t with     // visit node between children
    | L -> []
    | T(l,e,r) -> inorder l @ [e] @ inorder r

do printfn "inorder t     = %A" (inorder t)

// Efficient version -- avoid @
let rec inorder_acc (acc:'a list) (t: 'a t) : 'a list =
  match t with     // visit node between children
    | L -> acc
    | T(l,e,r) -> inorder_acc (e :: inorder_acc acc r) l

do printfn "inorder_acc t = %A" (inorder_acc [] t)


let breathfirst t =
  let rec bF ts =
    match ts with
      | [] -> []
      | L :: ts -> bF ts
      | T (l,a,r) :: ts -> a :: bF (ts @ [l; r])
  bF [t]

do printfn "breathfirst t      = %A" (breathfirst t)

// ts is a queue!

module Queue =    // GOOD Queue Implementation
  type 'a queue = Q of 'a list * 'a list
  let empty () = Q ([],[])
  let insert (Q (b,f)) v = Q (v::b,f)
  let remove (Q (b,f)) : ('a * 'a queue) option =
    match f with
      | x :: xs -> Some (x,Q(b,xs))
      | [] ->
        match List.rev b with
          | [] -> None
          | x :: xs -> Some (x,Q([],xs))

let breathfirst_good (t:'a t) : 'a list =
  let rec bF (q:'a t Queue.queue) : 'a list =
    match Queue.remove q with
      | None -> []
      | Some(L,q) -> bF q
      | Some(T (l,a,r), q) ->
          a :: bF (Queue.insert (Queue.insert q l) r)
  bF (Queue.insert (Queue.empty()) t)

do printfn "breathfirst_good t = %A" (breathfirst_good t)

let rec insert t v =
  match t with
    | L -> E v
    | T (l,w,r) ->
      if v < w then T (insert l v, w, r)
      else T (l, w, insert r v)


type 'a tg = Lg | Tg of 'a * 'a tg list

let breathfirst_gen (t:'a tg) : 'a list =
  let rec bF (q:'a tg list Queue.queue) : 'a list =
    match Queue.remove q with
      | None -> []
      | Some(gts,q) -> bFs q gts
  and bFs (q:'a tg list Queue.queue) tgs : 'a list =
    match tgs with
      | [] -> bF q
      | Lg::rest -> bFs q rest
      | Tg (a,tgs)::rest -> a :: bFs (Queue.insert q tgs) rest
  bF (Queue.insert (Queue.empty()) [t])

let Eg a = Tg (a,[])

let a = Tg(1,[Eg 2;Tg(3,[Eg 5;Eg 6;Eg 7]);Eg 4])

do printfn "breathfirst_gen a = %A" (breathfirst_gen a)

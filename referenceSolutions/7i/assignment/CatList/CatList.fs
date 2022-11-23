module CatList
open DiffList

type 'a catlist =
    | Empty
    | Single of 'a
    | Append of 'a catlist * 'a catlist


let nil = Empty
let single (elm:'a) : 'a catlist =
    Single elm

let append (xs : 'a catlist) (ys : 'a catlist) : 'a catlist =
    Append (xs, ys)
    
let cons (elm : 'a) (xs : 'a catlist) : 'a catlist =
    append (single elm) xs

let snoc (xs : 'a catlist) (elm : 'a) : 'a catlist =
    append xs (single elm)

let fold (cf:('a -> 'a -> 'a),(e:'a)) (f:('b->'a)) (xs:'b catlist) : 'a =
    let rec g xs =
        match xs with
        | Empty -> e
        | Single x -> f x        
        | Append (ys,zs) -> cf (g ys) (g zs)
    g xs

// length is not required for assignment but is a nifty helper function
let length (xs : 'a catlist) = fold ((+), 0) (fun _ -> 1) xs


let fromCatList (xs : 'a catlist) : 'a list =
    DiffList.fromDiffList (fold (DiffList.append, DiffList.nil) DiffList.single xs)

let toCatList (xs : 'a list) : 'a catlist =
    List.foldBack cons xs nil

let item (i:int) (xs : 'a catlist) : 'a =
    let rec f i' xs =
        match xs with
        | Empty -> failwith "Precondition not satisfied, catlist was empty"
        | Single x when i' = 0 -> x
        | Append (ys, zs) ->
            let ylen = length ys
            if ylen > i' then f i' ys
            else f (i' - ylen) zs
        | _ -> failwith <| sprintf "Precondition violated: catlist has lees than %d elements" i
    f i xs

let insert (i:int) (elm:'a) (xs:'a catlist) : 'a catlist =
    let rec f i' xs =
        if i' = 0 then cons elm xs
        else
            match xs with
            | Append (ys,zs) ->
                let ylen = length ys
                if ylen > i' then append (f i' ys) zs
                else append ys (f (i' - ylen) zs)
            | _ -> failwith "Precondition violated, i > length xs"
    f i xs

let delete (i:int) (xs:'a catlist) : 'a catlist =
    let rec f i' xs =
        match xs with
            | Single _ when i' = 0 -> nil
            | Append (ys,zs) ->
                let ylen = length ys
                if ylen > i' then append (f i' ys) zs
                else append ys (f (i' - ylen) zs)
            | _ -> failwith <| sprintf "Precondition violated: item %d does not exist in catlist" i
    f i xs


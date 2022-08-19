
let leven (a:string) (b:string) : int =
  let arr = Array2D.init (String.length a + 1) (String.length b + 1) (fun _ _ -> -1)
  let rec lev i j =
       if i = 0 then j else if j = 0 then i
       else let la = lev_mem (i-1) j + 1
            let lb = lev_mem i (j-1) + 1
            let lc = lev_mem (i-1) (j-1) + (if a.[i-1] = b.[j-1] then 0 else 1)
            in min la (min lb lc)
      and lev_mem i j =
       let v = arr.[i,j]
       in if v = -1 then
            let res = lev i j
            do arr.[i,j] <- res
            res
          else v
  in lev_mem (String.length a) (String.length b)

let t1 = ("test1", "hi", "hej", 2)

let t2 = ("test2", "horse", "house", 1)

let t3 = ("test3", "dangerous house", "danger horse", 4)

let t4 = ("test4", "horse back riding is a dangerous sport, in particular when the horses are big and the paths are narrow", "riding a horse is dangerous, in particular when the horses are very big and the paths you are riding on are particularly narrow", 59)


let test (t,a,b,e) =
  let l = leven a b
  in if e = l then printf "%s: OK\n" t
     else printf "%s: ERR - expected %d, got %d\n" t e l

do List.map test [t1;t2;t3;t4] |> ignore

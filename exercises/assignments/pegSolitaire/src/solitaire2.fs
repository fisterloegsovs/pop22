module Board =

  // A board is 7x7 and represented as a 2D boolean array of size 7x7

  type t = bool[,]
  type pos = int * int
  type dir = Up | Down | Left | Right
  type mv = pos * dir

  //
  //       0 1 2 3 4 5 6
  //     0     * * *
  //     1     * * *
  //     2 * * * * * * *
  //     3 * * * o * * *
  //     4 * * * * * * *
  //     5     * * *
  //     6     * * *
  //

  let (==>) x y = not x || y

  let valid (r,c) : bool =
    r >= 0 && c >= 0 &&
    r <= 6 && c <= 6 &&
    ((r < 2 || r > 4) ==> (c >= 2 && c <= 4)) &&
    ((c < 2 || c > 4) ==> (r >= 2 && r <= 4))

  let peg (t:t) ((r,c):pos) : bool = Array2D.get t r c

  let set (t:t) ((r,c):pos) (b:bool) : unit =
    Array2D.set t r c b

  let neighbor ((r,c):pos) (d:dir) : pos option =
    let p = match d with Up -> (r-1,c)
                       | Down -> (r+1,c)
                       | Left -> (r,c-1)
                       | Right -> (r,c+1)
    in if valid p then Some p else None

  let mv (t:t) ((p,d):mv) : t option =
    if valid p && peg t p then
      match neighbor p d with
        Some p' ->
          if peg t p' then
            match neighbor p' d with
              Some p'' ->
                if peg t p'' then None
                else let t' = Array2D.copy t
                     let () = set t' p false
                     let () = set t' p' false
                     let () = set t' p'' true
                     in Some t'
            | None -> None
          else None
      | None -> None
    else None

  let empt() : t = Array2D.init 7 7 (fun _ _ -> false)

  let sets (ps:pos list) : t =
    let t = empt()
    in List.fold (fun _ p -> set t p true) () ps; t


  let init () : t =
    sets [//            (0,2);(0,3);(0,4);
          //            (1,2);(1,3);(1,4);
          //(2,0);(2,1);(2,2);(2,3);(2,4);(2,5);(2,6);
          (3,0);//(3,1);(3,2);      (3,4);(3,5);(3,6);
          (4,0);(4,1);(4,2);(4,3);(4,4);(4,5);(4,6);
                      (5,2);(5,3);(5,4);
                      (6,2);(6,3);(6,4)             ]

  let print (t:t) : string =
      List.init 7 (fun r ->
                     List.init 7 (fun c ->
                                    if peg t (r,c) then " *"
                                    else if valid (r,c) then " o"
                                    else "  ")
                     |> String.concat "")
      |> String.concat "\n"

  let pegcount (b:t) : int =
    let c = ref 0
    in Array2D.iter (fun b -> if b then c:= !c + 1 else ()) b; !c

  let pegcenter : t = Array2D.init 7 7 (fun r c -> r=3 && c=3)

// The Main module
module Main =

  module B = Board

  let show b = printfn "%s" (B.print b)

  let mv0 : B.mv = ((0,0), B.Right)

  let nextd (d:B.dir) : B.dir option =
    match d with B.Right -> Some B.Down
               | B.Down -> Some B.Left
               | B.Left -> Some B.Up
               | B.Up -> None

  let nextmv0 ((r,c),d) =
    match nextd d with
        Some d -> Some ((r,c),d)
      | None -> if c < 6 then Some ((r,c+1),B.Right)
                else if r < 6 then Some ((r+1,0),B.Right)
                else None
  let rec nextmv (p,d) =
    match nextmv0 (p,d) with
        Some(p,d) -> if B.valid p then Some(p,d)
                     else nextmv (p,d)
      | None -> None

  type s = B.t * B.mv list

  let rec solve P (b,mvs) ((p,d):B.mv) : s option =
    let maybenext () =
      match nextmv (p,d) with
          Some (p,d) -> solve P (b,mvs) (p,d)
        | None -> None
    in match B.mv b (p,d) with
           None -> maybenext()
         | Some b ->
           if P b then Some (b,(p,d)::mvs)
           else match solve P (b,(p,d)::mvs) mv0 with
                    Some s -> Some s
                  | None -> maybenext()

  let pr_pos ((r,c):B.pos) = "Row " + string r + " Column " + string c

  let pr_dir dir =
    match dir with B.Up -> "Up"
                 | B.Down -> "Down"
                 | B.Left -> "Left"
                 | B.Right -> "Right"

  let pr_mv (p,d) = pr_pos p + " " + pr_dir d
  let pr_mvs mvs = String.concat "\n" (List.map pr_mv mvs)

  let show_mvs mvs = printfn "%s" (pr_mvs mvs)

  let run () =
    let P b = b = B.pegcenter
    let b = B.init()
    let () = show b
    in match solve P (b,[]) mv0 with
           Some (b,mvs) -> (show_mvs (List.rev mvs); show b)
         | None -> printfn "** No solutions..."

let it = Main.run()

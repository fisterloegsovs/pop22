// requires 64-bit words!

module Board =

  // A board is 7x7 and represented as a 64-bit integer; a peg is
  // present at a given position (row,column) if bit row*7+column
  // is set.

  type t = uint64
  type pos = int * int
  type dir = Up | Down | Left | Right
  type mv = pos * dir

  let seti (w:uint64) (i:int) (b:bool) : uint64 =
    let w1 = 1UL <<< i
    in if b then w ||| w1
       else w &&& (~~~ w1)

  let geti (w:uint64) (i:int) : bool =
    1UL &&& (w >>> i) = 1UL

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

  let posi ((r,c):pos) : int = r*7+c

  let peg (t:t) (p:pos) : bool = geti t (posi p)

  let neighbor ((r,c):pos) (d:dir) : pos option =
    let p = match d with Up -> (r-1,c)
                       | Down -> (r+1,c)
                       | Left -> (r,c-1)
                       | Right -> (r,c+1)
    in if valid p then Some p else None

  let mv (t:t) ((p,d):mv) : t option =
    if peg t p then
      match neighbor p d with
        Some p' ->
          if peg t p' then
            match neighbor p' d with
              Some p'' ->
                if peg t p'' then None
                else let t1 = seti t (posi p) false
                     let t2 = seti t1 (posi p') false
                     let t3 = seti t2 (posi p'') true
                     in Some t3
            | None -> None
          else None
      | None -> None
    else None

  let sets ps = List.fold (fun t p -> seti t (posi p) true) 0UL ps

  let init () : t =
    sets [            (0,2);(0,3);(0,4);
                      (1,2);(1,3);(1,4);
          (2,0);(2,1);(2,2);(2,3);(2,4);(2,5);(2,6);
          (3,0);(3,1);(3,2);      (3,4);(3,5);(3,6);
          (4,0);(4,1);(4,2);(4,3);(4,4);(4,5);(4,6);
                      (5,2);(5,3);(5,4);
                      (6,2);(6,3);(6,4)             ]

  let print (t:t) : string =
      List.init 7 (fun r ->
                     List.init 7 (fun c ->
                                    if geti t (posi (r,c)) then " *"
                                    else if valid (r,c) then " o"
                                    else "  ")
                     |> String.concat "")
      |> String.concat "\n"

  let pegcount (b:t) : int =
    let rec iter b i a =
      if i < 0 then a
      else iter (b >>> 1) (i-1)
                (if b &&& 1UL = 1UL then a+1 else a)
    in iter b 49 0

  let pegcenter (b:t) : bool =
    pegcount b = 1 && peg b (3,3)

// The Main module
module Main =

  module B = Board

  let move b (p,d) =
    match B.mv b (p,d) with
        Some b -> b
      | None -> failwith "illegal move"

  let show b = printfn "%s" (B.print b)

  let mv0 : B.mv = ((0,0), B.Right)

  let nextd (d:B.dir) : B.dir option =
    match d with B.Right -> Some B.Down
               | B.Down -> Some B.Left
               | B.Left -> Some B.Up
               | B.Up -> None

  let nextmv ((r,c),d) =
    match nextd d with
        Some d -> Some ((r,c),d)
      | None -> if c < 6 then Some ((r,c+1),B.Right)
                else if r < 6 then Some ((r+1,0),B.Right)
                else None

  type s = B.t * B.mv list

  let rec solve P (b,mvs) ((p,d):B.mv) : s option =
    let maybenext () =
      match nextmv (p,d) with
          Some (p,d) -> solve P (b,mvs) (p,d)
        | None -> None
    in match B.mv b (p,d) with
           None -> maybenext()
         | Some b' ->
           if P b' then Some (b',(p,d)::mvs)
           else match solve P (b',(p,d)::mvs) mv0 with
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
    let P b = B.pegcenter b
    let b = B.init()
    let () = show b
    in match solve P (b,[]) mv0 with
           Some (b,mvs) -> (show_mvs (List.rev mvs); show b)
         | None -> printfn "** No solutions..."

let it = Main.run()

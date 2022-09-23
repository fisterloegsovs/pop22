type pos = int*int

let dist ((x1,y1): pos) ((x2,y2): pos) =
  let dx = x2-x1
  let dy = y2-y1
  dx*dx+dy*dy

let candidates neighbourhood tgt src =
  let currDist = dist tgt src
  let (x2,y2) = src
  neighbourhood
  |> List.map (fun (i,j) -> (x2+i,y2+j)) 
  |> List.filter (fun p -> dist tgt p < currDist) 

let prepend elm lstLst =
  List.map (fun lst -> elm::lst) lstLst

let rec routes neighbourhood (tgt: pos) (src: pos) : pos list list =
  match src with
    p when p = tgt -> [[tgt]]
    | _ ->
      let r = 
        candidates neighbourhood tgt src
        |> List.map (fun p -> prepend src (routes neighbourhood tgt p))
        |> List.concat
      let minLength = List.map List.length r |> List.min
      List.filter (fun elm -> minLength = List.length elm) r

let neighbourhood4 = [(-1,0); (1,0); (0,-1); (0,1)]
let neighbourhood8 = List.allPairs [-1..1] [-1..1] |> List.except [(0,0)]

let init = (3,4)
let goal = (1,1)
let r = routes neighbourhood8 goal init
printfn "The direct routes from %A to %A are\n%A" init goal r

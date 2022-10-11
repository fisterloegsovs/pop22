type pos = int*int


let pow2 x = x*x
let dist (p1:pos) (p2:pos) : int =
    let p1x,p1y = p1
    let p2x,p2y = p2
    (p2x - p1x |> pow2) + (p2y - p1y |> pow2)



let candidates (src:pos) (tg : pos) : pos list =
    let sx,sy = src
    let tx,ty = tg
    let d = dist src tg
    let neighbors = [sx-1,sy;sx+1,sy;sx,sy-1;sx,sy+1]
    neighbors
    |> List.filter (fun x -> dist x tg <= d)
    
let rec routes (src:pos) (tg: pos) : pos list list =
    match candidates src tg with
        // No candidates, so return target as a pos list list
        | [] -> [tg]::[]
        | cs -> // some candidates
            // For each candidate, call routes
            List.map (fun x -> routes x tg) cs
            // Flatten the list
            |> List.concat
            // And prepend the source to the result
            |> List.map (fun x -> src::x)


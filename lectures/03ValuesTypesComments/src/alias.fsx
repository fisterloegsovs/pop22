type record = {mutable content: int}
let r = {content = 1}
let s = r
r.content <- 2
printfn "%A" s;;
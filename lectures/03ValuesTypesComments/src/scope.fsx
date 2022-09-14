let greeting = "Hello" 
let name = "Jon"
do printfn "%s %s" greeting name
(
   let name = "Anders"
   do printfn "%s %s" greeting name
)
do printfn "%s %s" greeting name

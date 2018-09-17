let greetings name =
  "Hello " + name

let str = greetings "Jon"
do printfn "%s" str
do printfn "%s" (greetings "World")

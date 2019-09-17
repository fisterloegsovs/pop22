let greetings (name : string) : string =
  "Hello " + name

let str = greetings "Jon"
do printfn "%s" str
do printfn "%s" (greetings "World")

// Demonstration of Html library:
// To compile:
//    $ fsharpc html.fs html_use.fs
// To run:
//    $ mono html_use.exe

open Html

let flat (xs:html list) : html =
  List.foldBack (fun x acc -> x ++ acc) xs (S"")

let intitems (xs:int list) : html =
  let es = List.map (fun x -> tag "li" (S(x.ToString()))) xs
  tag "ul" (flat es)

let rec fib n =
  if n <= 2 then 1
  else fib (n-1) + fib (n-2)

let doc =
  let fibs = List.map fib [1..10]
  in tag "html" (tag "body" (tag "h2" (S"Fibs") ++
                            intitems fibs))

do printfn "%A" (toString doc)

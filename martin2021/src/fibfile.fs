let rec fib n = if n <= 2 then 1 else fib(n-1)+fib(n-2)
let rec loop n i (w:System.IO.StreamWriter) =
  if i > n then w.Close()
  else (w.WriteLine(string(fib i)); loop n (i+1) w)
do loop 10 1 (System.IO.File.CreateText "out.txt")

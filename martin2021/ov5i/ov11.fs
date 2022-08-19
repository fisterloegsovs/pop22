let reverseArray (a:'a array) : 'a array =
  Array.init a.Length (fun i -> a.[a.Length-i-1])

let res = Array.toList(reverseArray [|1..5|]) = [5 .. -1 .. 1]
do printf "%b\n" res

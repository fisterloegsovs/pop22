let rec fibLessThan N lst =
  match lst with
    b::a::rst -> 
      let c = a + b
      if c > N then
        b::a::rst
      else
        fibLessThan N (c::b::a::rst)
    | _ -> fibLessThan N [1; 1]
let n = 4000000;
let sum = fibLessThan n []
          |> List.filter (fun i -> i%2 = 0)
          |> List.sum
printfn "The sum of even Fibonacci values below %A is %A" n sum

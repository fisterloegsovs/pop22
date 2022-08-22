let arr = Array.init 50000 (fun x -> x)
let mutable sum = 0
for x in arr do sum <- sum + x
do printf "%d\n" sum

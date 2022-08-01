open linalg

let a = [[1;2]; [3;4]]
echo a
let b = [[1; 1]; [2; 2]; [3; 3]]
echo b
let c = map2 (+) a b
echo c

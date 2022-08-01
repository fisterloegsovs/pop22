let reverseApply x f = f x
let applyList lst x =
  (List.map (reverseApply x) lst)
let res =  applyList [cos; sin] 3.5
printfn "applyList [cos; sin] 3.5 = %A" res
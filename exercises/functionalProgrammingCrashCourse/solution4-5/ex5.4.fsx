let reverseApply x f = f x
let applyList lst x =
  let app f = reverseApply x f
  (List.map app lst)
let res =  applyList [cos; sin] 3.5
printfn "applyList [cos; sin] 3.5 = %A" res
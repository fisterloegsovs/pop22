let romanNumerals = function
  | i when 0 < i && i < 999 ->
    let thousand = "M"
    let fivehundred = "D"
    let hundred = "C"
    let fifty = "L"
    let ten = "X"
    let five = "V"
    let one = "I"
    let romanDigit (ten:string) (five:string) (one:string) = function
      | 1 -> one
      | 2 -> one+one
      | 3 -> one+one+one
      | 4 -> one+five
      | 5 -> five
      | 6 -> five+one
      | 7 -> five+one+one
      | 8 -> five+one+one+one
      | 9 -> one+ten
      | _ -> ""
    let digit num = (i/(int (10.0**(float (num-1)))))%10
    (romanDigit thousand fivehundred hundred (digit 3)) + (romanDigit hundred fifty ten (digit 2)) + (romanDigit ten five one (digit 1))
  | _ -> failwith "outside domain 0 < i < 999"

let lst = List.collect (fun j -> List.filter (fun i -> 0 < i && i < 999) (List.map (fun i -> i+j ) ([1..9]@[10..10..99]@[100..100..999]))) [0..5]
let numberPairs i = (i,romanNumerals i)
printfn "%A" (List.map numberPairs lst)

/// Calculates the floating value from a continued fraction represented as a list of integers lst.
let rec cfrac2float (lst : int list) : float =
  if lst.IsEmpty then
    0.0
  else
    (float lst.Head) + 1.0/(cfrac2float lst.Tail)

/// Calculates the continued fraction represented as a list of integers from a floating value x.
let rec float2cfrac (x : float) : int list =
  if abs(x) < 1e-1 then
    []
  else
    let w = int x
    let f = x - (float w)
    w :: (float2cfrac (1.0/f))

/// Calculates the continued fraction represented as a list of integers from a fraction of integers t/n using gcd.
let rec frac2cfrac (t : int) (n : int) : int list =
  if n = 0 then
    []
  else
    (t / n) :: (frac2cfrac n (t % n))

/// Calculates the best integer fraction t/n as (t,n) which best approximates a continued fraction lst in at most j steps.
let cfrac2frac (lst : int list) (j : int) : int * int =
  let rec f (lst : int list) (i : int) (fLst : int list) : int =
    if i >= 0 then
      let elm = lst.Head * fLst.Head + fLst.Tail.Head
      f lst.Tail (i - 1) (elm :: fLst)
    else 
      fLst.Head

  let jClamp = min j (lst.Length - 1)
  let t = f lst jClamp [1; 0]
  let n = f lst jClamp [0; 1]
  (t, n)

// Testing cfrac2float and float2cfrac
// Expected result: "[3; 4; 12; 4] -> 3.245 -> [3; 4; 11; 4]", but rounding error may occur
let c = [3; 4; 12; 4]
let xc = cfrac2float c
let cx = float2cfrac xc
printfn "%A -> %g -> %A" c xc cx

// Testing frac2cfrac and cfrac2frac
// Expected result: "649/200 -> [3; 4; 12; 4] -> 649/200"
let t = 649
let n = 200
let ctn = frac2cfrac t n
let (ct, cn) = cfrac2frac ctn 4
printfn "%d/%d -> %A -> %d/%d" t n ctn ct cn

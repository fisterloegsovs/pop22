let rec exists (p: 'a -> bool) (xs: 'a list) : bool =
  match xs with [] -> false
              | y :: ys -> p y || exists p ys

let b = exists (fun x -> x=23) [34;23;56]

// exists p [34;23;56] $\evals$
// p 34 || exists p [23;56] $\eval$ (34=23) || exists p [23;56] $\eval$
// false || exists p [23;56]  $\eval$ exists p [23;56] $\evals$
// p 23 || exists p [56] $\eval$ (23=23) || exists p [56] $\eval$
// true || exists p [56] $\eval$ true

do printf "%A\n" b

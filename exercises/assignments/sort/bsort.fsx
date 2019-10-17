///<summary>Perform a bubble of the bubble sort algorithm. The
///elements must be comparable, i.e., if x and y are elements, then y
///< x must be defined.</summary>
///<param name = "x"s>An unsorted list</param>
///<returns>An unsorted list with sorted list with the smallest
///element at the Head list with y inserted into xs.</returns>
let rec bubble (xs:int list) =
  if List.isEmpty xs then []   // x::y::ys
  else let x = List.head xs    //  => y::bubble(x::ys) (y<x)
       let ys = List.tail xs
       in if List.isEmpty ys then [x]
          else let y = List.head ys
               in if x < y then x :: bubble ys
                  else y :: bubble (x::List.tail ys)

///<summary>Sort a list using bubble sort. The elements must be
///comparable, i.e., if x and y are elements, then y < x must be
///defined.</summary>
///<param name = "xs">An unsorted list</param>
///<returns> A sorte list with the smallest element at the Head.</returns>
let bsort xs =
  List.fold (fun acc _ -> bubble acc) xs xs

let xs = [7;55;34;23;5;42;32;34;8]
let ys = bsort xs
do printf "%A\n" ys

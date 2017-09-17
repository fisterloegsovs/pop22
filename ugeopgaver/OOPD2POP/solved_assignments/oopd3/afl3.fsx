(*
    F# solution to OOPD course assignment 3
*)

open System.Collections.Generic
open System.IO
open System.Net

exception IndexError
exception InvalidArgument

type Matrix(height: int, width: int) =

    let m = height
    let n = width
    let index i j = j*n + i

    let mutable arr = Array.init (m * n) (fun index -> 0)

    static member (+) (left: Matrix, right: Matrix) =

        let n = left.getWidth
        let m = left.getHeight

        let newMatrix = new Matrix(m, n)
        for j in 0 .. m - 1 do
            for i in 0 .. n - 1 do
                newMatrix.Set i j (right.[i,j] + right.[i,j])
        newMatrix

    member x.Item with get(i:int, j:int) = arr.[index i j]

    member this.Set i j value = arr.[index i j] <- value

    member this.getHeight = m

    member this.getWidth = n

    member this.getSum =
        let mutable sum = 0
        for i in 0 .. m * n-1 do
            sum <- sum + arr.[i]
        sum

    member this.printMatrix = 0

    override x.Equals(other) =
        match other with
        | :? Matrix as y -> x.getSum = y.getSum
        | _              -> false

    interface System.IComparable with
        member x.CompareTo other =
            match other with
            | :? Matrix as y -> compare x.getSum y.getSum
            | _              -> raise InvalidArgument "Cannot compare to non-matrix type"



[<EntryPoint>]
let main _ =
    let m1 = new Matrix(2,3)
    let m2 = new Matrix(3,2)
    let m3 = new Matrix(10,10)
    m1.Set 0 1 1
    m1.Set 2 1 3
    m2.Set 0 1 1
    m2.Set 1 1 5
    m3.Set 9 9 6
    if (m1 < m2) then
        printfn "TEST 1 SUCCES"
    else
        printfn "TEST 1 FAILURE"
    if (not (m1 > m2)) then
        printfn "TEST 2 SUCCES"
    else
        printfn "TEST 2 FAILURE"
    if (not (m1 = m2)) then
        printfn "TEST 3 SUCCES"
    else
        printfn "TEST 3 FAILURE"
    if (m2 = m3) then
        printfn "TEST 4 SUCCES"
    else
        printfn "TEST 4 FAILURE"
    0
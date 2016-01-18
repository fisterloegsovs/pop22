/// Generic interfaces are examples of polymorphism
/// Note, the following example does not work in interactive mode in Mono 4.2!

/// Example Syme et al., Expert F# 4.0 pp. 105
/// We define a generic interface
type INumeric<'T> =
    abstract Zero : 'T
    abstract Subtract : 'T * 'T -> 'T
    abstract LessThan : 'T * 'T -> bool

/// Operations are packaged as a dictionaries of operations
let intOps =
    {new INumeric<int> with
        member ops.Zero = 0
        member ops.Subtract (x, y) = x - y
        member ops.LessThan (x, y) = x < y}
let bigintOps =
    {new INumeric<bigint> with
        member ops.Zero = 0I
        member ops.Subtract (x, y) = x - y
        member ops.LessThan (x, y) = x < y}

/// We define a highest common factor (hcf) using Euclid’s algorithm and a dictionary of operations
let hcfGeneric (ops : INumeric<'T>) =
    let rec hcf a b =
        if a = ops.Zero then b
        elif ops.LessThan(a, b) then hcf a (ops.Subtract(b, a))
        else hcf (ops.Subtract(a, b)) b
    hcf

/// Finally we can declare values of hcf for different dictionaries
let hcfInt = hcfGeneric intOps
let hcfBigInt = hcfGeneric bigintOps
printfn "%A" (hcfInt 18 12)
printfn "%A" (hcfBigInt 1810287116162232383039576I 1239028178293092830480239032I)

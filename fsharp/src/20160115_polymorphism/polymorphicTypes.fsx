/// Type inference is an example of polymorphism

/// Example Symes et al., Expert F# 4.0 p. 97
///
/// Keyword "%A" in printf is an example of polymorphism, and a
/// function can be automatically inferred to be generic
let getFirst (a,b,c) = a
printfn "%A" (getFirst (1,"hej",3.5))
printfn "%A" (getFirst ('c','b','a'))

/// Example Symes et al., Expert F# 4.0 p. 103
///
/// Proof for Euclid’s algorithm for finding the highest common factor
/// (HCF) see Hansen & Rischel, Functional Programming Using F#
/// pp. 15.
///
/// A non-polymorphic version of hcf is
let rec hcf a b =
    if a = 0 then b
    elif a < b then hcf a (b - a)
    else hcf (a - b) b
printfn "%A" (hcf 18 12)
printfn "%A" (hcf 33 24)

/// To convert to polymorphic version, we must supply operations for the corresponding type
let hcfGeneric (zero, sub, lessThan) =
    let rec hcf a b =
        if a = zero then b
        elif lessThan a b then hcf a (sub b a)
        else hcf (sub a b) b
    hcf
/// We can now derive implementations for specific types
let hcfInt = hcfGeneric (0, (-), (<))
let hcfBigInt = hcfGeneric (0I, (-), (<))

printfn "%A" (hcfInt 18 12)
printfn "%A" (hcfBigInt 1810287116162232383039576I 1239028178293092830480239032I)

/// The operations may be collected in a record type
type Numeric<'T> =
    {Zero : 'T
     Subtract : ('T -> 'T -> 'T)
     LessThan : ('T -> 'T -> bool) }
let intOps = {Zero = 0; Subtract = (-); LessThan = (<)}
let bigintOps = {Zero = 0I; Subtract = (-); LessThan = (<)}

let hcfGenericRecords (ops : Numeric<'T>) =
    let rec hcf a b =
        if a = ops.Zero then b
        elif ops.LessThan a b then hcf a (ops.Subtract b a)
        else hcf (ops.Subtract a b) b
    hcf
let hcfIntRecords = hcfGenericRecords intOps
let hcfBigIntRecords = hcfGenericRecords bigintOps

printfn "%A" (hcfIntRecords 18 12)
printfn "%A" (hcfBigIntRecords 1810287116162232383039576I 1239028178293092830480239032I)

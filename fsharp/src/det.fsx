#load "../packages/MathNet.Numerics.FSharp.3.6.0/MathNet.Numerics.fsx"

open MathNet.Numerics
let v = SpecialFunctions.Gamma(0.5)
printfn "%g" v

open MathNet.Numerics.LinearAlgebra
let m : Matrix<float> = DenseMatrix.randomStandard 50 50
printfn "%s" (m.ToString "G5")
let d = (m * m.Transpose()).Determinant()
printfn "%g" d

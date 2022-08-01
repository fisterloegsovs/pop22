#I "../packages/MathNet.Numerics.3.6.0/lib/net40/"
#r "../packages/MathNet.Numerics.3.6.0/lib/net40/MathNet.Numerics.dll"

open MathNet.Numerics
let v = SpecialFunctions.Gamma(0.5)
printfn "%g" v

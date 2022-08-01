/// Demonstrate various degrees of hiding in modules
/// First make the dll:
///   fsharpc -a hideModule.fs
/// Then compiling as:
///   fsharpc -r hideModule.dll hideModule.fsx
/// will have g and h inaccessible while compiling as:
///   fsharpc hideModule.fs hideModule.fsx
/// will only have g as inaccessible

printfn "f: %g" (Hide.f 3.0)
printfn "g: %g" (Hide.g 3.0)
printfn "h: %g" (Hide.h 3.0)
printfn "k: %g" (Hide.k 3.0)

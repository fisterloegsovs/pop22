/// Discriminant unions are an example of polymorphism

/// Example Symes et al., Expert F# 4.0, pp. 92: Transport is one of either Car, Bicyle, or Bus
type Route = int
type Make = string
type Model = string
type Transport =
    | Car of Make * Model
    | Bicycle
    | Bus of Route

/// We may bind identifiers to Transport or Transport list
let ian = Car("BMW","360")
let don = [Bicycle; Bus 8]
let peter = [Car ("Ford","Fiesta"); Bicycle]

/// We use pattern matching to define functions, but since it's a concrete type, then all possibilities must be matched
let averageSpeed (tr : Transport) =
    match tr with
    | Car _ -> 35
    | Bicycle -> 16
    | Bus _ -> 24

ian :: don @ peter |> List.map averageSpeed 

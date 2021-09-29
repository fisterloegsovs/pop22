module area

/// <summary>Calculate the area of a circle.</summary>
/// <remarks>Radius is assumed to be non-negative.</remarks>
/// <example>
/// The following code:
/// <code>
///   let r = 1.5
///   let a = areaCircle r
///   printfn "areaCircle %.1f = %.1f" r a
/// </code>
/// prints <c>areaCircle 1.5 = 7.1</c>.
/// </example>
/// <param name="r">Radius of the circle.</param>
/// <returns>The area of the circle.</returns>
val areaCircle : r:float -> float

/// Calculate the area of an annulus with outer and inner radius R and r.
val areaAnnulus : R:float -> r:float -> float

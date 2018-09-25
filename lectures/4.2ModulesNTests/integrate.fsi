module metaFunctions

/// Estimate the integral of f
/// from a to b with stepsize d
val integrate : (float -> float) -> float -> float -> float -> float

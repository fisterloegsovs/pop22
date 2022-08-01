module Hide

/// This function can be called by all
let public f x = x**2.0

/// This function can only be called from this file and in this module
let private g x = x + 3.0

/// This function can only be called from the same dll or exe
let internal h x = 1.0/x

/// This function is implicitely public, and inside this module, all functions are accessible
let k x =
  x |> f |> g |> h
  

/// A demonstration of System.Diagnostics with trace redirected to console
/// How to compile and run:
///   fsharpc --define:DEBUG assert.fsx
///   mono assert.exe
/// Remove --define:DEBUG to remove debug information

#if DEBUG
/// We first redirect trace and debug information to the console
let consoleTracer = new System.Diagnostics.ConsoleTraceListener ()
System.Diagnostics.Trace.Listeners.Add(consoleTracer);
#endif

/// A reworked example form Symes et al., Expert F# 4.0, p. 529
let isPalindrome (str : string) =
  let rec check(s : int, e : int) =
    /// Simple examples of asserts
    System.Diagnostics.Debug.WriteLine("check call")
    System.Diagnostics.Debug.WriteLineIf((s = 0), "check: First call")
    System.Diagnostics.Debug.Assert (s >= 0 && s < str.Length, "s is outside bounds")
    System.Diagnostics.Debug.Assert (e >= 0 && e < str.Length, "e is outside bounds")
    if s = e || s = e + 1 then
      true
    else if str.[s] <> str.[e] then
      false
    else check(s + 1, e - 1)
  check(0, str.Length - 1)

System.Diagnostics.Debug.Assert (false, "False always fails")
isPalindrome("regninger")
System.Diagnostics.Debug.WriteLine("Finished")

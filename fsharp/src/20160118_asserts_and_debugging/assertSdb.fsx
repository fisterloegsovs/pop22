/// A demonstration of System.Diagnostics with sdb debugging
/// How to compile and run:
///   fsharpc --debug+ assertSdb.fsx
///   sdb "run assertSdb.exe"

/// The program sdb
/// (https://github.com/mono/sdb/blob/master/README.md) is a
/// command-line tool, which is still under development and its
/// documentation is limitted to the command-line help: Once sdb has
/// started use "help" to get help, "step" or "s" to step one line,
/// "help step" to get further help on the step function, "print s" to
/// inspect the value of s, etc..

System.Diagnostics.Debugger.Break ()

/// A reworked example form Symes et al., Expert F# 4.0, p. 529
let isPalindrome (str : string) =
  let rec check(s : int, e : int) =
    if s = e || s = e + 1 then
      true
    else if str.[s] <> str.[e] then
      false
    else check(s + 1, e - 1)
  check(0, str.Length - 1)

isPalindrome("regninger")

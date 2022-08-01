module MyTicker
/// Generate a ticker
///
/// Warning, Mono 4.2 gives a runtime error, when compiling this as a
/// dll. I guess this is a bug in Mono

type public ITicker =
  abstract member tick : unit -> int

val generateTicker : unit -> ITicker

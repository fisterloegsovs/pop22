open System

printfn "Start typing"
while true do
  let key = Console.ReadKey ()
  let shift = if key.Modifiers = ConsoleModifiers.Shift then "SHIFT+" else ""
  let alt = if key.Modifiers = ConsoleModifiers.Alt then "ALT+" else ""
  let ctrl = if key.Modifiers = ConsoleModifiers.Control then "CTRL+" else ""
  printfn "\nDu trykkede på: %s%s%s%s" shift alt ctrl (key.Key.ToString ())


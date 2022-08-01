// Exercise 4.1
fsharpc ex4.1.fsx && mono ex4.1.exe

// Exercise 4.2
fsharpc ex4.2.fsx && mono ex4.2.exe
// The recursive function is very short, the while
// implementation uses several mutable values, which
// may case errors. 

// Exercise 4.3
fsharpc ex4.3.fsx && mono ex4.3.exe
// gcd 8 2 -> gcd 2 0 -> 2
// gcd 2 8 -> gcd 8 2 -> gcd 2 0 -> 2

// Exercise 5.1
// with recursion
fsharpc ex5.1.fsx && mono ex5.1.exe
// with fold
fsharpc ex5.1Alt.fsx && mono ex5.1Alt.exe

// Exercise 5.2
fsharpc ex5.2.fsx && mono ex5.2.exe

// Exercise 5.3
fsharpc ex5.3.fsx && mono ex5.3.exe

// Exercise 5.4
fsharpc ex5.4.fsx && mono ex5.4.exe
// shorter with currying:
fsharpc ex5.4Alt.fsx && mono ex5.4Alt.exe

// Exercise 5.5
fsharpc ex5.5.fsx && mono ex5.5.exe
// Shorter with List.collect
fsharpc ex5.5Alt.fsx && mono ex5.5Alt.exe

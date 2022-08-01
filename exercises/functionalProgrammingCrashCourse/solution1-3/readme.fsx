// Exercise 1.1
3.14+2.78;;
// Fsharp prints 5.92

// Exercise 1.2
// ex1.2.fsx
fsharpi ex1.2.fsx
fsharpc ex1.2.fsx && mono ex1.2.exe
// Fsharp/mono runs with no output, since no printf is included

// Exercise 1.3
3 + 1.0;;
// Type mismatch
3.0 + 1.0;;
3 + 1;;
float 3 + 1.0;;
3 + int 1.0;;

// Exercise 1.4
164uy+230uy;;
// uy means unsigned int, and expressions gives an overflow

// Exercise 1.5
"hello world".[6..];;

// Exercise 2.1
// ex2.1.fsx
fsharpc ex2.1.fsx && mono ex2.1.exe
// The paranthesis is needed to ensure that the expression is evaluate befor printing. Otherwise the statement will give a syntax error
// ex2.1b.fsx
fsharpc ex2.1b.fsx && mono ex2.1b.exe

// Exercise 2.2
// ex2.2.fsx
fsharpc ex2.2.fsx && mono ex2.2.exe

// Exercise 2.3
// Ex2.3.fsx
fsharpc ex2.3.fsx && mono ex2.3.exe
// The error was that 'name' was not defined outside the scope of the let-in statement.
// Ex2.3b.fsx
fsharpc ex2.3b.fsx && mono ex2.3b.exe

// Exercise 2.4
// Ex2.4.fsx
fsharpc ex2.4.fsx && mono ex2.4.exe

// Exercise 2.5
// Ex2.5.fsx
fsharpc ex2.5.fsx && mono ex2.5.exe
// Ex2.5b.fsx
fsharpc ex2.5b.fsx && mono ex2.5b.exe
// Ex2.5c.fsx
fsharpc ex2.5c.fsx && mono ex2.5c.exe
// ex2.5b with the for loop is simplest and most elegant, since it has fewest lines, easiest to modify for other boundaries, and no explicit updates of the counter variable.

// Exercise 2.6
// Ex2.6.fsx
fsharpc ex2.6.fsx && mono ex2.6.exe

// Exercise 3.1
// vec2d.fsi and vec2d.fs
fsharpc -a vec2d.fsi vec2d.fs

// Exercise 3.2
// vec2dapp.fsx
fsharpc -r vec2d.dll vec2dapp.fsx && mono vec2dapp.exe

// Exercise 3.3
// vec2dAlt.fsx
// Alternative is to use vector elements as arguments, but then
// functions that return vectors can only return on of the two.
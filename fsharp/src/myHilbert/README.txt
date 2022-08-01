Illustration of using fsharp for working with polygons in Fsharp

1. fsharpi drawing.fsx:
  Curves represented as float list list
  functions that produce new curves as modifications of old

2. fsharpi curve.fs drawing2.fsx:
  Structuring programs with modules separates the general from the
  specific and aids in focussing attention on the relevant

3. fsharpi curve.fs drawing3.fsx:
  Displaying curves in a window - coordinate systems are a pain

4. fsharpi curve.fs hilbert1.fsx
  Turtle graphics for space-filling curves - warmup for recursive
  drawing
  Function composition versus piping

5. fsharpi curve.fs hilbert2.fsx
  Lindemayer system for hilbert curves:
    Alphabet : A, B
    Constants : F + −
    Axiom : A
    Production rules:
    A → − B F + A F A + F B −
    B  → + A F − B F B − F A +
  Construction is evaluated to some depth, and when drawing, then A
  and B rules are ignored.

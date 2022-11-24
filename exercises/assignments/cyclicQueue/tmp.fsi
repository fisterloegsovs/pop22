module stack
type stack<'e> = 'e list
val init : unit:'a -> stack<'e>
val pop : stck:stack<'e> -> 'e * stack<'e>
val push : elm:'e -> stck:stack<'e> -> stack<'e>


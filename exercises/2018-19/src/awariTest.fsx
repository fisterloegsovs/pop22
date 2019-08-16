let test str b p i expected =
  let (result, finalPitsPlayer, finalPit) = Awari.distribute b p i
  printfn "%5b: %s" (result = expected) str

let b : Awari.board = [3;3;3;3;3;3;0;3;3;3;3;3;3;0]
  
test "Initial setup player 1 chooses pit 1" b Awari.Player1 1 [0;4;4;4;3;3;0;3;3;3;3;3;3;0]
test "Initial setup player 1 chooses pit 2" b Awari.Player1 2 [3;0;4;4;4;3;0;3;3;3;3;3;3;0]
test "Initial setup player 1 chooses pit 3" b Awari.Player1 3 [3;3;0;4;4;4;0;3;3;3;3;3;3;0]
test "Initial setup player 1 chooses pit 4" b Awari.Player1 4 [3;3;3;0;4;4;1;3;3;3;3;3;3;0]
test "Initial setup player 1 chooses pit 5" b Awari.Player1 5 [3;3;3;3;0;4;1;4;3;3;3;3;3;0] 
test "Initial setup player 1 chooses pit 6" b Awari.Player1 6 [3;3;3;3;3;0;1;4;4;3;3;3;3;0]
test "Initial setup player 2 chooses pit 1" b Awari.Player2 1 [3;3;3;3;3;3;0;0;4;4;4;3;3;0]
test "Initial setup player 2 chooses pit 2" b Awari.Player2 2 [3;3;3;3;3;3;0;3;0;4;4;4;3;0]
test "Initial setup player 2 chooses pit 3" b Awari.Player2 3 [3;3;3;3;3;3;0;3;3;0;4;4;4;0]
test "Initial setup player 2 chooses pit 4" b Awari.Player2 4 [3;3;3;3;3;3;0;3;3;3;0;4;4;1]
test "Initial setup player 2 chooses pit 5" b Awari.Player2 5 [4;3;3;3;3;3;0;3;3;3;3;0;4;1]
test "Initial setup player 2 chooses pit 6" b Awari.Player2 6 [4;4;3;3;3;3;0;3;3;3;3;3;0;1]
test "Capture starting player 1 side landing player 1 side" [3;3;3;0;3;3;0;3;3;3;3;3;3;0] Awari.Player1 1 [0;4;4;0;3;3;4;3;3;0;3;3;3;0]
test "Capture starting player 1 side landing player 2 side" [3;3;3;3;3;3;0;3;0;3;3;3;3;0] Awari.Player1 6 [3;3;3;3;0;0;5;4;0;3;3;3;3;0]
test "Capture starting player 2 side landing player 2 side" [3;3;3;3;3;3;0;3;3;3;0;3;3;0] Awari.Player2 1 [3;3;0;3;3;3;0;0;4;4;0;3;3;4]
test "Capture starting player 2 side landing player 1 side" [3;0;3;3;3;3;0;3;3;3;3;3;3;0] Awari.Player2 6 [4;0;3;3;3;3;0;3;3;3;3;0;0;5]

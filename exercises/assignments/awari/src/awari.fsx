// Initial board, first 7 is player 1's 1-6 and home, second 7 is
// player 2's
let initialB : Awari.board = [3;3;3;3;3;3;0;3;3;3;3;3;3;0]

let finalB = Awari.play initialB Awari.Player1
let winnerStr =
  if finalB.[6] > finalB.[13] then
    "Player 1 wins."
  elif finalB.[6] = finalB.[13] then
    "It's a tie."
  else
    "Player 2 wins."
    
printfn "Game over: %s" winnerStr

type suit = Hearts | Diamonds | Clubs | Spades     // The suit of a card

type rank = Two | Three | Four | Five | Six        // The rank of a card
          | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace

type card = rank * suit  // A card is a combination of a rank and a suit

let succSuit s =
  match s with
    | Hearts -> Some Diamonds
    | Diamonds -> Some Clubs
    | Clubs -> Some Spades
    | Spades -> None

let succRank r =
  match r with
   | Two -> Some Three
   | Three -> Some Four
   | Four -> Some Five
   | Five -> Some Six
   | Six -> Some Seven
   | Seven -> Some Eight
   | Eight -> Some Nine
   | Nine -> Some Ten
   | Ten -> Some Jack
   | Jack -> Some Queen
   | Queen -> Some King
   | King -> Some Ace
   | Ace -> None

let succCard (r,s) =
  match succRank r with
    | None -> (match succSuit s with
                 | None -> None
                 | Some s' -> Some (Two,s'))
    | Some r' -> Some (r',s)

let initDeck() =
  let rec run c =
        c :: (match succCard c with
                  | Some c' -> run c'
                  | None -> [])
  in run (Two,Hearts)

let cards = initDeck()

do printfn "cards : %d" (List.length cards)

let sameRank c1 c2 = fst c1 = fst c2
let sameSuit c1 c2 = snd c1 = snd c2
let highCard c1 c2 = if fst c1 >= fst c2 then c1 else c2

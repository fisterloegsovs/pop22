/// A game is a battleship game
type game() = class end

/// A player is a human player, which has 2 boards and
/// several ships
type player() = class end

/// An opponent is another player
type opponent() = class end

/// A ship can be damaged
type ship() = class end

/// A board is a square set of fields with row-column
/// coordinates. Ships are placed on the board
type board() = class end

/// A field is can be covered by a ship and can have been
/// shootend at
type field() = class end

/// A coordinate is a location on a board
type coordinate() = class end

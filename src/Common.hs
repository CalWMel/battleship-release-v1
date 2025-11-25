{- Common data types used throughout the application -}
module Common where 

type RowID = Int
type ColumnID = Int

-- Row number, column number
type CoOrdinate = (RowID, ColumnID)

data Instruction =
    Quit
  | Fire CoOrdinate
  deriving (Show, Eq)

data GameError = AlreadyHit | InvalidCoOrdinate

instance Show GameError where
    show AlreadyHit = "Co-ordinate has already been hit!"
    show InvalidCoOrdinate = "Invalid co-ordinate!"

data TurnError = TEQuit | TEParseError String | TEGameError GameError

instance Show TurnError where
    show TEQuit = "Quit"
    show (TEParseError err) = "Parse error: " ++ err
    show (TEGameError err) = "Game error: " ++ (show err)


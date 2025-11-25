module Parser (parseInstruction, parseCoordinateAndDir) where
import Text.Parsec
import Game
import Common
import Data.Char(ord, toLower)

type Parser a = Parsec String () a

-- Parse a coordinate like "a12" --> (0, 12) or "C7" --> (2, 7)
coordinate :: Parser CoOrdinate
coordinate = do
  row <- oneOf ['A'..'Z'] <|> oneOf ['a'..'z']
  col <- many1 digit
  pure (ord (toLower row) - ord 'a', read col)

-- Parse ship names
ship :: Parser Ship
ship =
      (string "carrier"    >> pure Carrier)
  <|> (string "battleship" >> pure Battleship)
  <|> (string "cruiser"    >> pure Cruiser)
  <|> (string "submarine"  >> pure Submarine)
  <|> (string "destroyer"  >> pure Destroyer)
  <?> "ship"

direction :: Parser Direction
direction =
      (string "up"    >> pure DirUp)
  <|> (string "down"  >> pure DirDown)
  <|> (string "left"  >> pure DirLeft)
  <|> (string "right" >> pure DirRight)
  <?> "direction"


coordinateAndDir :: Parser (CoOrdinate, Direction)
coordinateAndDir = do
    coOrd <- coordinate
    spaces
    dir <- direction
    eofOrSpace
    return (coOrd, dir)

-- Parse instructions
instruction :: Parser Instruction
instruction =
      (string "quit" >> eofOrSpace >> pure Quit)
  <|> try (do
        _ <- string "fire"
        spaces
        coord <- coordinate
        eofOrSpace
        pure (Fire coord))
  <?> "instruction"

eofOrSpace :: Parser ()
eofOrSpace = skipMany (space <|> newline) >> optional eof >> pure ()

parseInstruction :: String -> Either TurnError Instruction
parseInstruction str = 
  case parse instruction "<input>" str of
    Left err -> Left $ TEParseError (show err)
    Right res -> Right res

parseCoordinateAndDir :: String -> Either TurnError (CoOrdinate, Direction)
parseCoordinateAndDir str = 
  case parse coordinateAndDir "<input>" str of
    Left err -> Left $ TEParseError (show err)
    Right res -> Right res

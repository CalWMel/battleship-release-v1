{- Main data structures and game logic -}
module Game where
import Data.Char (chr)
import Data.Foldable
import Data.Maybe
import System.Random
import Common

{- Definitions of data structures and some useful helpers -}
data AIState = MkAIState { aiGenerator :: StdGen }

initAIState :: IO AIState
initAIState = fmap MkAIState initStdGen

data Player = PlayerOne | PlayerTwo
    deriving (Eq)

advanceTurn :: Player -> Player
advanceTurn PlayerOne = PlayerTwo
advanceTurn PlayerTwo = PlayerOne

instance Show Player where
    show PlayerOne = "Player 1"
    show PlayerTwo = "Player 2"

data Ship = Carrier | Battleship | Cruiser | Submarine | Destroyer
    deriving (Show, Eq)

allShips :: [Ship]
allShips = [Carrier, Battleship, Cruiser, Submarine, Destroyer]

-- Number of squares taken up by a piece
shipSize :: Ship -> Size
shipSize Carrier = 5
shipSize Battleship = 4
shipSize Cruiser = 3
shipSize Submarine = 3
shipSize Destroyer = 2

-- Two-letter code for each ship
shipCode :: Ship -> String
shipCode Carrier = "Ca"
shipCode Battleship = "Bs"
shipCode Cruiser = "Cr"
shipCode Submarine = "Sb"
shipCode Destroyer = "Ds"

data Direction = DirUp | DirDown | DirLeft | DirRight
    deriving (Show, Eq)

type Size = Int
type Range = (CoOrdinate, CoOrdinate)

-- Result of a command
data Result = 
        Hit
      | Miss
      | Sunk Ship
      deriving (Eq)

instance Show Result where
    show Hit = "Hit!"
    show Miss = "Miss!"
    show (Sunk ship) = "Sunk the opponent's " ++ show ship ++ "!"

type BoardSize = (Int, Int)

-- State of the game from the point of view of the player
data PlayerState = MkPlayerState {
    -- Board size
    psBoardSize :: BoardSize,
    -- Player's ship placements
    psShips :: [(Ship, Range)],
    -- Player's hits on the opponent's grid
    psOpponentHits :: [CoOrdinate],
    -- Player's misses on the opponent's grid
    psOpponentMisses :: [CoOrdinate],
    -- Hits on player's own ships
    psSelfHits :: [CoOrdinate]
}
    deriving Show

emptyPlayerState :: BoardSize -> PlayerState
emptyPlayerState size = MkPlayerState {
        psBoardSize = size,
        psShips = [],
        psOpponentHits = [],
        psOpponentMisses = [],
        psSelfHits = []
    }

-- State of a game that's in progress
data GameState = MkGameState { 
    gameP1State  :: PlayerState,
    gameP2State  :: PlayerState,
    gameAIState  :: AIState
}

mkGameState :: PlayerState -> PlayerState -> AIState -> GameState
mkGameState p1State p2State aiState = MkGameState {
    gameP1State = p1State,
    gameP2State = p2State,
    gameAIState = aiState
}

{- Exercise 1: Helper functions -}

expandRange :: Range -> [CoOrdinate]
expandRange ((r1, c1), (r2, c2)) = [(r, c) | r <- [r1..r2], c <- [c1..c2]]

inRange :: CoOrdinate -> Range -> Bool
inRange (r, c) ((r1, c1), (r2, c2)) = r >= r1 && r <= r2 && c >= c1 && c <= c2

lookupShip :: PlayerState -> CoOrdinate -> Maybe (Ship, Range)
lookupShip pst coord = find (\(s, r) -> inRange coord r) (psShips pst)

{- Exercise 2: Win checking -}
checkWin :: GameState -> Maybe Player
checkWin gs
    | allSunk (gameP1State gs) = Just PlayerTwo 
    | allSunk (gameP2State gs) = Just PlayerOne
    | otherwise                = Nothing
  where
    allSunk :: PlayerState -> Bool
    allSunk pst = all (\coord -> coord `elem` psSelfHits pst) allShipCoords
      where
        allShipCoords = concatMap (expandRange . snd) (psShips pst)

{- Exercise 3: Displaying the player's board -}
showPlayerBoard :: PlayerState -> String
showPlayerBoard pst = header ++ "\n" ++ rows
  where
    (numRows, numCols) = psBoardSize pst

    header = "  " ++ unwords [padColNumber c | c <- [0..numCols - 1]]
    
    padColNumber :: Int -> String
    padColNumber n = if n < 10 then " " ++ show n ++ " " else " " ++ show n

    rows = unlines [renderRow r | r <- [0..numRows - 1]]

    renderRow :: Int -> String
    renderRow r = rowLabel ++ " " ++ unwords [renderCell (r, c) | c <- [0..numCols - 1]]
      where
        rowLabel = [chr (65 + r)]

    renderCell :: CoOrdinate -> String
    renderCell coord =
        case lookupShip pst coord of
            Nothing -> "~~~"
            Just (ship, _) -> 
                let 
                    code = shipCode ship 
                    status = if coord `elem` psSelfHits pst then 'x' else 'o'
                in code ++ [status]
    
{- Exercise 4: Displaying the player's view of the opponent's board -}
showOpponentBoard :: PlayerState ->  String
showOpponentBoard pst = header ++ "\n" ++ rows
  where
    (numRows, numCols) = psBoardSize pst

    header = "  " ++ unwords [padColNumber c | c <- [0..numCols - 1]]
    
    padColNumber :: Int -> String
    padColNumber n = if n < 10 then " " ++ show n ++ " " else " " ++ show n

    rows = unlines [renderRow r | r <- [0..numRows - 1]]

    renderRow :: Int -> String
    renderRow r = rowLabel ++ " " ++ unwords [renderCell (r, c) | c <- [0..numCols - 1]]
      where
        rowLabel = [chr (65 + r)]

    renderCell :: CoOrdinate -> String
    renderCell coord
        | coord `elem` psOpponentHits pst   = "xxx"
        | coord `elem` psOpponentMisses pst = "000"
        | otherwise                         = "???"

{- Exercise 5: Ship placement -}
placeShip :: PlayerState -> Ship -> CoOrdinate -> Direction -> Maybe PlayerState
placeShip pst ship (r, c) dir = 
    if validRange && noOverlap
    then Just (pst { psShips = (ship, range) : psShips pst })
    else Nothing
  where
    (rows, cols) = psBoardSize pst
    size = shipSize ship
    
    rawRange :: Range
    rawRange = case dir of
        DirUp    -> ((r - size + 1, c), (r, c))
        DirDown  -> ((r, c), (r + size - 1, c))
        DirLeft  -> ((r, c - size + 1), (r, c))
        DirRight -> ((r, c), (r, c + size - 1))

    range :: Range
    range = 
        let ((r1, c1), (r2, c2)) = rawRange
        in ((min r1 r2, min c1 c2), (max r1 r2, max c1 c2))

    validRange :: Bool
    validRange = 
        let ((r1, c1), (r2, c2)) = range
        in r1 >= 0 && r2 < rows && c1 >= 0 && c2 < cols

    noOverlap :: Bool
    noOverlap = not (any isOccupied (expandRange range))
      where
        isOccupied :: CoOrdinate -> Bool
        isOccupied coord = isJust (lookupShip pst coord)

{- Exercise 6: Firing -}
fire :: GameState -> Player -> CoOrdinate -> Either GameError (Result, GameState)
fire gs player coord
    | not validCoord = Left InvalidCoOrdinate
    | alreadyFired   = Left AlreadyHit
    | otherwise      = Right (result, newGameState)
  where
    (shooter, target, updateGameState) = case player of
        PlayerOne -> (gameP1State gs, gameP2State gs, \s t -> gs { gameP1State = s, gameP2State = t })
        PlayerTwo -> (gameP2State gs, gameP1State gs, \s t -> gs { gameP2State = s, gameP1State = t })

    (rows, cols) = psBoardSize shooter
    (r, c) = coord
    validCoord = r >= 0 && r < rows && c >= 0 && c < cols

    alreadyFired = coord `elem` psOpponentHits shooter || coord `elem` psOpponentMisses shooter

    (result, newShooter, newTarget) = case lookupShip target coord of
        Nothing -> 
            (Miss, shooter { psOpponentMisses = coord : psOpponentMisses shooter }, target)
        
        Just (ship, range) ->
            let 
                updatedTarget = target { psSelfHits = coord : psSelfHits target }
                updatedShooter = shooter { psOpponentHits = coord : psOpponentHits shooter }
                
                shipCoords = expandRange range
                isSunk = all (\c -> c `elem` psSelfHits updatedTarget) shipCoords
            in 
                (if isSunk then Sunk ship else Hit, updatedShooter, updatedTarget)

    newGameState = updateGameState newShooter newTarget

{- Exercise 7: Board randomisation -}
randomBoard :: BoardSize -> StdGen -> PlayerState
randomBoard _ _ = error "Fill me in"

{- Exercise 8: Random move -}
makeRandomAIMove :: AIState -> PlayerState -> (CoOrdinate, AIState)
makeRandomAIMove _ _ = error "Fill me in"


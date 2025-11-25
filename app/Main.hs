module Main (main) where
import Game
import Common
import qualified Parser as P
import qualified Render (gameLoop)
import Data.Bifunctor (first)
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import System.IO
import System.Random
import Options.Applicative

data GameMode = PlayerVsPlayer | PlayerVsAI | AIVsAI
data ArgConfig = MkArgConfig { acGameMode :: GameMode, acRandomiseBoard :: Bool, acRender :: Bool }

type Battleship = ExceptT TurnError IO 

placementLoop :: Player -> PlayerState -> [Ship] -> IO PlayerState
placementLoop _ pst [] = return pst
placementLoop turn pst (ship:ships) = do
    let turnStr = "[" ++ show turn ++ "]"
    putStrLn (showPlayerBoard pst ++ "\n")
    putStr $ turnStr ++ " Co-ordinate and direction for " ++ (show ship) ++ " > "
    hFlush stdout
    input <- getLine
    case P.parseCoordinateAndDir input of
        Left _ ->
            putStrLn "Invalid co-ordinate or direction!" >>
            placementLoop turn pst (ship:ships)
        Right ((r, c), dir) ->
            case placeShip pst ship (r, c) dir of
                Just pst' ->
                    putStrLn "Placed!" >>
                    placementLoop turn pst' ships
                Nothing ->
                    putStrLn "Invalid ship placement!" >>
                    placementLoop turn pst (ship:ships)


defaultBoardSize :: (Int, Int)
defaultBoardSize = (10, 10)

playerState :: Player -> GameState -> PlayerState
playerState PlayerOne gs = gameP1State gs
playerState PlayerTwo gs = gameP2State gs

humanPlayer :: Player -> GameState -> Battleship GameState
humanPlayer t gs = do
    liftIO (putStrLn "Our view of opponent's board:")
    liftIO (putStrLn (showOpponentBoard (playerState t gs)))
    --
    liftIO (putStrLn "Our board:")
    liftIO (putStrLn (showPlayerBoard (playerState t gs)))
    --
    liftIO $ putStr "Command > "
    cmdStr <- liftIO getLine
    cmd <- liftEither $ P.parseInstruction cmdStr
    case cmd of
        Quit -> throwE TEQuit
        Fire coOrd -> do
            (res, gs') <- liftEither $ first TEGameError (fire gs t coOrd)
            liftIO (putStrLn $ show res)
            return gs'

gameLoop :: Player -> GameState -> IO ()
gameLoop t gs =
    putStrLn ((show t) ++ "'s turn.") >>
    {- Check if either player has won, terminating if so -}
    case checkWin gs of
        Just winner -> putStrLn $ show winner ++ " wins!"
        Nothing ->
            {- Next, run the turn -} 
            runExceptT (humanPlayer t gs) >>= \res ->
            case res of
                Left (TEQuit) -> return ()
                -- Print the error and retry
                Left (TEParseError err) ->
                    putStrLn ("[Error] " ++ err) >> gameLoop t gs
                Left (TEGameError err) ->
                    putStrLn ("[Error] " ++ (show err)) >> gameLoop t gs
                Right gs' ->
                    gameLoop (advanceTurn t) gs'



aiGameLoop :: Player -> GameState -> IO ()
aiGameLoop t gs =
    putStrLn ((show t) ++ "'s turn.") >>
    case checkWin gs of
        Just winner -> putStrLn $ show winner ++ " wins!"
        Nothing -> do
            let ais = gameAIState gs 
            let ps = if t == PlayerOne then (gameP1State gs) else (gameP2State gs)
            let (coOrd, ais') = makeRandomAIMove ais ps
            let gs' = gs { gameAIState = ais' }
            putStrLn (show t ++ ": firing at " ++ (show coOrd))
            case fire gs' t coOrd of
                Left err ->
                    putStrLn ("AI Game error: " ++ (show err)) >>
                    aiGameLoop t gs'
                Right (res, gs'') ->
                    putStrLn (show res) >> aiGameLoop (advanceTurn t) gs''


humanVsAIGameLoop :: Player -> GameState -> IO ()
humanVsAIGameLoop t gs = do
    putStrLn ((show t) ++ "'s turn.")
    case checkWin gs of
        Just winner -> putStrLn $ show winner ++ " wins!"
        Nothing -> do
            let ais = gameAIState gs 
            if t == PlayerOne then do
                -- Human's turn
                {- Next, run the turn -} 
                res <- runExceptT (humanPlayer t gs)
                case res of
                    Left (TEQuit) -> return ()
                    -- Print the error and retry
                    Left (TEParseError err) ->
                        putStrLn ("[Error] " ++ err) >> humanVsAIGameLoop t gs
                    Left (TEGameError err) ->
                        putStrLn ("[Error] " ++ (show err)) >> humanVsAIGameLoop t gs
                    -- Advance the turn
                    Right gs' ->
                        humanVsAIGameLoop (advanceTurn t) gs'
            else do
                -- AI's turn
                let ps = gameP2State gs
                let (coOrd, ais') = makeRandomAIMove ais ps
                let gs' = gs { gameAIState = ais' }
                putStrLn (show t ++ ": firing at " ++ (show coOrd))
                case fire gs' t coOrd of
                    Left err ->
                        putStrLn ("AI Game error: " ++ (show err)) >>
                        humanVsAIGameLoop t gs'
                    Right (res, gs'') ->
                        putStrLn (show res) >> humanVsAIGameLoop (advanceTurn t) gs''

twoPlayerSetup :: IO (PlayerState, PlayerState)
twoPlayerSetup = do
    p1State <- placementLoop PlayerOne (emptyPlayerState defaultBoardSize) allShips
    p2State <- placementLoop PlayerTwo (emptyPlayerState defaultBoardSize) allShips
    return (p1State, p2State)


initRandomPlayerState :: IO PlayerState
initRandomPlayerState = initStdGen >>= return . (randomBoard defaultBoardSize)

twoAISetup :: IO (PlayerState, PlayerState)
twoAISetup = (,) <$> initRandomPlayerState <*> initRandomPlayerState

humanVsAISetup :: Bool -> IO (PlayerState, PlayerState)
humanVsAISetup randomise = do
    gen0 <- initStdGen
    ps1 <- if randomise then
             return $ randomBoard defaultBoardSize gen0
           else 
             placementLoop PlayerOne (emptyPlayerState defaultBoardSize) allShips
    gen1 <- initStdGen
    let ps2 = randomBoard defaultBoardSize gen1
    return (ps1, ps2)

parseArgs :: IO ArgConfig
parseArgs = execParser $ info (helper <*> argParser) mempty
    where argParser :: Parser ArgConfig
          argParser =
              MkArgConfig
                <$>
                (flag' PlayerVsAI  (long "ai")
                  <|> flag' AIVsAI (long "aiai")
                  <|> pure PlayerVsPlayer)
                <*> switch (long "randBoard")
                <*> switch (long "render")

main :: IO ()
main = do 
    ac <- parseArgs
    aiState <- initAIState
    -- Only randomised player vs. AI is supported in render mode
    if (acRender ac) then 
        twoAISetup >>= \(p1State, p2State) ->
        let gs = mkGameState p1State p2State aiState in
        Render.gameLoop gs
    else 
        case acGameMode ac of
                PlayerVsPlayer ->
                    (if acRandomiseBoard ac then twoAISetup else twoPlayerSetup) >>= \(p1State, p2State) -> 
                    gameLoop PlayerOne (mkGameState p1State p2State aiState)
                PlayerVsAI ->
                    humanVsAISetup (acRandomiseBoard ac) >>= \(p1State, p2State) ->
                    humanVsAIGameLoop PlayerOne (mkGameState p1State p2State aiState)
                AIVsAI ->
                    twoAISetup >>= \(p1State, p2State) ->
                    aiGameLoop PlayerOne (mkGameState p1State p2State aiState) 

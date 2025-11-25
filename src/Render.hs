module Render where

import Data.Char
import Data.Maybe
import Game
import Common
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Interact

data Phase = Placing Player | Playing Player | Won Player

data RenderState = MkRenderState {
    rsGameState :: GameState,
    rsMessage :: String,
    rsWinner :: Maybe Player
}

windowWidth :: Float
windowWidth = 1600

windowHeight :: Float
windowHeight = 800

cellSize :: Float
cellSize = 35

targetBoardOffset :: (Float, Float)
targetBoardOffset = (50, -180)

colorBackground :: Color
colorBackground = makeColorI 20 25 30 255  -- Dark Slate Grey

colorOcean :: Color
colorOcean = makeColorI 40 50 60 255      -- Ocean Grey

colorShip :: Color
colorShip = makeColorI 140 140 160 255    -- Steel Grey

colorHit :: Color
colorHit = makeColorI 215 50 50 255       -- Alert Red

colorMiss :: Color
colorMiss = makeColorI 255 255 255 100    -- Transparent White

colorFog :: Color
colorFog = makeColorI 30 35 40 255        -- Fog Grey

colorText :: Color
colorText = makeColorI 200 220 240 255    -- Pale Blue

{- Exercise 9: Rendering -}
render :: RenderState -> Picture
render rs = pictures [background, uiLayout, victoryOverlay]
  where
    gs = rsGameState rs
    message = rsMessage rs
    
    -- Get Player states
    p1 = gameP1State gs
    p2 = gameP2State gs
    
    aiMisses = psOpponentMisses p2

    background = color colorBackground $ rectangleSolid 2000 2000

    uiLayout = pictures 
        [ drawTitle message
        , translate (-400) (-180) $ drawBoard "DEPLOYMENT ZONE" (renderPlayerBoard p1 aiMisses)
        , translate (fst targetBoardOffset) (snd targetBoardOffset) $ drawBoard "TARGET GRID" (renderOpponentBoard p1)
        ]

    victoryOverlay = case rsWinner rs of
        Nothing -> blank
        Just p -> translate 0 0 $ pictures
            [ color (makeColorI 0 0 0 200) $ rectangleSolid 600 200
            , color green $ scale 0.4 0.4 $ translate (-350) (-20) $ text (show p ++ " WINS!")
            ]

drawBoard :: String -> Picture -> Picture
drawBoard label boardPic = pictures
    [ translate 15 420 $ scale 0.15 0.15 $ color colorText $ text label
    , boardPic
    ]

drawTitle :: String -> Picture
drawTitle msg = translate (-300) 340 $ scale 0.2 0.2 $ color colorText $ text ("> SYSTEM STATUS: " ++ msg)

renderPlayerBoard :: PlayerState -> [CoOrdinate] -> Picture
renderPlayerBoard pst opponentMisses = pictures [cells, labels]
  where
    (rows, cols) = psBoardSize pst
    cells = pictures [ drawCell r c | r <- [0..rows-1], c <- [0..cols-1] ]
    drawCell r c = 
        let x = fromIntegral c * cellSize
            y = fromIntegral (rows - 1 - r) * cellSize
            coord = (r, c)
            baseColor = case lookupShip pst coord of
                Nothing -> 
                    if coord `elem` opponentMisses 
                    then colorMiss 
                    else colorOcean
                Just (ship, _) -> 
                    if coord `elem` psSelfHits pst 
                    then colorHit 
                    else colorShip
        in translate x y $ color baseColor $ rectangleSolid (cellSize - 2) (cellSize - 2)
    labels = drawLabels rows cols

renderOpponentBoard :: PlayerState -> Picture
renderOpponentBoard pst = pictures [cells, labels]
  where
    (rows, cols) = psBoardSize pst
    cells = pictures [ drawCell r c | r <- [0..rows-1], c <- [0..cols-1] ]
    drawCell r c = 
        let x = fromIntegral c * cellSize
            y = fromIntegral (rows - 1 - r) * cellSize
            coord = (r, c)
            baseColor
                | coord `elem` psOpponentHits pst   = colorHit
                | coord `elem` psOpponentMisses pst = colorMiss
                | otherwise                         = colorFog
        in translate x y $ color baseColor $ rectangleSolid (cellSize - 2) (cellSize - 2)
    labels = drawLabels rows cols

drawLabels :: Int -> Int -> Picture
drawLabels rows cols = 
    pictures [ translate (-30) (fromIntegral (rows - 1 - r) * cellSize - 10) $ scale 0.12 0.12 $ color colorText $ text [toEnum (65 + r)] | r <- [0..rows-1] ]
    <>
    pictures [ translate (fromIntegral c * cellSize - 5) (fromIntegral rows * cellSize + 10) $ scale 0.12 0.12 $ color colorText $ text (show c) | c <- [0..cols-1] ]


{- Exercise 10: Interaction -}
handleEvents :: Event -> RenderState -> IO RenderState
handleEvents (EventKey (MouseButton LeftButton) Down _ (mx, my)) rs = 
    case rsWinner rs of
        Just _  -> return rs 
        Nothing -> 
            case getClickedCoordinate (mx, my) of
                Nothing -> return rs 
                Just coord -> playTurn coord rs
handleEvents _ rs = return rs

getClickedCoordinate :: (Float, Float) -> Maybe CoOrdinate
getClickedCoordinate (mx, my) = 
    let 
        (bx, by) = targetBoardOffset
        localX = mx - bx
        localY = my - by
        col = floor ((localX + cellSize / 2) / cellSize)
        rowInv = floor ((localY + cellSize / 2) / cellSize)
        row = 9 - rowInv
    in
        if col >= 0 && col <= 9 && row >= 0 && row <= 9
        then Just (row, col)
        else Nothing

playTurn :: CoOrdinate -> RenderState -> IO RenderState
playTurn coord rs = do
    let gs = rsGameState rs
    
    case fire gs PlayerOne coord of
        Left err -> return rs { rsMessage = "ERROR: " ++ show err }
        Right (humanResult, gsAfterHuman) -> do
            
            case checkWin gsAfterHuman of
                Just winner -> return rs { rsGameState = gsAfterHuman, rsWinner = Just winner, rsMessage = "VICTORY ACHIEVED!" }
                Nothing -> do
                    
                    let aiState = gameAIState gsAfterHuman
                    let (aiCoord, newAIState) = makeRandomAIMove aiState (gameP1State gsAfterHuman)
                    let gsWithAI = gsAfterHuman { gameAIState = newAIState }
                    
                    case fire gsWithAI PlayerTwo aiCoord of
                        Left _ -> 
                            return rs { rsGameState = gsWithAI, rsMessage = "You: " ++ show humanResult ++ " | AI: Forfeited" }
                        Right (aiResult, gsFinal) -> do
                            
                            case checkWin gsFinal of
                                Just winner -> return rs { rsGameState = gsFinal, rsWinner = Just winner, rsMessage = "DEFEAT: " ++ show winner ++ " Wins" }
                                Nothing -> 
                                    let msg = "You: " ++ show humanResult ++ "  |  AI: " ++ show aiResult
                                    in return rs { rsGameState = gsFinal, rsMessage = msg }

mkRenderState :: GameState -> RenderState
mkRenderState gs = MkRenderState { rsGameState = gs, rsMessage = "COMMANDER: AWAITING ORDERS", rsWinner = Nothing }

gameLoop :: GameState -> IO ()
gameLoop gs =
    interactIO
        (InWindow "Battleships" (floor windowWidth, floor windowHeight) (0, 0))
        colorBackground
        (mkRenderState gs)
        (return . render)
        handleEvents
        (const (return ()))
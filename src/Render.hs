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

boardSpacing :: Float
boardSpacing = 100

boardOffset :: (Int, Int) -> (Float, Float)
boardOffset (r, c) = 
    let w = fromIntegral c * cellSize
        h = fromIntegral r * cellSize
    in (-w / 2, -h / 2)

{- Exercise 9: Rendering -}
render :: RenderState -> Picture
render rs = pictures [title, p1Board, p2Board, victoryText]
  where
    gs = rsGameState rs
    message = rsMessage rs
    
    title = translate (-200) 300 $ scale 0.2 0.2 $ color black $ text ("Status: " ++ message)

    p1Board = translate (-300) 0 $ pictures 
        [ translate (-50) 200 $ scale 0.15 0.15 $ color black $ text "Your Fleet"
        , renderPlayerBoard (gameP1State gs)
        ]

    p2Board = translate 100 0 $ pictures
        [ translate (-50) 200 $ scale 0.15 0.15 $ color black $ text "Target Grid"
        , renderOpponentBoard (gameP1State gs) -- Note: We use P1's state to see what P1 knows!
        ]

    victoryText = case rsWinner rs of
        Nothing -> blank
        Just p -> translate (-150) (-300) $ scale 0.3 0.3 $ color red $ text (show p ++ " Wins!")

renderPlayerBoard :: PlayerState -> Picture
renderPlayerBoard pst = pictures [drawGrid, drawCells]
  where
    (rows, cols) = psBoardSize pst
    
    drawGrid = color black $ pictures [
        line [(c * cellSize, 0), (c * cellSize, fromIntegral rows * cellSize)] | c <- [0..fromIntegral cols]] 
        <> pictures [
        line [(0, r * cellSize), (fromIntegral cols * cellSize, r * cellSize)] | r <- [0..fromIntegral rows]]

    drawCells = pictures [ drawCell r c | r <- [0..rows-1], c <- [0..cols-1] ]

    drawCell :: Int -> Int -> Picture
    drawCell r c = 
        let 
            x = fromIntegral c * cellSize + cellSize / 2
            y = fromIntegral (rows - 1 - r) * cellSize + cellSize / 2 -- Flip Y so A is top
            coord = (r, c)
            
            fillColor = case lookupShip pst coord of
                Nothing -> blue -- Water
                Just (ship, _) -> 
                    if coord `elem` psSelfHits pst then red else greyN 0.5 -- Ship (Hit vs Safe)
        in
            translate x y $ color fillColor $ rectangleSolid (cellSize - 5) (cellSize - 5)

renderOpponentBoard :: PlayerState -> Picture
renderOpponentBoard pst = pictures [drawGrid, drawCells]
  where
    (rows, cols) = psBoardSize pst
    
    drawGrid = color black $ pictures [
        line [(c * cellSize, 0), (c * cellSize, fromIntegral rows * cellSize)] | c <- [0..fromIntegral cols]] 
        <> pictures [
        line [(0, r * cellSize), (fromIntegral cols * cellSize, r * cellSize)] | r <- [0..fromIntegral rows]]

    drawCells = pictures [ drawCell r c | r <- [0..rows-1], c <- [0..cols-1] ]

    drawCell :: Int -> Int -> Picture
    drawCell r c = 
        let 
            x = fromIntegral c * cellSize + cellSize / 2
            y = fromIntegral (rows - 1 - r) * cellSize + cellSize / 2
            coord = (r, c)
            
            fillColor
                | coord `elem` psOpponentHits pst   = red    -- Hit
                | coord `elem` psOpponentMisses pst = white  -- Miss
                | otherwise                         = cyan   -- Unknown (Fog)
        in
            translate x y $ color fillColor $ rectangleSolid (cellSize - 5) (cellSize - 5)

{- Exercise 10: Interaction -}
handleEvents :: Event -> RenderState -> IO RenderState
handleEvents _ _ = error "Fill me in"
-- handleEvents _ rs = return rs <<< Uncomment this if you want to test rendering without interaction
    
mkRenderState :: GameState -> RenderState
mkRenderState gs = MkRenderState { rsGameState = gs, rsMessage = "", rsWinner = Nothing }

gameLoop :: GameState -> IO ()
gameLoop gs =
    interactIO
        (InWindow "Battleships" (floor windowWidth, floor windowHeight) (0, 0))
        white
        (mkRenderState gs)
        (return . render)
        handleEvents
        (const (return ()))

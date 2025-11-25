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

{- Exercise 9: Rendering -}
render :: RenderState -> Picture
render _ = error "Fill me in"

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

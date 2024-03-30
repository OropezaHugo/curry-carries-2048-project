module View.UI (startUI) where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core   as Core
import           Graphics.UI.Threepenny.Canvas as Canvas
import           Graphics.UI.Threepenny.Elements
import           System.FilePath ((</>))
import           Control.Monad (forM_, void)


startUI :: IO ()
startUI = do
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "../wwwroot"
        } setup

setup :: Window -> UI ()
setup window = do
    _ <- return window # set UI.title "2048 - CurryCarries"
    titleMainPage <- UI.h2 # set UI.text "2048 - The game"

    let imagePath = "2048project\\src\\View\\2048-logo.png"
    img <- mkElement "img" # set (attr "src") imagePath

    instruction1 <- UI.label # set UI.text "1. Merge the blocks with similar value to obtain score."
    instruction2 <- UI.label # set UI.text "2. Obtain the number 2048 to win."
    instruction3 <- UI.label # set UI.text "3. Enjoy!!!"

    textColum <- Core.column [element instruction1, element instruction2, element instruction3]

    bestScoreLabel <- UI.label # set UI.text "BestScore: " # set style [("font-family", "'Courier New'"), ("color", "#2F48E0")]
    bestScore <- UI.label # set UI.text "000: " # set style [("font-family", "'Courier New'"), ("color", "#8080FF")]
    actualScoreLabel <- UI.label # set UI.text "Score: " # set style [("font-family", "'Courier New'"), ("color", "#2F48E0")]
    actualScore <- UI.label # set UI.text "000: " # set style [("font-family", "'Courier New'"), ("color", "#8080FF")]
    rowScore <- Core.row [element bestScoreLabel, element bestScore, element actualScoreLabel, element actualScore] # set style [("display", "flex"), ("align-items", "center")]

    --canvas <- UI.canvas # set UI.width 400
                       -- # set UI.height 400
    --element canvas # set style [("border", "1px solid black"), ("background-color", "#BBADA0")]

    startGame <- UI.button # set UI.text "Start game"
    _ <- getBody window #+ [element titleMainPage, element textColum, element img, element startGame, element rowScore]
    on UI.click startGame $ const $ do
        element startGame # set UI.text "Starting game... wait please"
    --drawGridLines canvas

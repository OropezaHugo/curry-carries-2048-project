module View.UI (startUI) where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core   as Core
import           Graphics.UI.Threepenny.Canvas as Canvas
import           Graphics.UI.Threepenny.Elements
import           System.FilePath ((</>))
import           Control.Monad (forM_, void)
import           MergeFunction


startUI :: IO ()
startUI = do
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "../wwwroot"
        } setup

canvasSize = 400
tileSize = 80.0

setup :: Window -> UI ()
setup window = do
    _ <- return window # set UI.title "2048 - CurryCarries"
    titleMainPage <- UI.h2 # set UI.text "2048 - The game"

    url <- UI.loadFile "image/png" "C:\\Users\\jorge\\Desktop\\JorgeHB\\Ing.SC\\5toSemestre\\Programming V\\Homework\\curry-carries-2048-project\\2048project\\src\\View\\2048-logo.png"
    img <- mkElement "img" # set UI.src url # set UI.height 40 # set UI.width 40

    instruction1 <- UI.label # set UI.text "1. Merge the blocks with similar value to obtain score."
    instruction2 <- UI.label # set UI.text "2. Obtain the number 2048 to win."
    instruction3 <- UI.label # set UI.text "3. Enjoy!!!"

    textColum <- Core.column [element instruction1, element instruction2, element instruction3]

    bestScoreLabel <- UI.label # set UI.text "BestScore: " # set style [("font-family", "'Courier New'"), ("color", "#013D5A")]
    bestScore <- UI.label # set UI.text "000 " # set style [("font-family", "'Courier New'"), ("color", "#708C69")]
    actualScoreLabel <- UI.label # set UI.text "Score: " # set style [("font-family", "'Courier New'"), ("color", "#013D5A")]
    actualScore <- UI.label # set UI.text "000 " # set style [("font-family", "'Courier New'"), ("color", "#708C69")]
    rowScore <- Core.row [element bestScoreLabel, element bestScore, element actualScoreLabel, element actualScore] # set style [("display", "flex"), ("justify-content", "space-between")]

    canvas <- UI.canvas
        # set UI.height canvasSize
        # set UI.width canvasSize
        # set style [("border", "solid #013D5A 3px"), ("background", "#FCF3E3")]
    

    startGame <- UI.button # set UI.text "Start game"
    _ <- getBody window #+ [row [element titleMainPage, element img], element textColum, element startGame, element rowScore, element canvas]

    on UI.click startGame $ const $ do
        let lines = [ (100 , 0, 2, 400, "#013D5A")
                    , (200, 0, 2, 400, "#013D5A")
                    , (300, 0, 2, 400, "#013D5A")
                    , (0, 100, 400, 2, "#013D5A")
                    , (0, 200, 400, 2, "#013D5A")
                    , (0, 300, 400, 2, "#013D5A")
                    ]

        forM_ lines $ \(x,y,w,h,color) -> do
            canvas # set' UI.fillStyle (UI.htmlColor color)
            canvas # UI.fillRect (x,y) w h

        -- draw a tile
        canvas # set' UI.fillStyle (UI.htmlColor "#F4A258")
        return canvas
            # set UI.textFont    "30px sans-serif"
            # set UI.strokeStyle "#013D5A"
        canvas # UI.fillRect (10,10) 80 80
        canvas # UI.strokeText "2" (40,60)
        let initialGame = ([[2, 0, 0, 0], [0, 0, 4, 0], [0, 0, 0, 8], [0, 16, 0, 0]], 0)
        drawUpdateOnGame initialGame (element canvas)

drawTile :: Int -> (Int, Int) -> UI Element
drawTile value (x, y) = do
    canvas # set' UI.fillStyle (UI.htmlColor "#F4A258")
    canvas # set UI.textFont "30px sans-serif"
    canvas # set UI.strokeStyle "#013D5A"
    canvas # UI.fillRect (fromIntegral x, fromIntegral y) 80 80
    canvas # UI.strokeText (show value) (fromIntegral (x + 30), fromIntegral (y + 50))
    return canvas

drawBoard :: Board -> UI Element
drawBoard board = do
    sequence_ [drawTile value (x * 100, y * 100) | (y, row) <- zip [0..] board, (x, value) <- zip [0..] row]
    where
        tileSize = 80

drawUpdateOnGame :: Game -> UI Element -> UI Element
drawUpdateOnGame (board, _) canvas = do
    element canvas # UI.clearCanvas
    drawBoard board
module View.POC (main2) where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core   as Core
import           Graphics.UI.Threepenny.Canvas as Canvas
import           Graphics.UI.Threepenny.Elements
import           System.FilePath ((</>))
import           Control.Monad (forM_, void)
import           MergeFunction
import           Data.List (transpose)


main2 :: IO ()
main2 = do
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "../wwwroot"
        } setup

canvasSize = 400
tileSize = 80.0

setup :: Window -> UI ()
setup window = do
    _ <- return window # set UI.title "2048 - CurryCarries"
    titleMainPage <- UI.h2 # set UI.text "2048 - The game" # set style [("font-family", "'gill sans, georgia'"), ("color", "#013D5A"), ("text-align", "center")]

    instruction1 <- UI.label # set UI.text "1. Merge the blocks with similar value to obtain score."
    instruction2 <- UI.label # set UI.text "2. Obtain the number 2048 to win."
    instruction3 <- UI.label # set UI.text "3. Enjoy!!!" 

    textColum <- Core.column [element instruction1, element instruction2, element instruction3]

    canvas <- UI.canvas
        # set UI.height canvasSize
        # set UI.width canvasSize
        # set style [("border", "solid #013D5A 3px"), ("background", "#FCF3E3")]

    startGame <- UI.button # set UI.text "Start game"
    _ <- getBody window #+ [column [element titleMainPage, element textColum,row [element startGame], element canvas]] # set style [("display", "flex"), ("justify-content", "center"), ("flex-direction", "row")]
    
    let drawTile value (x, y) = do
            if value /= 0
                then do
                    return canvas # set UI.textFont "30px sans-serif"
                    canvas # UI.fillRect (fromIntegral (x + 10), fromIntegral (y + 10)) 80 80    
                    canvas # UI.strokeText (show value) (fromIntegral (x + 40), fromIntegral (y + 60))
                    return canvas
                else
                    return canvas

    let drawBoard board = do
            sequence_ [drawTile value (x * 100, y * 100) | (y, row) <- zip [0..] board, (x, value) <- zip [0..] row]
            where
                tileSize = 80

    let drawUpdateOnGame (board, score) canvas = do
            element canvas # set UI.width canvasSize
                           # set UI.height canvasSize
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

            drawBoard board

    on UI.click startGame $ const $ do        
        let initialGame = ([[2, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]], 0)
        drawUpdateOnGame initialGame canvas

    on UI.hover startGame $ const $ do
        element startGame # set UI.text "Let's play!"

    on UI.keydown startGame $ \c -> do
        let initialGame = ([[2, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]], 0)
        if c == 39 || c == 68 || c == 100
            then do
                drawUpdateOnGame (moveRight initialGame) canvas
        else if c == 37 || c == 97 || c == 65
            then do
                drawUpdateOnGame (moveLeft initialGame) canvas
        else if c == 38 || c == 87 || c == 119
            then do
                drawUpdateOnGame (moveUp initialGame) canvas
        else if c == 40 || c == 83 || c == 115
            then do
                drawUpdateOnGame (moveDown initialGame) canvas
        else
            return ()



moveRight :: ([[Int]], Int) -> ([[Int]], Int)
moveRight (board, score) = (newBoard, score)
    where
        moveRow :: [Int] -> [Int]
        moveRow row = let nonZeros = filter (/= 0) row
                          zeros = replicate (length row - length nonZeros) 0
                      in zeros ++ nonZeros

        newBoard = Prelude.map moveRow board

moveLeft :: ([[Int]], Int) -> ([[Int]], Int)
moveLeft (board, score) = (newBoard, score)
    where
        moveRow :: [Int] -> [Int]
        moveRow row = let nonZeros = filter (/= 0) row
                          zeros = replicate (length row - length nonZeros) 0
                      in nonZeros ++ zeros

        newBoard = Prelude.map moveRow board

moveUp :: ([[Int]], Int) -> ([[Int]], Int)
moveUp (board, score) = (newBoard, score)
    where
        moveRow :: [[Int]] -> [[Int]]
        moveRow = transpose . Prelude.map moveRow' . transpose
            where
                moveRow' row = let nonZeros = filter (/= 0) row
                                   zeros = replicate (length row - length nonZeros) 0
                               in nonZeros ++ zeros

        newBoard = moveRow board

moveDown :: ([[Int]], Int) -> ([[Int]], Int)
moveDown (board, score) = (newBoard, score)
    where
        moveRow :: [[Int]] -> [[Int]]
        moveRow = transpose . Prelude.map moveRow' . transpose
            where
                moveRow' row = let nonZeros = filter (/= 0) row
                                   zeros = replicate (length row - length nonZeros) 0
                               in replicate (length row - 1) 0 ++ nonZeros ++ zeros

        newBoard = moveRow (reverse board)



getBackgroundColor value | value == 2    = "#F4A258"
                         | otherwise     = "#FCB300"

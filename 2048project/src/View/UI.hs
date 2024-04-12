module View.UI (startUI) where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core   as Core
import           Graphics.UI.Threepenny.Canvas as Canvas
import           Graphics.UI.Threepenny.Elements
import           System.FilePath ((</>))
import           Control.Monad (forM_, void, when)
import           System.Random
import           MovementHandler
import           DataHandler
import           Data.IORef
import           SaveHighscore


startUI :: IO ()
startUI = do
    gameStateRef <- newIORef ([[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]], 0)
    highscoreRef <- newIORef =<< getHighscore
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "../wwwroot"
        } (setup gameStateRef highscoreRef)

canvasSize = 400
tileSize = 80.0

setup :: IORef Game -> IORef Int -> Window -> UI ()
setup gameStateRef highscoreRef window = do
    _ <- return window # set UI.title "2048 - CurryCarries"
    titleMainPage <- UI.h2 # set UI.text "2048 - The game" # set style [("font-family", "'gill sans, georgia'"), ("color", "#013D5A"), ("text-align", "center")]

    instruction1 <- UI.label # set UI.text "1. Merge the blocks with similar value to obtain score." # set style styleNormalText
    instruction2 <- UI.label # set UI.text "2. Obtain the number 2048 to win." # set style styleNormalText
    instruction3 <- UI.label # set UI.text "3. Enjoy!!!" # set style styleNormalText

    textColum <- Core.column [element instruction1, element instruction2, element instruction3]

    bestScoreLabel <- UI.label # set UI.text "BestScore: " # set style styleLabelScore
    highscore <- liftIO $ readIORef highscoreRef
    bestScore <- UI.label # set UI.text (show highscore) # set style styleScoreBoard
    actualScoreLabel <- UI.label # set UI.text "Score: " # set style styleLabelScore
    actualScore <- UI.label # set UI.text "0" # set style styleLabelScore

    canvas <- UI.canvas
        # set UI.height canvasSize
        # set UI.width canvasSize
        # set style [("border", "solid #013D5A 3px"), ("background", "#FCF3E3")]

    startGame <- UI.button # set UI.text "Start game" # set style styleButton
    _ <- getBody window #+ [column [element titleMainPage, element textColum, row [element startGame], row [element actualScoreLabel, element actualScore], row [element bestScoreLabel, element bestScore], element canvas]] # set style [("display", "flex"), ("justify-content", "center"), ("flex-direction", "row")]
        
    let drawTile value (x, y) = do
            if value /= 0
                then do
                    canvas # set' UI.fillStyle (UI.htmlColor (getBackgroundColor value))
                    return canvas # set UI.textFont "30px sans-serif"
                    return canvas # set UI.strokeStyle (getTextColor value)
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
            element actualScore # set UI.text (show score)
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
        highscore <- liftIO $ readIORef highscoreRef
        liftIO $ writeNewHighscore highscore
        element startGame # set UI.text "Restart"
        let initialGame = ([[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]], 0)
        liftIO $ writeIORef gameStateRef initialGame
        gen <- newStdGen
        let finalGame = moveAndInsertRandom initialGame gen
        liftIO $ writeIORef gameStateRef finalGame
        drawUpdateOnGame finalGame canvas

    on UI.keydown startGame $ \c -> do
        gameState <- liftIO $ readIORef gameStateRef
        highscore <- liftIO $ readIORef highscoreRef
        gen <- liftIO newStdGen

        let handleMove moveFunc = do
                let newGameState = moveFunc gameState
                let finalGame = moveAndInsertRandomTileIfPossible gameState newGameState gen
                let (board, score) = finalGame
                liftIO $ writeIORef gameStateRef finalGame
                when (score > highscore) $ do
                    liftIO $ writeNewHighscore score
                    liftIO $ writeIORef highscoreRef score
                element bestScore # set UI.text (show highscore)
                drawUpdateOnGame finalGame canvas

        case c of
            39 -> handleMove moveRight  
            68 -> handleMove moveRight
            100 -> handleMove moveRight

            37 -> handleMove moveLeft 
            97 -> handleMove moveLeft
            65 -> handleMove moveLeft

            38 -> handleMove moveUp  
            87 -> handleMove moveUp
            119 -> handleMove moveUp

            40 -> handleMove moveDown  
            83 -> handleMove moveDown
            115 -> handleMove moveDown

            _ -> return ()

getBackgroundColor value | value == 2    = "#F4A258"
                         | value == 4    = "#708C69"
                         | value == 8    = "#BDD3CE"
                         | value == 16   = "#013D5A"
                         | value == 32   = "#6f524e"
                         | value == 64   = "#997f87"
                         | value == 128  = "#c99983"
                         | value == 256  = "#FFBE98"
                         | value == 512  = "#141414"
                         | value == 1024 = "#DF1B3F"
                         | value == 2048 = "#19204E"
                         | otherwise     = "#FCB300"

getTextColor :: Int -> String
getTextColor value | value == 2    = "#FFFFFF"
                   | value == 4    = "#FFFFFF"
                   | value == 8    = "#013D5A"
                   | value == 16   = "#FFFFFF"
                   | value == 32   = "#FFFFFF"
                   | value == 64   = "#FFFFFF"
                   | value == 128  = "#000000"
                   | value == 256  = "#000000"
                   | value == 512  = "#FFFFFF"
                   | value == 1024 = "#FFFFFF"
                   | value == 2048 = "#FFFFFF"
                   | otherwise     = "#FFFFFF"

styleButton :: [(String, String)]
styleButton = [("padding-left", "10px"),
               ("padding-right", "10px"),
               ("padding-top", "5px"),
               ("padding-bottom", "5px"),
               ("margin", "5px"),
               ("border-radius", "8px"),
               ("background-color", "#FCF3E3"),
               ("border-color", "#FCF3E3"),
               ("color", "#013D5A"),
               ("box-shadow", "0px 3px 4px rgba(1, 61, 90, 0.5)")]                   

styleLabelScore :: [(String, String)]
styleLabelScore = [("font-family", "'Courier New'"), ("color", "#013D5A")]

styleScoreBoard :: [(String, String)]           
styleScoreBoard = [("font-family", "'Courier New'"), ("color", "#708C69")]

styleNormalText :: [(String, String)]
styleNormalText = [("font-family", "'gill sans , georgia'")]

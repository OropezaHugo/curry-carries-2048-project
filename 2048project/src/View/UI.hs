module View.UI (startUI) where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core   as Core
import           Graphics.UI.Threepenny.Elements
import           Control.Monad (forM_, when, Monad (return))
import           System.Random
import           MovementHandler
import           DataHandler
import           Data.IORef
import           SaveHighscore
import           View.Styles
import           GameConditions

startUI :: IO ()
startUI = do
    gameStateRef <- newIORef ([[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]], 0)
    highscoreRef <- newIORef =<< getHighscore
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "../wwwroot"
        } (setup gameStateRef highscoreRef)

canvasSize :: Int
canvasSize = 400

setup :: IORef Game -> IORef Int -> Window -> UI ()
setup gameStateRef highscoreRef window = do
    _ <- return window # set UI.title "2048 - CurryCarries"
    titleMainPage <- UI.h2 # set UI.text "2048 - The game" # set style styleLabelTitle
    instruction1 <- UI.label # set UI.text "1. Merge the blocks with similar value to obtain score." # set style styleNormalText
    instruction2 <- UI.label # set UI.text "2. Obtain the number 2048 to win." # set style styleNormalText
    instruction3 <- UI.label # set UI.text "3. Enjoy!!!" # set style styleNormalText

    textColum <- Core.column [element instruction1, element instruction2, element instruction3]

    popupWindow <- UI.div #. "popup-window" # set style stylePopupWindow
    popupContent <- UI.div #. "popup-content" # set style stylePopupContent
    popupTitle <- UI.h1 # set UI.text "Game Over" # set UI.style stylePopupText
    popupSubTitle <- UI.h2 # set UI.text "The board is full and there are no more moves to make :(" # set UI.style stylePopupText
    popupButtonRestart <- UI.button # set UI.text "Restart Game" 
        # set style (styleButton ++ [("margin-left", "345px")])

    popupWindowWin <- UI.div #. "popup-window-win" # set style stylePopupWindow
    popupContentWin <- UI.div #. "popup-content-win" # set style stylePopupContent
    popupTitleWin <- UI.h1 # set UI.text "You Win!" # set UI.style stylePopupText
    popupSubTitleWin <- UI.h2 # set UI.text "Congratulations! You have reached 2048!" # set UI.style stylePopupText
    popupButtonContinue <- UI.button # set UI.text "Continue Game" 
        # set style (styleButton ++ [("margin-left", "300px")])

    element popupWindow #+ [column [element popupTitle, element popupSubTitle, element popupButtonRestart]]

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
    _ <- getBody window #+ [column [element titleMainPage, element textColum, row [element startGame],
                            row [element actualScoreLabel, element actualScore], row [element bestScoreLabel, element bestScore],
                            element canvas]] # set style styleButtonStart

    let drawTile value (x, y) = do
            if value /= 0
                then do
                    canvas # set' UI.fillStyle (UI.htmlColor (getBackgroundColor value))
                    _ <- return canvas # set UI.textFont "30px sans-serif"
                    _ <- return canvas # set UI.strokeStyle (getTextColor value)
                    canvas # UI.fillRect (fromIntegral (x + 10), fromIntegral (y + 10)) 80 80
                    canvas # UI.strokeText (show value) (fromIntegral (x + (getTextTilePosition value)), fromIntegral (y + 60))
                    return canvas
                else
                    return canvas

    let drawBoard board = do
            sequence_ [drawTile value (x * 100, y * 100) | (y, row) <- zip [0..] board, (x, value) <- zip [0..] row]

    let drawUpdateOnGame (board, score) canvas = do
            _ <- element actualScore # set UI.text (show score)
            _ <- element canvas # set UI.width canvasSize
                           # set UI.height canvasSize
            let lines = getGridLines

            forM_ lines $ \(x,y,w,h,color) -> do
                canvas # set' UI.fillStyle (UI.htmlColor color)
                canvas # UI.fillRect (x,y) w h

            drawBoard board
    
    getBody window #+ [element popupWindow]

    on UI.click startGame $ const $ do
        highscore <- liftIO $ readIORef highscoreRef
        liftIO $ writeNewHighscore highscore
        _ <- element startGame # set UI.text "Restart"
        let initialGame = ([[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]], 0)
        liftIO $ writeIORef gameStateRef initialGame
        gen <- newStdGen
        let finalGame = moveAndInsertRandom initialGame gen
        liftIO $ writeIORef gameStateRef finalGame
        drawUpdateOnGame finalGame canvas

    on UI.click popupButtonRestart $ const $ do
        element popupWindow # set style [("display", "none")]
        let initialGame = ([[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]], 0)
        liftIO $ writeIORef gameStateRef initialGame
        gen <- newStdGen
        let finalGame = moveAndInsertRandom initialGame gen
        liftIO $ writeIORef gameStateRef finalGame
        drawUpdateOnGame finalGame canvas
        
    on UI.click popupButtonContinue $ const $ do
        element popupWindowWin # set style [("display", "none")]

    on UI.keydown startGame $ \c -> do
        gameState <- liftIO $ readIORef gameStateRef
        highscore <- liftIO $ readIORef highscoreRef
        gen <- liftIO newStdGen

        let handleMove moveFunc = do
                let newGameState = moveFunc gameState
                let finalGame = moveAndInsertRandomTileIfPossible gameState newGameState gen
                let (_, score) = finalGame
                liftIO $ writeIORef gameStateRef finalGame
                when (score > highscore) $ do
                    liftIO $ writeNewHighscore score
                    liftIO $ writeIORef highscoreRef score
                _ <- element bestScore # set UI.text (show highscore)
                drawUpdateOnGame finalGame canvas
                when (isLostGame finalGame) $ do
                    _ <- element popupWindow # set UI.style [("display", "flex")]
                    return ()
                when (isWinGame finalGame) $ do
                    _ <- element popupWindowWin # set UI.style [("display", "flex")]
                    return ()

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

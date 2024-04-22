module View.UI (startUI) where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core   as Core
import           Graphics.UI.Threepenny.Elements()
import           Control.Monad (forM_, when, unless)
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
    winContinuedRef <- liftIO $ newIORef False
    isGamePausedRef <- liftIO $ newIORef False
    previousStateRef <- liftIO $ newIORef Nothing
    canUndoRef <- liftIO $ newIORef False

    _ <- return window # set UI.title "2048 - CurryCarries"

    titleMainPage <- UI.h1 # set UI.text "2048 - Game" # set style styleLabelTitle
    instruction1 <- UI.h3 # set UI.text "1. Merge the blocks with similar value to obtain score." # set style styleNormalText
    instruction2 <- UI.h3 # set UI.text "2. Obtain the number 2048 to win." # set style styleNormalText
    instruction3 <- UI.h3 # set UI.text "3. Enjoy!!!" # set style styleNormalText
    emptySpace <- UI.h3 # set UI.text "" # set style emptySpace

    textColum <- Core.column [element instruction1, element instruction2, element instruction3, element emptySpace]

    popupWindow <- UI.div #. "popup-window" # set style stylePopupWindow
    popupTitle <- UI.h1 # set UI.text "Game Over" # set UI.style stylePopupText
    popupSubTitle <- UI.h2 # set UI.text "The board is full and there are no more moves to make :(" # set UI.style stylePopupText
    popupButtonRestart <- UI.img # set UI.src "https://i.postimg.cc/sgLGj3J0/undo-arrow.png" # set style (styleButton ++ [("width", "20px"), ("height", "20px"), ("margin-left", "345px")])

    popupWindowWin <- UI.div #. "popup-window-win" # set style stylePopupWindow
    popupTitleWin <- UI.h1 # set UI.text "You Win!" # set UI.style stylePopupText
    popupSubTitleWin <- UI.h2 # set UI.text "Congratulations! You have reached 2048!" # set UI.style stylePopupText
    popupButtonContinue <- UI.img # set UI.src "https://i.postimg.cc/zX0RQCHN/play-1.png" # set style (styleButton ++ [("width", "20px"), ("height", "20px"), ("margin-left", "240px")])

    _ <- element popupWindow #+ [column [element popupTitle, element popupSubTitle, element popupButtonRestart]]
    _ <- element popupWindowWin #+ [column [element popupTitleWin, element popupSubTitleWin, element popupButtonContinue]]

    bestScoreLabel <- UI.label # set UI.text "Best Score" # set style styleLabelScore
    highscore <- liftIO $ readIORef highscoreRef
    bestScore <- UI.label # set UI.text (show highscore) # set style styleScoreBoard
    actualScoreLabel <- UI.label # set UI.text "Current Score" # set style styleLabelScore
    actualScore <- UI.label # set UI.text "0" # set style styleScoreBoard

    rowMenuContainer <- UI.div #. "row-menu-conatiner" # set style menuStyle

    canvas <- UI.canvas
        # set UI.height canvasSize
        # set UI.width canvasSize
        # set style [("border", "solid #7d7577 3px"), ("background", "#ffffff"), ("margin-top", "25px")]

    undoMove <- UI.img # set UI.src "https://i.postimg.cc/GtrVnvfP/return-1.png" # set style styleButton 
    startGame <- UI.img # set UI.src "https://i.postimg.cc/zX0RQCHN/play-1.png" # set style styleButton

    let scoreColumn1 = column [element bestScoreLabel, element bestScore] # set style styleScoreBackground
    let scoreColumn2 = column [element actualScoreLabel, element actualScore] # set style styleScoreBackground
    let buttonsColumn = column [element startGame, element undoMove] # set style buttonsColumnStyle

    element rowMenuContainer #+ [buttonsColumn, scoreColumn1, scoreColumn2]

    let mainContainer = UI.div #. "mainContainer" #+ [row [element titleMainPage], row [element textColum], element rowMenuContainer,
                                    element canvas] # set style mainContainerStyle

    _ <- getBody window #+ [mainContainer] # set style [("justify-content", "center")]

    let drawTile tileValue (x, y) = do
            if tileValue /= 0
                then do
                    canvas # set' UI.fillStyle (UI.htmlColor (getBackgroundColor tileValue))
                    _ <- return canvas # set UI.textFont "30px sans-serif"
                    _ <- return canvas # set UI.strokeStyle (getTextColor tileValue)
                    canvas # UI.fillRect (fromIntegral (x + 10), fromIntegral (y + 10)) 80 80
                    canvas # UI.strokeText (show tileValue) (fromIntegral (x + getTextTilePosition tileValue), fromIntegral (y + 60))
                    return canvas
                else
                    return canvas

    let drawBoard board = do
            sequence_ [drawTile tileValue (x * 100, y * 100) | (y, row) <- zip [0..] board, (x, tileValue) <- zip [0..] row]

    let drawUpdateOnGame (board, score) canvas = do
            _ <- element actualScore # set UI.text (show score)
            _ <- element canvas # set UI.width canvasSize
                           # set UI.height canvasSize
            let lines = getGridLines

            forM_ lines $ \(x,y,w,h,color) -> do
                canvas # set' UI.fillStyle (UI.htmlColor color)
                canvas # UI.fillRect (x,y) w h
            
            drawBoard board

    _ <- getBody window #+ [element popupWindow, element popupWindowWin]

    on UI.click undoMove $ const $ do
        canUndo <- liftIO $ readIORef canUndoRef
        when canUndo $ do
            previousState <- liftIO $ readIORef previousStateRef
            case previousState of
                Nothing -> return ()
                Just lastState -> do
                    liftIO $ writeIORef gameStateRef lastState
                    liftIO $ writeIORef canUndoRef False
                    liftIO $ writeIORef winContinuedRef False
                    winContinued <- liftIO $readIORef winContinuedRef
                    when (isWinGame lastState winContinued) $do
                        liftIO $writeIORef winContinuedRef True
                        return()
                    drawUpdateOnGame lastState canvas

    on UI.click startGame $ const $ do
        highscore <- liftIO $ readIORef highscoreRef
        liftIO $ writeNewHighscore highscore
        _ <- element startGame # set UI.src "https://i.postimg.cc/sgLGj3J0/undo-arrow.png" # set style styleButton
        let initialGame = ([[0, 0, 0, 0], [0, 0, 0, 0], [0, 1024, 1024, 0], [0, 0, 0, 0]], 0)
        liftIO $ writeIORef gameStateRef initialGame
        gen <- newStdGen
        let finalGame = moveAndInsertRandom initialGame gen
        liftIO $ writeIORef gameStateRef finalGame
        liftIO $ writeIORef winContinuedRef False

        drawUpdateOnGame finalGame canvas

    on UI.click popupButtonRestart $ const $ do
        _ <- element popupWindow # set style [("display", "none")]
        let initialGame = ([[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]], 0)
        liftIO $ writeIORef gameStateRef initialGame
        gen <- newStdGen
        let finalGame = moveAndInsertRandom initialGame gen
        liftIO $ writeIORef gameStateRef finalGame
        drawUpdateOnGame finalGame canvas

    on UI.click popupButtonContinue $ const $ do
        _ <- element popupWindowWin # set style [("display", "none")]
        liftIO $ writeIORef winContinuedRef True
        liftIO $ writeIORef isGamePausedRef False

    body <- getBody window
    _ <- element body # set style bodyStyle

    on UI.keydown body $ \c -> do
        isGamePaused <- liftIO $ readIORef isGamePausedRef

        unless isGamePaused $ do
            gameState <- liftIO $ readIORef gameStateRef
            highscore <- liftIO $ readIORef highscoreRef
            winContinued <- liftIO $ readIORef winContinuedRef
            currentState <- liftIO $ readIORef gameStateRef
            liftIO $ writeIORef previousStateRef (Just currentState) 
            liftIO $ writeIORef canUndoRef True
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
                    when (isWinGame finalGame winContinued) $ do
                        _ <- element popupWindowWin # set UI.style [("display", "flex")]
                        liftIO $ writeIORef isGamePausedRef True
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

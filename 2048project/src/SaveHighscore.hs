module SaveHighscore (getHighscore, writeNewHighscore) where
    import System.IO
    import System.IO.Unsafe (unsafePerformIO)
    
    highscorePath :: String
    highscorePath = "src/resources/highscore.txt"

    getIOHighScore :: IO String
    getIOHighScore = openFile highscorePath  ReadMode >>= \cont -> hGetContents cont

    getStringHighscore :: String
    getStringHighscore = unsafePerformIO getIOHighScore

    getHighscore :: IO Int
    getHighscore = do
        highScoreStr <- getIOHighScore
        return (read highScoreStr)


    writeNewHighscore :: Int -> IO()
    writeNewHighscore highscore = do writeFile highscorePath (show highscore)

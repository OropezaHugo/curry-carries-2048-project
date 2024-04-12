module SaveHighscore (getHighscore, writeNewHighscore) where
    import System.IO
    
    highscorePath :: String
    highscorePath = "src/resources/highscore.txt"

    getIOHighScore :: IO String
    getIOHighScore = openFile highscorePath  ReadMode >>= \cont -> hGetContents cont

    getHighscore :: IO Int
    getHighscore = do
        highScoreStr <- getIOHighScore
        return (read highScoreStr)


    writeNewHighscore :: Int -> IO()
    writeNewHighscore highscore = do writeFile highscorePath (show highscore)

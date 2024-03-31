module MovmentsManager(game) where 

    import DataManager
    import MergeFunction
    import RandomGeneratorFunction

    moveRight :: Game -> IO Game
    moveRight ([], score) = return ([], score)
    moveRight (x:xs, score) = do
        let (newRow, newScore) = summonRow (moveEmptySpaces (reverse x)) score
        (newBoard, newTotalScore) <- moveRight (xs, newScore)
        return (reverse newRow : newBoard, newTotalScore)
    
    moveLeft :: Game -> IO Game
    moveLeft ([], score) = return ([], score)
    moveLeft (x:xs, score) = do
        let (newRow, newScore) = summonRow (moveEmptySpaces x) score
        (newBoard, newTotalScore) <- moveLeft (xs, newScore)
        return (newRow : newBoard, newTotalScore)
    
    moveUp :: Game -> IO Game
    moveUp ([], score) = return ([], score)
    moveUp (board, score) = do
        let rotatedBoard = rotateBoard board
        (newBoard, newScore) <- moveLeft (rotatedBoard, score)
        return (rotateBoard newBoard, newScore)
    
    moveDown :: Game -> IO Game
    moveDown ([], score) = return ([], score)
    moveDown (board, score) = do
        let rotatedBoard = rotateBoard board
        (newBoard, newScore) <- moveRight (rotatedBoard, score)
        return (rotateBoard newBoard, newScore)
    
    moveAndInsertRandom :: (Game -> IO Game) -> Game -> IO Game
    moveAndInsertRandom moveFunc game = do
        movedGame@(newBoard, _) <- moveFunc game
        newBoard' <- insertRandomTile newBoard
        return (newBoard', snd movedGame)
        
module MergeFunction where

    import RandomGenerator
    
    type Board = [[Int]]
    type Score = Int
    type Game = (Board, Score)
    
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

    summonRow :: [Int] -> Score -> ([Int], Score)
    summonRow [] score = ([], score)
    summonRow [x] score = ([x], score)
    summonRow (x:xs) score
        | x == head xs = (x + head xs : fst (summonRow (tail xs) (score + (x + head xs))) ++ [0], snd (summonRow (tail xs) (score + (x + head xs))))
        | otherwise = (x : fst (summonRow xs score), snd (summonRow xs score))
    
    moveEmptySpaces :: [Int] -> [Int]
    moveEmptySpaces [] = []
    moveEmptySpaces [x] = [x]
    moveEmptySpaces (x:xs)
        | x == 0 = moveEmptySpaces xs ++ [0]
        | otherwise = x : moveEmptySpaces xs
    
    rowToCol :: [Int] -> Board
    rowToCol [] = []
    rowToCol (x:xs) = [[x]] ++ rowToCol xs
    
    rowsToCols :: [Int] -> Board -> Board
    rowsToCols [] mat = mat
    rowsToCols row [] = rowToCol row
    rowsToCols (x:xs) (y:ys) = [y ++ [x]] ++ rowsToCols xs ys
    
    rotateBoard :: Board -> Board
    rotateBoard [] = []
    rotateBoard board = rotateBoardaux board []
    
    rotateBoardaux :: Board -> Board -> Board
    rotateBoardaux [] aux = aux
    rotateBoardaux [x] aux = rowsToCols x aux
    rotateBoardaux (x:xs) [] = rotateBoardaux xs (rowsToCols x [])
    rotateBoardaux (x:xs) aux = rotateBoardaux xs (rowsToCols x aux)
    
    printBoard :: Board -> IO ()
    printBoard [] = return ()
    printBoard (row:rows) = do
        putStrLn (unwords (map show row))
        printBoard rows
    
    printGame :: Game -> IO ()
    printGame (board, score) = do
        putStrLn "Board:"
        printBoard board
        putStrLn $ "Score: " ++ show score
    
    main :: IO ()
    main = do
        let initialGame = ([[2, 0, 2, 4], [4, 2, 0, 2], [0, 4, 0, 4], [4, 4, 2, 0]], 0)
        putStrLn "Initial Game State:"
        printGame initialGame
        putStrLn "---------------------------------"
        movedGame <- moveAndInsertRandom moveRight initialGame
        putStrLn "Game State After Moving Left:"
        printGame movedGame
        putStrLn "---------------------------------"
        movedGame <- moveAndInsertRandom moveRight initialGame
        putStrLn "Game State After Moving Right:"
        printGame movedGame

        

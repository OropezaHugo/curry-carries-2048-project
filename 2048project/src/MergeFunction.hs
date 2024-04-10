module MergeFunction (Board, Score, Game) where
    
    import System.Random (randomRIO)

    type Board = [[Int]]
    type Score = Int
    type Game = (Board, Score)

    moveRight :: Game -> Game
    moveRight ([], score) = ([], score)
    moveRight (x:xs, score) = (reverse (fst (summonRow (moveEmptySpaces (reverse x)) score)) : fst (moveRight (xs, score)), 
                                snd (summonRow (moveEmptySpaces (reverse x)) (if snd (moveRight (xs, score)) /= score then score + snd (moveRight (xs, score)) else score)))

    moveLeft :: Game -> Game
    moveLeft ([], score) = ([], score)
    moveLeft (x:xs, score) = (fst (summonRow (moveEmptySpaces x) score) : fst (moveLeft (xs, score)), 
                                snd (summonRow (moveEmptySpaces x) (if snd (moveLeft (xs, score)) /= score then score + snd (moveLeft (xs, score)) else score)))

    moveUp :: Game -> Game
    moveUp ([], score) = ([], score)
    moveUp (board, score) = (rotateBoard (fst (moveLeft (rotateBoard board, score))), snd (moveLeft (rotateBoard board, score)))

    moveDown :: Game -> Game
    moveDown ([], score) = ([], score)
    moveDown (board, score) = (rotateBoard (fst (moveRight (rotateBoard board, score))), snd (moveRight (rotateBoard board, score)))
    
    moveAndInsertRandom :: Game -> IO Game
    moveAndInsertRandom game = do
        movedGame <- ioGame game
        let (newBoard, newScore) = movedGame
        newBoardWithRandom <- insertRandomTile newBoard
        return (newBoardWithRandom, newScore)

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

    -- Functions to insert a random piece in board

    randomTile :: IO Int
    randomTile = do
        rand <- randomRIO (0, 1) :: IO Int
        return $ if rand == 0 then 2 else 4
    
    emptyPositions :: Board -> [(Int, Int)]
    emptyPositions board = [(r, c) | (r, row) <- zip [0..] board, (c, val) <- zip [0..] row, val == 0]
    
    insertRandomTile :: Board -> IO Board
    insertRandomTile board = do
        let empties = emptyPositions board
        if null empties
            then return board
            else do
                (row, col) <- fmap (empties !!) $ randomRIO (0, length empties - 1)
                newValue <- randomTile
                let newRow = take col (board !! row) ++ [newValue] ++ drop (col + 1) (board !! row)
                return $ take row board ++ [newRow] ++ drop (row + 1) board

    -- IO Parser
    ioGame :: Game -> IO Game
    ioGame game@(board, score) = return game

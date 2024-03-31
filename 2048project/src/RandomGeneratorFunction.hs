module RandomGeneratorFunction (randomTile, 
                                emptyPositions, 
                                insertRandomTile) where

    import System.Random (randomRIO)
    import Data.List (elemIndices) 
    import DataManager
    
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
    

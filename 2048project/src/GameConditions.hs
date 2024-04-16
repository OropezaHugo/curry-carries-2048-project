module GameConditions (isWinGame, isLostGame) where

    import DataHandler
    import Data.List

    isWinGame :: Game -> Bool -> Bool
    isWinGame (board, score) winContinued =
        if winContinued then False else checkBoard board

    checkBoard :: Board -> Bool
    checkBoard [] = False 
    checkBoard (row:rows) = 
        if checkRow row then True else checkBoard(rows) 

    checkRow :: [Int] -> Bool
    checkRow [] = False 
    checkRow (cell:cells) =
        if cell == 2048 then True else checkRow(cells)

    isLostGame :: Game -> Bool
    isLostGame (board, _) = isBoardFull board && not (canMakeMoves board)

    isBoardFull :: Board -> Bool
    isBoardFull board = all (all (/= 0)) board

    canMakeMoves :: Board -> Bool
    canMakeMoves board =
        any canMergeRows (board ++ transpose board)

    canMergeRows :: [Int] -> Bool
    canMergeRows [] = False
    canMergeRows [_] = False
    canMergeRows (x:y:rest) = (x == y) || canMergeRows (y:rest)

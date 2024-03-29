module MergeFunction where

    type Board = [[Int]]

    moveRigth :: Board -> Board
    moveRigth [] = []
    moveRigth (x:xs) = reverse (summonRow (moveEmptySpaces (reverse x))) : moveRigth xs

    moveLeft :: Board -> Board
    moveLeft [] = []
    moveLeft (x:xs) = summonRow (moveEmptySpaces x) : moveLeft xs

    moveUp :: Board -> Board
    moveUp [] = []
    moveUp board = rotateBoard (moveLeft (rotateBoard board))

    moveDown :: Board -> Board
    moveDown [] = []
    moveDown board = rotateBoard (moveRigth (rotateBoard board))
    
    summonRow :: [Int] -> [Int]
    summonRow [] = []
    summonRow [x] = [x]
    summonRow (x:xs)    | x == head xs = x + head xs : summonRow (tail xs) ++ [0]
                        | otherwise = x : summonRow xs
    moveEmptySpaces :: [Int] -> [Int]
    moveEmptySpaces [] = []
    moveEmptySpaces [x] = [x]
    moveEmptySpaces (x:xs)  | x == 0 = moveEmptySpaces xs ++ [0]
                            | otherwise = x: moveEmptySpaces xs

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
    
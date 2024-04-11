module MovementHandler (moveLeft, moveRight, moveUp, moveDown, moveAndInsertRandom)where

    import DataHandler
    import MergeFunction
    import RandomGeneratorFunction
    import System.Random

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
    
    moveAndInsertRandom :: Game -> StdGen -> Game
    moveAndInsertRandom (board, score) gen = (newBoardWithRandom, score)
        where
            (newBoardWithRandom, _) = insertRandomTile board gen

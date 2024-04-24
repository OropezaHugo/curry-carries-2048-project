module RandomGeneratorFunction(insertRandomTile) where

    import DataHandler
    import System.Random

    randomTile :: RandomGen g => g -> Int
    randomTile gen = let (rand, _) = randomR (0.0, 1.0) gen
                    in if rand < (0.5 :: Double) then 2 else 4

    emptyPositions :: Board -> [(Int, Int)]
    emptyPositions board = [(r, c) | (r, row) <- zip [0..] board, (c, val) <- zip [0..] row, val == 0]
    
    insertRandomTile :: RandomGen g => Board -> g -> (Board, g)
    insertRandomTile board gen =
      let empty = emptyPositions board
          (index, newGen) = randomR (0, length empty - 1) gen
          (r, c) = empty !! index
          tile = randomTile newGen
          newRow = take c (board !! r) ++ [tile] ++ drop (c + 1) (board !! r)
          newBoard = take r board ++ [newRow] ++ drop (r + 1) board
      in (newBoard, newGen)

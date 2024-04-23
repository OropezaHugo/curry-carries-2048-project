module BoardHandler(createEmptyBoard) where
    
    createEmptyBoard :: Int -> [[Int]]
    createEmptyBoard size = replicate size (replicate size 0)
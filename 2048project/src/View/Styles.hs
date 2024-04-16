module View.Styles (getTextColor, 
               getTextTilePosition, 
               getBackgroundColor,
               styleButton,
               styleLabelScore,
               styleNormalText,
               styleScoreBoard,
               getGridLines,
               styleButtonStart,
               styleLabelTitle) where 

    getBackgroundColor value | value == 2    = "#F4A258"
                         | value == 4    = "#708C69"
                         | value == 8    = "#BDD3CE"
                         | value == 16   = "#013D5A"
                         | value == 32   = "#6f524e"
                         | value == 64   = "#997f87"
                         | value == 128  = "#c99983"
                         | value == 256  = "#FFBE98"
                         | value == 512  = "#141414"
                         | value == 1024 = "#DF1B3F"
                         | value == 2048 = "#19204E"
                         | otherwise     = "#FCB300"

    getTextColor :: Int -> String
    getTextColor value | value == 2    = "#FFFFFF"
                    | value == 4    = "#FFFFFF"
                    | value == 8    = "#013D5A"
                    | value == 16   = "#FFFFFF"
                    | value == 32   = "#FFFFFF"
                    | value == 64   = "#FFFFFF"
                    | value == 128  = "#000000"
                    | value == 256  = "#000000"
                    | value == 512  = "#FFFFFF"
                    | value == 1024 = "#FFFFFF"
                    | value == 2048 = "#FFFFFF"
                    | otherwise     = "#FFFFFF"

    styleButton :: [(String, String)]
    styleButton = [("padding-left", "10px"),
                ("padding-right", "10px"),
                ("padding-top", "5px"),
                ("padding-bottom", "5px"),
                ("margin", "5px"),
                ("border-radius", "8px"),
                ("background-color", "#FCF3E3"),
                ("border-color", "#FCF3E3"),
                ("color", "#013D5A"),
                ("box-shadow", "0px 3px 4px rgba(1, 61, 90, 0.5)")] 
    styleButtonStart :: [(String, String)]
    styleButtonStart = [("display", "flex"),
                        ("justify-content", "center"),
                        ("flex-direction", "row")]

    styleLabelTitle :: [(String, String)]
    styleLabelTitle = [("font-family", "'gill sans, georgia'"),
                       ("color", "#013D5A"),
                       ("text-align", "center")]                  

    styleLabelScore :: [(String, String)]
    styleLabelScore = [("font-family", "'Courier New'"), ("color", "#013D5A")]

    styleScoreBoard :: [(String, String)]           
    styleScoreBoard = [("font-family", "'Courier New'"), ("color", "#708C69")]

    styleNormalText :: [(String, String)]
    styleNormalText = [("font-family", "'gill sans , georgia'")]

    getTextTilePosition :: Int -> Int
    getTextTilePosition x = if x > 1000 then 17 else if x > 100 then 24 else if x > 10 then 34 else 41

    getGridLines :: [(Double, Double, Double, Double, String)]
    getGridLines = 
        [ (100, 0, 2, 400, "#013D5A")
        , (200, 0, 2, 400, "#013D5A")
        , (300, 0, 2, 400, "#013D5A")
        , (0, 100, 400, 2, "#013D5A")
        , (0, 200, 400, 2, "#013D5A")
        , (0, 300, 400, 2, "#013D5A")
        ]

module View.Styles (getTextColor,
               getTextTilePosition,
               getBackgroundColor,
               styleButton,
               styleLabelScore,
               styleNormalText,
               styleScoreBoard,
               getGridLines,
               styleButtonStart,
               styleLabelTitle,
               stylePopupWindow,
               stylePopupContent,
               stylePopupText,
               styleScoreBackground) where

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
                        ("flex-direction", "row"),
                        ("margin-top","25px"),
                        ("margin-bottom","25px")]

    styleLabelTitle :: [(String, String)]
    styleLabelTitle = [("font-family", "'gill sans, georgia'"),
                       ("color", "#490e0c"),
                       ("text-align", "Left")]

    styleLabelScore :: [(String, String)]
    styleLabelScore = [("font-family", "'Courier New'"), 
                       ("color", "#FFFFFF"),
                       ("margin", "10px")]

    styleScoreBoard :: [(String, String)]
    styleScoreBoard = [("font-family", "'Courier New'"), ("color", "#FFFFFF"), ("margin-left","25px")]

    stylePopupText :: [(String, String)]
    stylePopupText = [("font-family", "'Courier New'"),
                      ("color", "#FCF3E3"),
                      ("text-align", "center")]

    styleNormalText :: [(String, String)]
    styleNormalText = [("font-family", "'gill sans , georgia'"),
                       ("margin-bottom","10px"),
                       ("margin-top", "10px")]

    getTextTilePosition :: Int -> Int
    getTextTilePosition x
                      | x > 1000 = 17
                      | x > 100 = 24
                      | x > 10 = 34
                      | otherwise = 41

    getGridLines :: [(Double, Double, Double, Double, String)]
    getGridLines =
        [ (100, 0, 2, 400, "#7d7577")
        , (200, 0, 2, 400, "#7d7577")
        , (300, 0, 2, 400, "#7d7577")
        , (0, 100, 400, 2, "#7d7577")
        , (0, 200, 400, 2, "#7d7577")
        , (0, 300, 400, 2, "#7d7577")
        ]
    stylePopupWindow :: [(String, String)]
    stylePopupWindow = [("display", "none"),
                        ("position", "fixed"),
                        ("z-index", "1"),
                        ("left", "0"),
                        ("top", "0"),
                        ("width", "100%"),
                        ("height", "100%"),
                        ("overflow", "auto"),
                        ("background-color", "rgba(0,0,0,0.4)"),
                        ("justify-content", "center"),
                        ("align-items", "center")]

    stylePopupContent :: [(String, String)]
    stylePopupContent = [("background-color", "#fefefe"),
                         ("margin", "45% auto"),
                         ("padding", "20px"),
                         ("border", "1px solid #888"),
                         ("width", "30%")]
    
    styleScoreBackground :: [(String, String)]
    styleScoreBackground = [("background-color", "#6f524e"),
                            ("margin-left","30px"),
                            ("border-radius", "10px")]

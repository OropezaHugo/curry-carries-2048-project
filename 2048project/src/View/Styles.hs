module View.Styles (getTextColor,
               getTextTilePosition,
               getBackgroundColor,
               styleButton,
               styleLabelScore,
               emptySpace,
               bodyStyle,
               buttonsColumnStyle,
               styleNormalText,
               styleScoreBoard,
               getGridLines,
               menuStyle,
               mainContainerStyle,
               styleLabelTitle,
               stylePopupWindow,
               stylePopupContent,
               stylePopupText,
               styleScoreBackground) where

    getBackgroundColor value | value == 2    = "#DCDCDD"
                             | value == 4    = "#CBDEEC"
                             | value == 8    = "#BBDEFB"
                             | value == 16   = "#90CAF9"
                             | value == 32   = "#64B5F6"
                             | value == 64   = "#42A5F5"
                             | value == 128  = "#2196F3"
                             | value == 256  = "#1E88E5"
                             | value == 512  = "#1565C0"
                             | value == 1024 = "#1565C0"
                             | value == 2048 = "#0D47A1"
                             | otherwise     = "#4895EF"

    getTextColor :: Int -> String
    getTextColor value | value == 2    = "#000000"
                       | value == 4    = "#000000"
                       | value == 8    = "#000000"
                       | value == 16   = "#000000"
                       | value == 32   = "#000000"
                       | value == 64   = "#FFFFFF"
                       | value == 128  = "#FFFFFF"
                       | value == 256  = "#FFFFFF"
                       | value == 512  = "#FFFFFF"
                       | value == 1024 = "#FFFFFF"
                       | value == 2048 = "#FFFFFF"
                       | otherwise     = "#FFFFFF"

    bodyStyle :: [(String, String)]
    bodyStyle = [("background", "url(https://i.postimg.cc/zfMhfSZP/Picsart-24-04-18-12-07-13-943.jpg)"),            
                ("background-size", "cover"),
                ("display", "flex"),
                ("justify-content", "center")
                ] --"background", "linear-gradient(to right, #F6F4EB, #91C8E4, #749BC2, #4682A9)"

    buttonsColumnStyle :: [(String, String)]
    buttonsColumnStyle = [("display", "flex"),
                     ("justify-content", "flex-start"),
                     ("align-items", "flex-start"),
                     ("margin-right", "65px"),
                     ("flex-direction", "column")]

    styleButton :: [(String, String)]
    styleButton = [("padding-left", "30px"),
                ("padding-right", "30px"),
                ("padding-top", "5px"),
                ("padding-bottom", "5px"),
                ("margin","5px"),
                ("width", "20px"), 
                ("height", "20px"),
                ("border-radius", "8px"),
                ("background-color", "#3282B8"),
                ("color", "#3282B8"),
                ("font-family", "sans-serif"),
                ("box-shadow", "0px 3px 4px rgba(1, 61, 90, 0.5)"),
                ("font-size", "15px"),
                ("font-weight", "bold"),
                ("transition", "all 0.3s ease"),
                ("cursor", "pointer")]

    menuStyle :: [(String, String)]
    menuStyle = [("display", "flex"),
                ("justify-content", "flex-start"),
                ("flex-direction", "flex-start"),
                ("align-content", "space-between"),
                ("margin-top","0px"),                
                ("color", "rgba(33, 161, 192, 0.7)"),
                ("margin-bottom","0px")]

    
    mainContainerStyle :: [(String, String)]
    mainContainerStyle = [("display", "flex"),
                        ("align-items", "center"),
                        ("flex-direction", "column"),                    
                        ("margin", "20px"),
                        ("padding", "0px 240px 30px 240px"),
                        ("border-radius", "20px"),
                        ("background-color", "rgba(217, 217, 217, 0.5)")]

    styleLabelTitle :: [(String, String)]
    styleLabelTitle = [("font-family", "system-ui"),
                        ("font-size", "60px"),
                        ("font-weight", "bold"),
                        ("color", "#1C2B2D"),
                        ("margin-left","30px"),
                        ("margin-bottom","16px"),
                        ("text-align", "center")]

    styleLabelScore :: [(String, String)]
    styleLabelScore = [("display", "flex"),
                       ("justify-content", "center"),
                       ("font-family", "sans-serif"), 
                       ("font-weight", "800"),
                       ("color", "#395B64"),
                       ("margin", "10px")]

    styleScoreBoard :: [(String, String)]
    styleScoreBoard = [("font-family", "sans-serif"), 
                        ("font-size", "20px"),
                        ("font-weight", "800"),
                        ("color", "#1C2B2D")]

    emptySpace :: [(String, String)]
    emptySpace = [("margin-bottom","20px"),
                    ("margin-top", "0px")]

    stylePopupText :: [(String, String)]
    stylePopupText = [("font-family", "'Courier New'"),
                      ("color", "#FCF3E3"),
                      ("text-align", "center")]

    styleNormalText :: [(String, String)]
    styleNormalText = [("font-family", "sans-serif"),
                       ("font-size", "15px"),
                       ("color", "#393E46"),
                       ("margin-bottom","3px"),
                       ("margin-top", "0px")]

    getTextTilePosition :: Int -> Int
    getTextTilePosition x
                      | x > 1000 = 17
                      | x > 100 = 24
                      | x > 10 = 34
                      | otherwise = 41

    getGridLines :: [(Double, Double, Double, Double, String)]
    getGridLines =
        [ (100, 0, 2, 400, "#7F7F7F")
        , (200, 0, 2, 400, "#7F7F7F")
        , (300, 0, 2, 400, "#7F7F7F")
        , (0, 100, 400, 2, "#7F7F7F")
        , (0, 200, 400, 2, "#7F7F7F")
        , (0, 300, 400, 2, "#7F7F7F")
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
    styleScoreBackground = [("display", "flex"),
                            ("flex-direction", "column"),
                            ("justify-content", "top-center"),
                            ("align-items", "center"),
                            ("background-color", "#E8E8E8"),
                            ("margin-left","10px"),
                            ("margin-top", "5px"),
                            ("margin-bottom", "10px"),
                            ("border-radius", "10px")]

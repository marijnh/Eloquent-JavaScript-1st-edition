module Html where

data HTML = Tx String | Tg String [(String, String)] [HTML]

instance Show HTML where
    show (Tx s) = escapeHtml s
    show (Tg tag props []) = "<" ++ tag ++ (propString props) ++ "/>"
    show (Tg tag props children) = "<" ++ tag ++ (propString props) ++ ">" ++ (concatMap show children) ++ "</" ++ tag ++ ">"

tg tag children = Tg tag [] children
                  
propString [] = ""
propString ((prop, value):props) = " " ++ prop ++ "=\"" ++ (escapeHtml value) ++ "\"" ++ (propString props)

escapeHtml = concatMap fixChar
    where fixChar '<' = "&lt;"
          fixChar '>' = "&gt;"
          fixChar '&' = "&amp;"
          fixChar '"' = "&quot;"
          fixChar c = [c]

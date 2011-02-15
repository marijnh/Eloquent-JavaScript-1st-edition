module Main where

import Data.Time.Clock
import Data.Time.Calendar
import System(getArgs)
import Data.Char
import Data.Maybe
import Data.List hiding (stripPrefix)
import Data.Ord
import Control.Monad
import Control.Monad.State

import Html
import Highlight
import UTF8

main = do args <- getArgs
          renderFile (args !! 0)

-- Utils

forEach [] f = return ()
forEach (x:xs) f = (f x) >> (forEach xs f)

trim pred [] = []
trim pred (c:cs) | pred c = trim pred cs
                 | otherwise = trimBack (c:cs)
    where trimBack [] = []
          trimBack (c:cs) | pred c = case trimBack cs
                                     of [] -> []
                                        remainder -> c : remainder
                          | otherwise = c : trimBack cs

trimWS = trim (oneOf "\n ")

paragraphs [] = []
paragraphs ('\n':'\n':cs) = [] : (paragraphs $ dropWhile (=='\n') cs)
paragraphs (c:cs) = case paragraphs cs
                    of [] -> [[c]]
                       (p:ps) -> (c:p):ps

alternate [] _ = []
alternate [e] _ = [e]
alternate (e:es) x = e : x : alternate es x

findIf _ [] = Nothing
findIf p (x:xs) | p x = Just x
                | otherwise = findIf p xs

oneOf = flip elem

capitalize (c:cs) = (toUpper c):cs
maybeCapitalise True cs = capitalize cs
maybeCapitalise False cs = cs

infixr 4 $>
infixr 4 $$>
f $> (fst, snd) = (f fst, snd)
c $$> (fst, snd) = (c:fst, snd)

monthNames = ["January", "February", "March", "April", "May", "June", "July", "August", "September",
              "October", "November", "December"]

formatDate (year, month, day) = (monthNames !! (month - 1)) ++ " " ++ (show day) ++ " " ++ (show year)

-- Data Structures

data Chapter = Chapter {ctype::ChapterType, num::Int, title::String, tag::String, content::[Block], footnotes::[Paragraph]}
  deriving Show

data ChapterType = Normal | Appendix
  deriving (Show, Eq)

data Block = Block [Paragraph] | Exercise String Int [Paragraph] [Paragraph]
  deriving Show

data CodeType = Regular | Invalid | Expression
  deriving Show

data Paragraph = Paragraph [Fragment] | Code CodeType String | Pre String | Quote [Fragment] |
                 List ListType [[Fragment]] | Footnote Int [Fragment] |
                 Illustration String | Picture String
  deriving Show

data ListType = Bullet | Numbered
  deriving (Show, Eq)

data Fragment = Plain String | CodeFrag String | Emp String | Keyword String Bool Bool Int | ExRef Bool String |
                ChapRef Bool String | FootRef Int | Break | Exponent String | Link String String
  deriving Show

-- Parser

parse text = chapters 1 1 (paragraphs text)
    where chapters _ _ [] = []
          chapters nc na ps@(('=':_):_) = let (chap, rest) = chapter Normal nc ps in chap : chapters (nc + 1) na rest
          chapters nc na ps@(('+':_):_) = let (chap, rest) = chapter Appendix na ps in chap : chapters nc (na + 1) rest

chapter ctype n (header:ps) = (Chapter ctype n title tag (withoutFootnotes parts) (gatherFootnotes parts), rest)
    where (title', tag') = span (/='/') (trim (oneOf "\n=+") header)
          title = trim (==' ') title'
          tag = trim (oneOf "/ ") tag'
          (parts, rest) = blocks ps

isFootnote (Footnote _ _) = True
isFootnote _ = False

withoutFootnotes = map removeNotes
    where remove = filter (not . isFootnote)
          removeNotes (Block ps) = Block $ remove ps
          removeNotes (Exercise name id eps sps) = Exercise name id (remove eps) (remove sps)

gatherFootnotes bs = concatMap findFootnotes bs
    where getFootnotes = filter isFootnote
          findFootnotes (Block ps) = getFootnotes ps
          findFootnotes (Exercise _ _ eps sps) = getFootnotes eps ++ getFootnotes sps

gatherKeywords ch = concatMap keywords' (content ch)
    where keywords' (Block ps) = concatMap keywords'' ps
          keywords' (Exercise _ _ qs as) = concatMap keywords'' (qs ++ as)
          keywords'' (Paragraph fs) = keywords fs
          keywords'' (Quote fs) = keywords fs
          keywords'' (List _ fss) = concatMap keywords fss
          keywords'' (Footnote _ fs) = keywords fs
          keywords'' _ = []
          keywords fs = filter isKeyword fs
          isKeyword (Keyword _ _ _ _) = True
          isKeyword _ = False

blocks = block (Block [])
    where block b [] = ([b], [])
          block b ps@(('=':'=':_):_) = ([b], ps)
          block b ps@(('+':'+':_):_) = ([b], ps)
          block b ("---":ps) = b $$> block (Block []) ps
          block b (('*':'*':'*':tag):ps) = b $$> block (Exercise (trim (==' ') tag) 0 [] []) ps
          block (Block xs) (p:ps) = block (Block $ xs ++ [makeParagraph p]) ps
          block (Exercise tag id qs []) ("///":p:ps) = block (Exercise tag id qs [makeParagraph p]) ps
          block (Exercise tag id qs []) (p:ps) = block (Exercise tag id (qs ++ [makeParagraph p]) []) ps
          block (Exercise tag id qs as) (p:ps) = block (Exercise tag id qs (as ++ [makeParagraph p])) ps

makeParagraph ('>':'>':' ':cs) = Code Expression (stripPrefix 2 cs)
makeParagraph ('>':' ':cs) = Code Regular (stripPrefix 2 cs)
makeParagraph ('!':'>':' ':cs) = Code Invalid (stripPrefix 3 cs)
makeParagraph (']':' ':cs) = Pre (stripPrefix 2 cs)
makeParagraph ('|':' ':cs) = Quote (splitFrag (stripPrefix 2 cs))
makeParagraph ('[':'[':'[':cs) = Picture (takeWhile (/=']') cs)
makeParagraph ('[':'[':cs) = Illustration (takeWhile (/=']') cs)
makeParagraph cs@(' ':'*':' ':_) = List Bullet (map splitFrag (linesFrom "* " cs))
makeParagraph cs@(' ':'1':'.':' ':_) = List Numbered (map splitFrag (linesFrom ". " cs))
makeParagraph ('#':'#':' ':cs) = Footnote 0 (splitFrag cs)
makeParagraph cs = Paragraph (splitFrag cs)

stripPrefix n [] = []
stripPrefix n ('\n':cs) = '\n' : (stripPrefix n $ drop' n cs)
    where drop' 0 xs = xs
          drop' _ [] = []
          drop' _ xs@('\n':_) = xs
          drop' (n+1) (x:xs) = drop' n xs
stripPrefix n (c:cs) = c : (stripPrefix n cs)

linesFrom p cs = map (removeUpto p) $ lines cs
    where removeUpto [] cs = cs
          removeUpto _ [] = []
          removeUpto (p:ps) (c:cs) | p == c = removeUpto ps cs
                                   | otherwise = removeUpto (p:ps) cs

-- Fragment parsers

codeFrag xs = case xs
              of ('|':'|':'|':xs') -> CodeFrag $> codeFrag' xs
                 ('|':'|':xs) -> (CodeFrag "|", xs)
                 otherwise -> CodeFrag $> codeFrag' xs
    where codeFrag' [] = ([], [])
          codeFrag' ('|':'|':xs) = '|' $$> '|' $$> codeFrag' xs
          codeFrag' ('|':xs) = ([], xs)
          codeFrag' (x:xs) = x $$> codeFrag' xs

empFrag xs = Emp $> empFrag' xs
    where empFrag' [] = ([], [])
          empFrag' (x:'*':xs) | isAlphaNum(x) = ([x], xs)
                              | otherwise = x $$> empFrag' ('*':xs)
          empFrag' (x:xs) = x $$> empFrag' xs

refFrag (x:xs) = ((constructor x) tag, rest)
    where (tag, rest) = span isAlphaNum xs
          constructor 'c' = ChapRef False
          constructor 'C' = ChapRef True
          constructor 'e' = ExRef False
          constructor 'E' = ExRef True

keywordFrag xs visible = makeKey $> keyFrag' xs
    where keyFrag' [] = ([], [])
          keyFrag' ('_':xs) = ([], xs)
          keyFrag' (x:xs) = x $$> keyFrag' xs
          isCode ('|':xs) = (True, take (length xs - 1) xs)
          isCode xs = (False, xs)
          makeKey x = let (code, keyword) = isCode x
                      in Keyword keyword visible code 0

expFrag xs = Exponent $> expFrag' xs
    where expFrag' [] = ([], [])
          expFrag' (x:xs) | isNumber x = x $$> expFrag' xs
                          | otherwise = ([], x:xs)

linkFrag xs = (Link (trimWS href) (trimWS title), rest')
    where getTitle [] = ([], [])
          getTitle ('|':xs) = ([], xs)
          getTitle (x:xs) = x $$> getTitle xs
          getHref [] = ([], [])
          getHref (']':xs) = ([], xs)
          getHref (x:xs) = x $$> getHref xs
          (title, rest) = getTitle xs
          (href, rest') = getHref rest

splitFrag xs = splitFrag' xs True
    where splitFrag' ('^':xs) _ = continue (expFrag xs)
          splitFrag' ('\\':'\\':xs) _ = continue (refFrag xs)
          splitFrag' ('[':xs) _ = continue (linkFrag xs)
          splitFrag' ('#':'#':xs) _ = continue (FootRef 0, xs)
          splitFrag' ('\n':'\n':xs) _ = continue (Break, xs)
          splitFrag' ('-':'-':xs) _ = add '\x2015' (splitFrag' xs True)
          splitFrag' ('@':'_':xs) _ = continue (keywordFrag xs False)
          splitFrag' ('_':x:xs) True | isAlphaNum x || x == '|' = continue (keywordFrag (x:xs) True)
                                     | otherwise = add '_' (splitFrag' (x:xs) True)
          splitFrag' ('*':x:xs) True | isAlphaNum x = continue (empFrag (x:xs))
                                     | otherwise = add '*' (splitFrag' (x:xs) True)
          splitFrag' ('|':xs) True = continue (codeFrag xs)
          splitFrag' (x:xs) _ = add x (splitFrag' xs (not (isAlphaNum x)))
          splitFrag' [] _ = []
          continue (frag, []) = [frag]
          continue (frag, xs) = frag : splitFrag' xs True
          add c (Plain xs: rest) = (Plain (c:xs)) : rest
          add c fs = (Plain [c]):fs

-- Id assignment

data Nums = Nums {exNum::Int, footNum::Int, refNum::Int, keyNum::Int}

setExNum ns num = ns {exNum = num}
setFootNum ns num = ns {footNum = num}
setRefNum ns num = ns {refNum = num}
setKeyNum ns num = ns {keyNum = num}

nextNum getter setter = do current <- get
                           let num = getter current
                           put (setter current (num + 1))
                           return num

nextExercise, nextFootnote, nextFootref :: State Nums Int
nextExercise = nextNum exNum setExNum
nextFootnote = nextNum footNum setFootNum
nextFootref = nextNum refNum setRefNum
nextKeyword = nextNum keyNum setKeyNum

numberChapter ch = ch {content = numberedContent, footnotes = numberedFootnotes}
    where (numberedContent, numbers) = runState (numberBlocks (content ch)) (Nums 1 1 1 1)
          numberedFootnotes = fst (runState (numberParagraphs (footnotes ch)) numbers)

numberBlocks bs = mapM number bs
    where number (Block ps) = do nps <- numberParagraphs ps
                                 return $ Block nps
          number (Exercise tag _ eps sps) = do exNum <- nextExercise
                                               neps <- numberParagraphs eps
                                               nsps <- numberParagraphs sps
                                               return $ Exercise tag exNum neps nsps

numberParagraphs ps = mapM number ps
    where number (Paragraph fs) = do nfs <- numberFragments fs
                                     return $ Paragraph nfs
          number (Quote fs) = do nfs <- numberFragments fs
                                 return $ Quote nfs
          number (List t fss) = do nfss <- mapM numberFragments fss
                                   return (List t nfss)
          number (Footnote _ fs) = do nfs <- numberFragments fs
                                      id <- nextFootnote
                                      return $ Footnote id nfs
          number p = return p

numberFragments fs = mapM number fs
    where number (FootRef _) = do id <- nextFootref
                                  return (FootRef id)
          number (Keyword s v c _) = do id <- nextKeyword
                                        return (Keyword s v c id)
          number f = return f

-- Rendering

findChapter chapters t = info
    where info = case (findIf ((== t) . tag) chapters)
                 of Just c -> (fileName c, (typeName c) ++ " " ++ (show (num c)))

findExercise chapters tag = info
    where hasTag (Exercise t _ _ _) = t == tag
          hasTag _ = False
          findExercise c = case (findIf hasTag (content c))
                           of Nothing -> Nothing
                              Just e -> Just (c, e)
          info = case (catMaybes (map findExercise chapters))
                 of ((c, (Exercise _ n _ _)):_) -> ((fileName c) ++ "#exercise" ++ (show n), "exercise " ++ (show (num c)) ++ "." ++ (show n))

typeName c = case (ctype c)
             of Normal -> "chapter"
                Appendix -> "appendix"
fileName c = (typeName c) ++ (show (num c)) ++ ".html"
                    
outputPath = "web/"
imgPath = "img/"

writeFileUTF8 file content = writeFile file (map (chr . fromIntegral) (encode content))
readFileUTF8 file = do input <- readFile file
                       let (chars, []) = decode (map (toEnum . ord) input)
                       return chars

renderFile file = do input <- readFileUTF8 file
                     now <- (getCurrentTime >>= return . toGregorian . utctDay)
                     let chapters = map numberChapter (parse input)
                     renderChapters chapters now
                     writeFileUTF8 (outputPath ++ "contents.html") (showPage (renderContent chapters now))
                     writeFileUTF8 (outputPath ++ "terms.html") (showPage (renderIndex chapters now))

-- "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
showPage html = (show html)

styleSheet href = Tg "link" [("rel", "stylesheet"), ("type", "text/css"), ("href", href)] []
loadScript src = Tg "script" [("type", "text/javascript"), ("src", src)] [Tx " "]
uft8Type = Tg "meta" [("http-equiv", "Content-Type"), ("content", "text/html; charset=utf-8")] []

divClass cls children = Tg "div" [("class", cls)] children
spanClass cls children = Tg "span" [("class", cls)] children
listitem content = tg "li" content

block es = divClass "block" es
anchor name = Tg "a" [("name", name)] [Tx ""]
link href text = Tg "a" [("href", href)] [Tx text]

--scripts = ["js/Mochi.js", "js/util.js", "js/select.js", "js/tokenize.js", "js/highlight.js", "js/bookutil.js", "js/initenv.js", "js/env.js", "js/book.js"];
scripts = ["js/ejs.js"];
stylesheets = ["css/book.css", "css/highlight.css", "css/console.css"];

addScripts around = loadScript "js/before.js" : around : map loadScript scripts
contentDiv = divClass "content"

page title body = tg "html" [tg "head" ((map styleSheet stylesheets) ++
                                        [uft8Type, tg "title" [Tx $ title ++ " -- Eloquent JavaScript"]]),
                             tg "body" body]

sortKeywords cs = splitByLetter (collapse (sortBy compareWord (concatMap extractKeywords cs)))
    where extractKeywords ch = map (simplifyKeyword ch) (gatherKeywords ch)
          simplifyKeyword ch (Keyword w _ c n) = (w, c, ch, n)
          sameWord x y = compareWord x y == EQ
          compareWord (w1, c1, _, _) (w2, c2, _, _) = case comparing (map (replaceSpecial . toLower)) w1 w2
                                                      of EQ -> compare c1 c2
                                                         GT -> GT
                                                         LT -> LT
              where replaceSpecial '\n' = ' '
                    replaceSpecial '|' = '@' -- to get around '|' and '{' being sorted after 'z'
                    replaceSpecial '{' = '@'
                    replaceSpecial x = x
          end (_, _, x, y) = (x, y)
          collapse [] = []
          collapse (k@(w, c, ch, n):ws) = let (same, other) = span (sameWord k) ws
                                          in (w, c, (map end (k:same))) : collapse other
          fstLetter ((x:xs), _, _) | isAlphaNum x = toLower x
                                   | otherwise = ' '
          splitByLetter [] = []
          splitByLetter (w:ws) = let (this, rest) = span ((==(fstLetter w)) . fstLetter) ws
                                 in (w:this) : splitByLetter rest

renderIndex cs now = page "Index" [contentDiv ((header : map letterList (sortKeywords cs) ++ [footer now]))]
    where renderKeyword (w, c, ls) = tg "li" (text : (concatMap renderLink ls))
              where text = case c
                           of True -> tg "code" [Tx w]
                              False -> Tx w
          renderLink (ch, n) = [Tx " ", link ((fileName ch) ++ "#key" ++ (show n)) "\x25CB"]
          letterList ws = Tg "ul" [("class", "index")] (map renderKeyword ws)
          header = tg "h1" [Tx "Index of Terms"]

renderContent cs now = page "Contents" [contentDiv [header, listHeader, makeList chapters, appendixHeader, makeList appendices, bottomLinks, footer now]]
    where header = tg "h1" [Tx "Eloquent JavaScript"]
          appendixHeader = tg "h2" [Tx "Appendices"]
          listHeader = tg "h2" [Tx "Contents"]
          chapters = filter ((==Normal) . ctype) cs
          appendices = filter ((==Appendix) . ctype) cs
          makeLink c = link (fileName c) (title c)
          makeList cs = Tg "ol" [("class", "contents")] (map (listitem . (:[]) . makeLink) cs)
          bottomLinks = tg "p" [link "terms.html" "Alphabetic index of terms", tg "br" [], link "index.html" "Cover page"]


renderChapters cs now = do let rcs = map (renderChapter' cs) cs
                           writeFileUTF8 (outputPath ++ "print.html") (showPage (renderPrintable rcs now))
                           forEach (zip [0..] (zip cs rcs)) (\c -> writeFileUTF8 (outputPath ++ (fileName (fst (snd c))))
                                                                                 (showPage (renderChapter c cs now)))

renderChapter' cs c = header : (alternate blocks sep) ++ footnotes'
    where blocks = map (renderBlock c cs) (content c)
          sep = tg "hr" []
          footnotes' | null (footnotes c) = []
                     | otherwise = [Tg "ol" [("class", "footnotes")] (map (renderParagraph cs) (footnotes c))]
          header = tg "h1" [spanClass "number" [Tx ((capitalize (typeName c)) ++ " " ++ (show (num c)) ++ ": ")], Tx (title c)]

renderChapter (pos, (c, rc)) cs now = page (title c) (addScripts content')
    where navigation = divClass "navigation" [prevChapter, Tx " | ", link "contents.html" "Contents", Tx " | ", 
                                              link "index.html" "Cover", Tx " | ", nextChapter]
          prevChapter | pos == 0 = Tx "<< Previous chapter"
                      | otherwise = link (fileName (cs !! (pos - 1))) "<< Previous chapter"
          nextChapter | (pos + 1) == (length cs) = Tx "Next chapter >>"
                      | otherwise = link (fileName (cs !! (pos + 1))) "Next chapter >>"
          tagName = Tg "script" [("type", "text/javascript")] [Tx ("var chapterTag = '" ++ (tag c) ++ "';")]
          content' = contentDiv (tagName : navigation : rc ++ [navigation,  footer now])

renderPrintable rcs now = page "Print Version" [divClass "content" (concat rcs), footer now]

footer now = divClass "footer" [Tx "\x00A9 ", link "mailto:marijnh@gmail.com" "Marijn Haverbeke",
                                Tx " (", (link "http://creativecommons.org/licenses/by/3.0/" "license"), Tx ")",
                                Tx (", written March to July 2007, last modified on " ++ (formatDate now) ++ ".")]

renderBlock c cs (Block ps) = block (map (renderParagraph cs) ps)
renderBlock c cs (Exercise _ n eps sps) = block [anchor ("exercise" ++ (show n)),
                                                 divClass "exercisenum" [Tx ("Ex. " ++ (show (num c)) ++ "." ++ (show n))],
                                                 divClass "exercise" (map (renderParagraph cs) eps),
                                                 divClass "solution" (map (renderParagraph cs) sps)]
                                          
renderParagraph cs (Paragraph fs) = tg "p" (renderFragments cs fs)
renderParagraph cs (Quote fs) = tg "blockquote" (renderFragments cs fs)
renderParagraph cs (Code codetype content) = Tg "pre" [("class", classFor codetype)] ((highlighterFor codetype) content)
    where classFor Invalid = "code invalid"
          classFor Regular = "code"
          classFor Expression = "code expression"
          highlighterFor Expression = highlightExpression
          highlighterFor _ = highlightStatements
renderParagraph cs (Pre content) = Tg "pre" [("class", "preformatted")] [Tx content]
renderParagraph cs (List t is) = tg tagname (map (listitem . (renderFragments cs)) is)
    where tagname = case t
                    of Bullet -> "ul"
                       Numbered -> "ol"
renderParagraph cs (Footnote n fs) = tg "li" $ anchor ("footnote" ++ (show n)) : (renderFragments cs fs)
renderParagraph cs (Illustration src) = divClass "illustration" [(Tg "img" [("src", imgPath ++ src)] [])]
renderParagraph cs (Picture src) = divClass "picture" [(Tg "img" [("src", imgPath ++ src)] [])]

renderFragment cs (Plain s) = [Tx s]
renderFragment cs (CodeFrag s) = [tg "code" [Tx s]]
renderFragment cs (Emp s) = [tg "em" [Tx s]]
renderFragment cs (Keyword s v c n) = case (v, c)
                                      of (False, _) -> [a]
                                         (True, False) -> [a, Tx s]
                                         (True, True) -> [a, tg "code" [Tx s]]
    where a = anchor ("key" ++ (show n))
renderFragment cs (ExRef capit tag) = [link href (maybeCapitalise capit text)]
    where (href, text) = findExercise cs tag
renderFragment cs (ChapRef capit tag) = [link href (maybeCapitalise capit text)]
    where (href, text) = findChapter cs tag
renderFragment cs (FootRef n) = [Tg "a" [("class", "footref"), ("href", "#footnote" ++ (show n))] [Tx (show n)]]
renderFragment cs Break = [tg "br" [], tg "br" []]
renderFragment cs (Exponent n) = [spanClass "exponent" [Tx n]]
renderFragment cs (Link href title) = [link href title]

renderFragments cs fs = concatMap (renderFragment cs) fs

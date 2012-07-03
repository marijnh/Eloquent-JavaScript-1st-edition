module Main where

import Data.Time.Clock
import Data.Time.Calendar
import System.Environment(getArgs)
import Data.Char
import Data.Maybe
import Data.List
import Data.Ord
import Control.Monad
import Control.Monad.State
import System.IO.UTF8

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
alternate [e] _ = e
alternate (e:es) x = e ++ x ++ (alternate es x)

findIf _ [] = Nothing
findIf p (x:xs) | p x = Just x
                | otherwise = findIf p xs

member x [] = False
member x (y:ys) = if (x == y) then True else (member x ys)

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

chapter ctype n (header:ps) = (Chapter ctype n title tag parts [], rest)
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

makeParagraph ('>':'>':' ':cs) = Code Expression (Main.stripPrefix 2 cs)
makeParagraph ('>':' ':cs) = Code Regular (Main.stripPrefix 2 cs)
makeParagraph ('!':'>':' ':cs) = Code Invalid (Main.stripPrefix 3 cs)
makeParagraph (']':' ':cs) = Pre (Main.stripPrefix 2 cs)
makeParagraph ('|':' ':cs) = Quote (splitFrag (Main.stripPrefix 2 cs))
makeParagraph ('[':'[':'[':cs) = Picture (takeWhile (/=']') cs)
makeParagraph ('[':'[':cs) = Illustration (takeWhile (/=']') cs)
makeParagraph cs@(' ':'*':' ':_) = List Bullet (map splitFrag (linesFrom "* " cs))
makeParagraph cs@(' ':'1':'.':' ':_) = List Numbered (map splitFrag (linesFrom ". " cs))
makeParagraph ('#':'#':' ':cs) = Footnote 0 (splitFrag cs)
makeParagraph cs = Paragraph (splitFrag cs)

stripPrefix n [] = []
stripPrefix n ('\n':cs) = '\n' : (Main.stripPrefix n $ drop' n cs)
    where drop' 0 xs = xs
          drop' _ [] = []
          drop' _ xs@('\n':_) = xs
          drop' n (x:xs) = drop' (n-1) xs
stripPrefix n (c:cs) = c : (Main.stripPrefix n cs)

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
          splitFrag' ('-':'-':xs) _ = add' "---" (splitFrag' xs True)
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
          add' s (Plain xs: rest) = (Plain (s ++ xs)) : rest
          add' s fs = (Plain s):fs

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

typeName c = case (ctype c)
             of Normal -> "chapter"
                Appendix -> "appendix"
fileName c = padz (show (num c)) ++ "_" ++ (tag c) ++ ".tex"
  where padz [nm] | (ctype c) == Normal = "0" ++ [nm]
                  | otherwise = "a" ++ [nm]
        padz x = x
                    
outputPath = "tex/"
imgPath = "img/"

renderFile file = do input <- System.IO.UTF8.readFile file
                     let chapters = map numberChapter (parse input)
                     renderChapters chapters

renderChapters cs = forEach cs (\c -> System.IO.UTF8.writeFile (outputPath ++ (fileName c)) (renderChapter c))

renderChapter c = header ++ (alternate blocks sep)
    where blocks = map (renderBlock c) (content c)
          sep = "\\sep\n\n"
          tp = case (ctype c) of
                  Normal -> "chapter"
                  Appendix -> "appendix"
          header = "\\" ++ tp ++ "{" ++ (title c) ++ "}\\label{chap:" ++ (tag c) ++ "}\n\n"

latexWrap' content tp arg = "\\begin" ++ (arg' arg) ++ "{" ++ tp ++ "}\n" ++ content ++ "\\end{" ++ tp ++ "}\n"
    where arg' Nothing = ""
          arg' (Just x) = "[" ++ x ++ "]"
latexWrap content tp = latexWrap' content tp Nothing

renderBlock c (Block ps) = concat (map renderParagraph ps)
renderBlock c (Exercise tag n eps sps) = (latexWrap (lbl ++ (concatMap renderParagraph eps)) "exercise") ++
                                         (latexWrap (concatMap renderParagraph sps) "solution")
    where lbl = (case tag
                   of "" -> ""
                      x -> "\\label{ex:" ++ tag ++ "}")

needEsc c = member c ['$', '{', '}', '\\', '^', '_', '&', '#', '%']
latexEsc [] = []
latexEsc ('^':cs) = "\\verb1^1" ++ (latexEsc cs)
latexEsc ('\\':cs) = "\\bs " ++ (latexEsc cs)
latexEsc (c:cs) = if (needEsc c) then ('\\':c:(latexEsc cs)) else (c:(latexEsc cs))

nonewlines [] = []
nonewlines ('\n':cs) = ' ':(nonewlines cs)
nonewlines (c:cs) = c:(nonewlines cs)

renderParagraph (Paragraph fs) = (renderFragments fs)
renderParagraph (Quote fs) = latexWrap (renderFragments fs) "quote"
renderParagraph (Code _ content) = "\\begin{Code}\n" ++ content ++ "\n\\end{Code}\n\n"
renderParagraph (Pre content) = "\\begin{verbatim}\n" ++ content ++ "\n\\end{verbatim}\n\n"
renderParagraph (List t is) = latexWrap items (blockType t)
    where blockType Bullet = "itemize"
          blockType Numbered = "enumerate"
          items = concatMap ("\\item " ++) (map renderFragments is)
renderParagraph (Footnote n fs) = "TODO footnote: " ++ (renderFragments fs)
renderParagraph (Illustration src) = "\\includegraphics{" ++ imgPath ++ src ++ "}"
renderParagraph (Picture src) = "\\includegraphics{" ++ imgPath ++ src ++ "}"

renderFragment (Plain s) = s
renderFragment (CodeFrag s) = "\\verb`" ++ (nonewlines s) ++ "`"
renderFragment (Emp s) = "{\\em " ++ s ++ "}"
renderFragment (Keyword s v c n) = index ++ content
    where index = "\\index{" ++ (latexEsc s) ++ "}"
          content = case (v, c)
                      of (False, _) -> ""
                         (True, False) -> s
                         (True, True) -> "\\verb`" ++ (nonewlines s) ++ "`"
renderFragment (ExRef capit tag) = "\\exref" ++ cap ++ "{" ++ tag ++ "}"
    where cap | capit = "[1]"
              | True = ""
renderFragment (ChapRef capit tag) = "\\chapref" ++ cap ++ "{" ++ tag ++ "}"
    where cap | capit = "[1]"
              | True = ""
renderFragment (FootRef n) = "(TODO footnote)"
renderFragment Break = "\n\n"
renderFragment (Exponent n) = "$^{" ++ n ++ "}$"
renderFragment (Link href title) = "\\href{" ++ href ++ "}{" ++ title ++ "}"

renderFragments fs = (concatMap renderFragment fs) ++ "\n\n"

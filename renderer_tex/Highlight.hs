module Highlight (highlightStatements, highlightExpression) where

import Html
import Text.ParserCombinators.Parsec
import Data.Char
import Data.Map hiding (map, null)
import Data.Set hiding (map, null)

-- Parser

data WordType = LikeIf | LikeDo | LikeNew | Function | Var | Catch | For | Case | Operator | Atom | Variable
  deriving (Show, Eq)

wordTypes :: Map String WordType
wordTypes = Data.Map.fromList([("if", LikeIf), ("switch", LikeIf), ("while", LikeIf), ("with", LikeIf),
                               ("else", LikeDo), ("do", LikeDo), ("try", LikeDo), ("finally", LikeDo),
                               ("return", LikeNew), ("break", LikeNew), ("continue", LikeNew),
                               ("new", LikeNew), ("delete", LikeNew), ("throw", LikeNew),
                               ("in", Operator), ("typeof", Operator), ("instanceof", Operator),
                               ("var", Var), ("for", For), ("case", Case), ("function", Function), ("catch", Catch),
                               ("true", Atom), ("false", Atom), ("undefined", Atom), ("null", Atom),
                               ("NaN", Atom), ("Infinity", Atom)]);

wordType :: String -> WordType
wordType word = findWithDefault Variable word wordTypes

data PState = PS [[String]] [(String, String)]

output :: String -> String -> CharParser PState ()
output text style = updateState write
    where write (PS env out) = PS env ((text, style):out)

getOutput :: CharParser PState [(String, String)]
getOutput = do (PS _ ps) <- getState
               return ps

pushContext :: CharParser PState ()
pushContext = updateState push
    where push (PS env out) = PS (["arguments", "this"]:env) out

popContext :: CharParser PState ()
popContext = updateState pop
    where pop (PS (e:es) out) = PS es out

registerVariable :: String -> CharParser PState Bool
registerVariable name = do PS env out <- getState
                           if (null env)
                              then return False
                              else do setState (PS (update env) out)
                                      return True
    where update (e:es) = ((name:e):es)

isLocalVariable :: String -> CharParser PState Bool
isLocalVariable name = do PS env _ <- getState
                          return ((any (elem name)) env)

punctuation :: Char -> CharParser PState ()
punctuation c = do char c
                   output [c] "punctuation"
                   ws

commaSep :: CharParser PState a -> CharParser PState [a]
commaSep p = sepBy p (punctuation ',')

zeroOrOne :: CharParser st t -> CharParser st [t]
zeroOrOne p = do one <- p
                 return [one]
              <|> return []

symbolOperator :: CharParser st [Char]
symbolOperator = many1 (oneOf "+-*&%/=<>!|")

wordOperator :: CharParser st [Char]
wordOperator = do name <- word
                  if (wordType name /= Operator)
                     then unexpected name
                     else return name

operator :: CharParser PState ()
operator = do op <- (symbolOperator <|> wordOperator)
              output op "operator"
              ws

keyword :: String -> CharParser PState ()
keyword name = do string name
                  output name "keyword"
                  ws

untilUnescaped :: Char -> CharParser st [Char]
untilUnescaped c = do next <- anyChar
                      if (next == c)
                         then return [next]
                         else if (next == '\\')
                              then do next' <- anyChar
                                      rest <- untilUnescaped c
                                      return (next:next':rest)
                              else do rest <- untilUnescaped c
                                      return (next:rest)

regexp, stringValue, numberValue, hexNumberValue, ws :: CharParser PState ()

regexp = do char '/'
            content <- untilUnescaped '/'
            options <- many (oneOf "ig")
            output ('/':(content ++ options)) "string"
            ws

stringValue = do delimiter <- (char '"' <|> char '\'')
                 content <- untilUnescaped delimiter
                 output (delimiter:content) "string"
                 ws

numberValue = do digits <- many1 digit
                 dot <- ((try afterDot) <|> return "")
                 exp <- (exponent <|> return "")
                 output (digits ++ dot ++ exp) "atom"
                 ws
    where afterDot = do char '.'
                        digits <- many1 digit
                        return ('.':digits)
          exponent = do exp <- oneOf "eE"
                        minus <- zeroOrOne (char '-')
                        digits <- many digit
                        return (exp:(minus ++ digits))

hexNumberValue = do x <- try (do char '0'
                                 oneOf "xX")
                    digits <- many1 (satisfy isHexDigit)
                    output ('0':x:digits) "atom"
                    ws

ws = spaces <|> (try lineComment) <|> (try comment) <|> return ()
    where spaces = do space <- many1 (satisfy isSpace)
                      output space "whitespace"
                      ws
          lineComment = do string "//"
                           content <- many (satisfy (/='\n'))
                           output ("//" ++ content) "comment"
                           ws
          comment = do string "/*"
                       content <- manyTill anyChar (try (string "*/"))
                       output ("/*" ++ content ++ "*/") "comment"
                       ws

word :: CharParser st [Char]
word = do start <- satisfy varStart
          rest <- many (satisfy varLetter)
          return (start:rest)
    where varStart c = isAlpha c || c == '_' || c == '$'
          varLetter c = varStart c || isDigit c

statement :: CharParser PState ()
statement = (punctuation ';') <|> block <|> wordStatement <|> exprStatement
    where block = do punctuation '{'
                     many statement
                     punctuation '}'
          exprStatement = do expression
                             punctuation ';'

wordStatement, wordExpression :: CharParser PState ()
wordStatement = do start <- word
                   perform start (action start)
    where perform start (Just act) = do output start "keyword"
                                        ws
                                        act
          perform start Nothing = (try (label start)) <|> (expr start)
          label start = do output start "property"
                           ws
                           punctuation ':'
          expr start = do wordExpression' start
                          punctuation ';'
          action word =
              case (wordType word)
              of LikeIf -> Just (do punctuation '('
                                    expression
                                    punctuation ')'
                                    statement)
                 LikeDo -> Just statement
                 Function -> Just functionDef
                 Var -> Just (do commaSep variableDef
                                 punctuation ';')
                 Catch -> Just (do pushContext
                                   punctuation '('
                                   newVariable
                                   punctuation ')'
                                   statement
                                   popContext)
                 For -> Just (do punctuation '('
                                 (try forIn) <|> normalFor
                                 punctuation ')'
                                 statement)
                 Case -> Just (do expression
                                  punctuation ':'
                                  statement)
                 otherwise -> Nothing
          forIn = do optional (keyword "var")
                     newVariable
                     keyword "in"
                     expression
          normalFor = do statement
                         optional expression
                         punctuation ';'
                         optional expression

wordExpression = do start <- word
                    wordExpression' start

wordExpression' :: String -> CharParser PState ()
wordExpression' start =
    case (wordType start)
    of LikeNew -> do name "keyword"
                     optional expression
       Operator -> do name "operator"
                      expression
       Function -> do name "keyword"
                      functionDef
       Atom -> do name "atom"
                  maybeOperator
       Variable -> do local <- isLocalVariable start
                      name (case local of {True -> "localvariable"; False -> "variable"})
                      maybeOperator
    where name n = do output start n
                      ws

newVariable, variableDef, functionDef, propertyName, objectLiteral, arrayLiteral,
  expression, postFixOperator, maybeOperator :: CharParser PState ()

newVariable = do name <- word
                 local <- registerVariable name
                 output name (case local of {True -> "variabledef"; False -> "variable"})
                 ws

variableDef = do newVariable
                 optional assignment
    where assignment = do punctuation '='
                          expression

functionDef = do optional newVariable
                 pushContext
                 punctuation '('
                 commaSep newVariable
                 punctuation ')'
                 statement
                 popContext

propertyName = do name <- word
                  output name "property"
                  ws

objectLiteral = do punctuation '{'
                   commaSep property
                   punctuation '}'
    where property = do (stringValue <|> numberValue <|> hexNumberValue <|> propertyName)
                        punctuation ':'
                        expression

arrayLiteral = do punctuation '['
                  commaSep expression
                  punctuation ']'

expression = do (parenthesed <|> regexp <|> stringValue <|> hexNumberValue <|> numberValue <|> 
                 objectLiteral <|> arrayLiteral <|> wordExpression)
                maybeOperator
             <|> do operator
                    expression
    where parenthesed = do punctuation '('
                           expression
                           punctuation ')'


postFixOperator = do op <- (string "++") <|> (string "--")
                     output op "operator"
                     ws

maybeOperator = funCall <|> propValue <|> propSubscript <|> (try postFixOperator) <|> choiceOperator 
                <|> operator' <|> return ()
    where operator' = do operator
                         expression
          choiceOperator = do punctuation '?'
                              expression
                              punctuation ':'
                              expression
          funCall = do punctuation '('
                       commaSep expression
                       punctuation ')'
                       maybeOperator
          propValue = do punctuation '.'
                         propertyName
                         maybeOperator
          propSubscript = do punctuation '['
                             expression
                             punctuation ']'
                             maybeOperator

-- Html generation

significantStyles :: Set String
significantStyles = Data.Set.fromList ["keyword", "atom", "variable", "string", "variabledef",
                                       "localvariable", "property", "comment"];

simplifyOutput :: [(String, String)] -> [(String, String)]
simplifyOutput output = simplify (map removeStyle output)
    where removeStyle (value, style) | Data.Set.member style significantStyles = (value, style)
                                     | otherwise = (value, "")
          simplify [] = []
          simplify [o] = [o]
          simplify ((v1, s1):rest) = let a@((v2, s2):rest') = simplify rest
                                     in if (s2 == s1)
                                           then ((v1 ++ v2, s1):rest')
                                           else ((v1, s1):a)

toHtml :: (String, String) -> HTML
toHtml (value, "") = Tx value
toHtml (value, style) = Tg "span" [("class", style)] [Tx value]

highlightOutput :: [(String, String)] -> [HTML]
highlightOutput = (map toHtml) . simplifyOutput . reverse

highlightStatements, highlightExpression :: String -> [HTML]
highlightStatements = highlight (do {ws; many statement; eof; getOutput})
highlightExpression = highlight (do {ws; expression; eof; getOutput})

highlight :: CharParser PState [(String, String)] -> String -> [HTML]
highlight parser code = case runParser parser (PS [] []) "" code
                        of Right ps -> highlightOutput ps
                           Left err -> error ("In code:\n\n" ++ code ++ "\n\nParse error at " ++ (show err))

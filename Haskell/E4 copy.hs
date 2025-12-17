import Data.Char
import Data.List
import Distribution.Compat.CharParsing (CharParsing(string))

-- DO NOT MODIFY THESE DATA TYPES
data LamMacroExpr = LamDef [(String, LamExpr)] LamExpr deriving (Eq, Show, Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Eq, Show, Read)

-------------------------------------------------------------Parsers-------------------------------------------------------------------------
-- Logging utility
logParsing :: String -> String -> IO ()
logParsing func input = putStrLn ("[Parsing] " ++ func ++ " called with input: " ++ show input)

parseLamMacro :: String -> IO (Maybe LamMacroExpr)
parseLamMacro str = do
    logParsing "parseLamMacro" str
    let strippedStr = removeWhitespace str
    if "def" `isPrefixOf` strippedStr
      then parseMacroWithDef (splitDef strippedStr)
      else do
        parsedExpr <- parseLamExprIO strippedStr
        return $ fmap (LamDef []) parsedExpr

-- Parse a macro that has a definition at the start
parseMacroWithDef :: Maybe (String, String) -> IO (Maybe LamMacroExpr)
parseMacroWithDef input = do
    logParsing "parseMacroWithDef" (show input)
    case input of
      Nothing -> return Nothing
      Just (beforeIn, afterIn) -> do
          parsedDef <- parseMacroDef beforeIn
          parsedExpr <- parseLamExprIO afterIn
          return $ LamDef <$> parsedDef <*> parsedExpr

-- Parse a macro definition
parseMacroDef :: String -> IO (Maybe [(String, LamExpr)])
parseMacroDef str = do
    logParsing "parseMacroDef" str
    case extractDefParts str of
      Nothing -> return Nothing
      Just (namePart, exprPart) -> do
          parsedExpr <- parseLamExprIO exprPart
          return $ do
              expr <- parsedExpr
              return [(namePart, expr)]

-- Parse a lambda expression
parseLamExprIO :: String -> IO (Maybe LamExpr)
parseLamExprIO str = do
    logParsing "parseLamExpr" str
    let strippedStr = removeWhitespace str
    if length strippedStr > 2 && head strippedStr == '(' && last strippedStr == ')'
      then parseLamExprIO (init (tail strippedStr))
      else if isLamMacro strippedStr
        then return $ parseLamMacroName strippedStr
        else if isLamVar strippedStr
          then return $ parseLamVar strippedStr
          else if isLamAbs strippedStr
            then parseLamAbs strippedStr
            else if isLamApp strippedStr
              then parseLamApp strippedStr
              else return Nothing

-- Parse when it's just a variable
parseLamVar :: String -> Maybe LamExpr
parseLamVar str = if all isDigit str then Just (LamVar (read str :: Int)) else Nothing

-- Parse when it's just the name of a macro
parseLamMacroName :: String -> Maybe LamExpr
parseLamMacroName str = if all isUpper str then Just (LamMacro str) else Nothing

-- Parse a lambda application
parseLamApp :: String -> IO (Maybe LamExpr)
parseLamApp str = do
    logParsing "parseLamApp" str
    let splits = validSplits str
    case find isValidAppSplit splits of
      Just (left, right) -> do
          leftExpr <- parseLamExprIO left
          rightExpr <- parseLamExprIO right
          return $ LamApp <$> leftExpr <*> rightExpr
      Nothing -> return Nothing
  where
    validSplits s = [splitAt i s | i <- [1..length s - 1]]
    isValidAppSplit (left, right) = isLamExpr left && isLamExpr right

-- Parse a lambda abstraction
parseLamAbs :: String -> IO (Maybe LamExpr)
parseLamAbs str = do
    logParsing "parseLamAbs" str
    let varPart = takeWhile isDigit (tail str)
    let restAfterArrow = drop (length varPart + 2) str
    expr <- parseLamExprIO restAfterArrow
    return $ LamAbs (read varPart :: Int) <$> expr

-------------------------------------------------------------Expression identifiers-------------------------------------------------------------------------
isLamExpr :: String -> Bool
isLamExpr str = isLamMacro str || isLamApp str || isLamAbs str || isLamVar str

-- Determine if an expression is a LamMacro
isLamMacro :: String -> Bool
isLamMacro = all isUpper

-- Determine if an expression is a LamApp
isLamApp :: String -> Bool
isLamApp str = any isValidAppSplit (validSplits str)
  where
    validSplits s = [splitAt i s | i <- [1..length s - 1]]
    isValidAppSplit (left, right) = isLamExpr left && isLamExpr right

-- Determine if an expression is a LamAbs
isLamAbs :: String -> Bool
isLamAbs str = (head str == 'λ' || head str == '\x03BB') && isLamVar varPart && (nextChar == '→' || nextChar == '\x2192')
  where
    (varPart, rest) = splitAt 2 (tail str)
    nextChar = head rest

isLamVar :: String -> Bool
isLamVar = all isDigit

-------------------------------------------------------------Helpers-------------------------------------------------------------------------
-- Break a string into before and after the 'in' part of a macro definition
splitDef :: String -> Maybe (String, String)
splitDef str = if "in" `isInfixOf` str
    then Just (before, drop 2 after)
    else Nothing
  where
    (before, after) = breakOn "in" str

-- Split a string based on a given substring
breakOn :: String -> String -> (String, String)
breakOn partToFind str = (concat before, drop (length partToFind) (concat rest))
  where
    (before, rest) = break (isPrefixOf partToFind) (tails str)

-- Get the macro expression name and body from a macro definition
extractDefParts :: String -> Maybe (String, String)
extractDefParts str =
    if "def" `isPrefixOf` str
      then let afterDef = drop 3 str
               (beforeEquals, afterEquals) = break (== '=') afterDef
            in if null afterEquals then Nothing else Just (beforeEquals, tail afterEquals)
      else Nothing

-- Remove the spaces from the input
removeWhitespace :: String -> String
removeWhitespace str = filter (not . (`elem` " ")) str

-------------------------------------------------------------Main-------------------------------------------------------------------------
main :: IO ()
main = do
    result1 <- parseLamMacro "def M = x1 in (M x2)"
    print result1
    result2 <- parseLamMacro "(x1 x2)"
    print result2
    result3 <- parseLamMacro "x3"
    print result3
    result4 <- parseLamMacro "λ1→x2"
    print result4

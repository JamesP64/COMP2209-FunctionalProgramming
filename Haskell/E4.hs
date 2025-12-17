-- James Palmer
-- jp14g23@soton.ac.uk
-- Copyright of the University of Southampton
-- 09/01/2025

-- Your imports here
import Data.Char
import Data.List

-- DO NOT MODIFY THESE DATA TYPES
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-------------------------------------------------------------Parsers-------------------------------------------------------------------------------------------
-- Main function
parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro str
-- Check if theres a 'def' at the start. If there is split it into the definition part and body part, and parse it
    | "def" `isPrefixOf` str = parseMacroWithDef (splitDef str)
-- Otherwise parse it as a normal expression with an empty LamDef in front
    | otherwise = case parseLamExpr str of
        Just expr -> Just (LamDef [] expr)
        Nothing   -> Nothing
  where
-- Whitespace is removed 
    str = removeWhitespace str


-- Parse a macro that has a definition at the start
parseMacroWithDef :: Maybe (String, String) -> Maybe LamMacroExpr
parseMacroWithDef Nothing = Nothing
parseMacroWithDef (Just (beforeIn, afterIn)) = do
    -- Parse the macro definition part
    parsedDef <- parseMacroDef beforeIn
    -- Parse the expression after the "in"
    parsedExpr <- parseLamExpr afterIn
    -- Return the LamDef with parsed macro definitions and expression
    return (LamDef parsedDef parsedExpr)

-- Parse a macro definition
parseMacroDef :: String -> Maybe [(String, LamExpr)]
parseMacroDef str = do
    -- Get the name part, and the expression part
    (namePart, exprPart) <- extractDefParts str
    -- Parse the expression part
    parsedExpr <- parseLamExpr exprPart
    -- Return the name and parsed expression
    return [(namePart, parsedExpr)]


-- Parse a lambda expression
parseLamExpr :: String -> Maybe LamExpr
parseLamExpr str
-- If its surrounded by brackets parse the middle bit
    | head str == '(' && last str == ')' = parseLamExpr (init (tail str))
-- If its a LamMacro
    | isLamMacro str = parseLamMacroName str
-- If its a LamVar
    | isLamVar str = parseLamVar str
-- If its a LamAbs
    | isLamAbs str = parseLamAbs str
-- If its a LamApp
--    | isLamApp str = parseLamApp str 
    | otherwise = parseLamApp str


-- Parse when it's just a variable
parseLamVar :: String -> Maybe LamExpr
parseLamVar str
  | all isDigit str = Just (LamVar (read str :: Int))
  | otherwise = Nothing

-- Parse when its just the name of a variable
parseLamMacroName :: String -> Maybe LamExpr
parseLamMacroName str
-- Just the string as long as it's length is 1
 | length str == 1 = Just (LamMacro str)
 | otherwise = Nothing

-- Parse application of two expressions
parseLamApp :: String -> Maybe LamExpr
parseLamApp = undefined

-- Parse a lambda abstraction
parseLamAbs :: String -> Maybe LamExpr
parseLamAbs str = do
    let varPart = takeWhile isDigit (tail str)
    let restAfterArrow = drop (length varPart + 2) str
    expr <- parseLamExpr restAfterArrow
    return (LamAbs (read varPart :: Int) expr)
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Expression identifiers----------------------------------------------------------------------------
-- Check if a string is a LamExpr
isLamExpr :: String -> Bool
isLamExpr str =
    isLamMacro str|| isLamApp str|| isLamAbs str|| isLamVar str

--Determine if an expression is a LamMacro
isLamMacro :: String -> Bool
isLamMacro =
    all isUpper

-- Determine if an expression is a LamApp
isLamApp :: String -> Bool
isLamApp str =
    any isValidAppSplit (validSplits str)
  where
    validSplits s = [splitAt i s | i <- [1..length s - 1]]
    isValidAppSplit (left, right) = isLamExpr left && isLamExpr right

-- Determine if an expression is a LamAbs
isLamAbs :: String -> Bool
isLamAbs str =
    (head str == 'λ' || head str == '\x03BB') && isLamVar varPart && (nextChar == '→' || nextChar == '\x2192')
  where
    (varPart, rest) = splitAt 2 (tail str)  -- the variable part like "x1"
    nextChar = head rest

-- Check if an expression is a LamVar
isLamVar :: String -> Bool
isLamVar =
    all isDigit
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Helpers-------------------------------------------------------------------------------------------
-- Break a string into before and after the 'in' part of a macro definition
splitDef :: String -> Maybe (String, String)
splitDef str
-- Check that 'in' is a part of the string
    | "in" `isInfixOf` str =
        --Splits String into before 'in' and the rest
        let (before, after) = breakOn "in" str
        --Drops the first 2 chars in the after bit, getting rid of 'in'
         in Just (before, drop 2 after)
    | otherwise = Nothing

-- Split a string based on a given substring
breakOn :: String -> String -> (String, String)
breakOn partToFind str =
    let (before, rest) = break (isPrefixOf partToFind) (tails str)
     in (concat before, drop (length partToFind) (concat rest))

-- Get the macro expression name and body from a macro definition
extractDefParts :: String -> Maybe (String, String)
extractDefParts str
-- If "def" is the first part of the string
    | "def" `isPrefixOf` str =
        --Take off the "def"
        let afterDef = drop 3 str
        --Break at the equals sign
            (beforeEquals, afterEquals) = break (== '=') afterDef
         in if null afterEquals
            then Nothing
            --Return before and after the equals sign, but without it
            else Just (beforeEquals, tail afterEquals)
    | otherwise = Nothing

-- Remove the spaces from the input
removeWhitespace :: String -> String
removeWhitespace =
    filter (not . (`elem` " "))
---------------------------------------------------------------------------------------------------------------------------------------------------------------


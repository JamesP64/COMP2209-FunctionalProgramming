-- James Palmer
-- jp14g23@soton.ac.uk
-- Copyright of the University of Southampton
-- 09/01/2025

-- Your imports here
import Data.List ( isInfixOf )

-- DO NOT MODIFY THESE DATATYPES
-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-------------------------------------------------------------Unparsing-----------------------------------------------------------------------------------------
-- Sorts out whether the expression has macro definitions or not
unparse :: LamMacroExpr -> String
-- Has macro definitions
unparse (LamDef [ (name,expr) ] expr2) =
    unparseLamDef (LamDef [ (name,expr) ] expr2)
-- Does not have a macro definition
unparse (LamDef [] expr) = 
    unparseLamExpr expr

-- Calls the corresponding function for each type of expression
unparseLamExpr :: LamExpr -> String
unparseLamExpr (LamVar int) =
    unparseLamVar (LamVar int)

unparseLamExpr (LamMacro str) = 
    unparseLamMacro (LamMacro str)

unparseLamExpr (LamAbs int expr) = 
    unparseLamAbs (LamAbs int expr)

-- Needs brackets if the first expression is a LamAbs
unparseLamExpr (LamApp (LamAbs int expr) expr2) = 
    "(" ++ 
    unparseLamExpr (LamAbs int expr) ++ 
    ")" ++ 
    unparseLamExpr expr2
unparseLamExpr (LamApp expr1 expr2) = 
    unparseLamExpr expr1 ++ 
    unparseLamExpr expr2

-- Variables are an x, followed by a given int
unparseLamVar :: LamExpr -> String
unparseLamVar (LamVar int) =
     "x" ++ 
     show int

-- Macros just unparse to the given letter
unparseLamMacro :: LamExpr -> String
unparseLamMacro (LamMacro str) =
     str

-- Adds notation then unparse the expression inside
unparseLamAbs :: LamExpr -> String
unparseLamAbs (LamAbs int expr) = 
    "\x03BB" ++ 
    "x" ++ 
    show int ++
    "\x2192" ++ 
    unparseLamExpr expr

-- Adds notation and unparses first expression, and then checks whether the macro is used in the body
unparseLamDef :: LamMacroExpr -> String
unparseLamDef (LamDef [ (name,expr) ] expr2) = 
    "def" ++ 
    name ++ 
    "=" ++ 
    unparseLamExpr expr ++ 
    "in" ++ 
    checkMacro name (unparseLamExpr expr) (unparseLamExpr expr2)
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Replacing already defined macros------------------------------------------------------------------
-- Main function to replace macros
checkMacro :: String -> String -> String -> String
checkMacro name macro expr =
    let
        noBracketsExpr = removeBrackets expr macro
    in
        if macro `isInfixOf` noBracketsExpr
        then replace macro name noBracketsExpr
        else expr

-- Replace all occurrences of a substring
replace :: String -> String -> String -> String
replace old new [] = []
replace old new str@(x:xs)
    | take (length old) str == old = new ++ replace old new (drop (length old) str)
    | otherwise = x : replace old new xs

-- Remove surrounding parentheses from the macro
removeBrackets :: String -> String -> String
removeBrackets expr macro =
        if ("(" ++ macro ++ ")") `isInfixOf` expr
        then replace ("(" ++ macro ++ ")") macro expr
        else expr
---------------------------------------------------------------------------------------------------------------------------------------------------------------
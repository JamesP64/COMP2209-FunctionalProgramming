-- Your imports here
import Data.List (isInfixOf)

-- DO NOT MODIFY THESE DATATYPES

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

unparse :: LamMacroExpr -> String
unparse (LamDef [ (name,expr) ] expr2) = unparseLamDef (LamDef [ (name,expr) ] expr2)
unparse (LamDef [] expr) = unparseLamExpr expr

unparseLamExpr :: LamExpr -> String
unparseLamExpr (LamVar int) = unparseLamVar (LamVar int)
unparseLamExpr (LamMacro str) = unparseLamMacro (LamMacro str)
unparseLamExpr (LamAbs int expr) = unparseLamAbs (LamAbs int expr)
unparseLamExpr (LamApp (LamAbs int expr) expr2) = "(" ++ unparseLamExpr (LamAbs int expr) ++ ")" ++ unparseLamExpr expr2
unparseLamExpr (LamApp expr1 expr2) = unparseLamExpr expr1 ++ unparseLamExpr expr2


unparseLamVar :: LamExpr -> String
unparseLamVar (LamVar int) = "x" ++ show int

unparseLamMacro :: LamExpr -> String
unparseLamMacro (LamMacro str) = str

unparseLamAbs :: LamExpr -> String
unparseLamAbs (LamAbs int expr) =  "\x03BB" ++ "x" ++ show int ++ "\x2192" ++ unparseLamExpr expr

unparseLamDef :: LamMacroExpr -> String
unparseLamDef (LamDef [ (name,expr) ] expr2) = "def" ++ name ++ "=" ++ unparseLamExpr expr ++ "in" ++ checkMacro name (unparseLamExpr expr) (unparseLamExpr expr2)


checkMacro :: String -> String -> String -> String
checkMacro name macro expr =
    let
        cleanExpr = stripParentheses expr macro
    in
        if macro `isInfixOf` cleanExpr
        then replace macro name cleanExpr
        else expr

-- Helper function to replace all occurrences of a substring
replace :: String -> String -> String -> String
replace old new [] = []
replace old new str@(x:xs)
    | take (length old) str == old = new ++ replace old new (drop (length old) str)
    | otherwise = x : replace old new xs

-- Helper function to remove surrounding parentheses from the macro
stripParentheses :: String -> String -> String
stripParentheses expr macro =
    let
        macroWithParens = "(" ++ macro ++ ")"
    in
        if macroWithParens `isInfixOf` expr
        then replace macroWithParens macro expr
        else expr
-- James Palmer
-- jp14g23@soton.ac.uk
-- Copyright of the University of Southampton
-- 09/01/2025

-- Your imports here

-- DO NOT MODIFY THESE DATATYPES
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-------------------------------------------------------------Transform LamMacroExpr----------------------------------------------------------------------------
-- This section splits the input into a list of macros, and the body of the expression
-- It transforms both parts and puts them together with the LamDef

-- Main function
cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform (LamDef macros body) =
    LamDef (transformMacros macros) (snd (transformLamExpr 10 body))

-- Transform a list of macros
transformMacros :: [(String, LamExpr)] -> [(String, LamExpr)]
transformMacros macros =
    [(name, snd (transformLamExpr 10 expr)) | (name, expr) <- macros]
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Transform LamExpr---------------------------------------------------------------------------------
-- This section transforms LamExprs
-- I put each type of LamExpr in a seperate function to make it look simpler
-- The transformLamExpr brings them together by using pattern matching to call the relevant function
-- An int is passed throughout to keep fresh variables


-- Transform a LamExpr to CPS style
transformLamExpr :: Int -> LamExpr -> (Int,LamExpr)

transformLamExpr newVar (LamVar x) =
    transformLamVar newVar (LamVar x)

transformLamExpr newVar (LamMacro str) =
    transformLamMacro newVar (LamMacro str)

transformLamExpr newVar (LamAbs x expr) =
    transformLamAbs newVar (LamAbs x expr)

transformLamExpr newVar (LamApp expr1 expr2) =
    transformLamApp newVar expr1 expr2

-- Transform a LamVar to CPS style
transformLamVar :: Int -> LamExpr -> (Int,LamExpr)
transformLamVar newVar (LamVar x) =
    (
        newVar+1,
        LamAbs newVar (LamApp (LamVar newVar) (LamVar x))
    )

-- Transform a LamMacro to CPS style 
transformLamMacro :: Int -> LamExpr -> (Int,LamExpr)
transformLamMacro newVar (LamMacro str) =
    (
        newVar,
        LamMacro str
    )

-- Transform a LamAbs to CPS style
transformLamAbs :: Int -> LamExpr -> (Int,LamExpr)
transformLamAbs newVar (LamAbs x expr) =
    (
        newVar+1,
        LamAbs newVar (LamApp (LamVar newVar) (LamAbs x (snd (transformLamExpr (newVar+1) expr))))
    )

-- Transform a LamApp to CPS style
transformLamApp :: Int -> LamExpr -> LamExpr -> (Int,LamExpr)
transformLamApp newVar expr1 expr2 =
    -- Transform the inner expressions first to avoid variable clashes
    let (newNewVar , transformedExpr1) = 
         transformLamExpr (newVar + 1) expr1
        (newNewNewVar , transformedExpr2) =
             transformLamExpr (newNewVar + 1) expr2
        result = 
            LamAbs newNewVar (LamApp transformedExpr1 (LamAbs newNewNewVar (LamApp transformedExpr2 (LamAbs (newNewNewVar + 1) (LamApp (LamApp (LamVar newNewNewVar) (LamVar (newNewNewVar + 1))) (LamVar newNewVar))))))
    in (newNewNewVar + 2, result)
---------------------------------------------------------------------------------------------------------------------------------------------------------------

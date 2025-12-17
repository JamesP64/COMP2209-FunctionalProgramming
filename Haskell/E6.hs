-- James Palmer
-- jp14g23@soton.ac.uk
-- Copyright of the University of Southampton
-- 09/01/2025

-- Your imports here

-- DO NOT MODIFY THESE DATA TYPES
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

-------------------------------------------------------------Main function-------------------------------------------------------------------------
-- Main function that calls doReductionSequence on the 4 cases
compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
compareInnerOuter expr maxSteps = 
    ( addMacrosToSteps (doReductionSequence innerRedn1 lamExpr maxSteps) expr,
      addMacrosToSteps (doReductionSequence outerRedn1 lamExpr maxSteps) expr, 
      addMacrosToSteps (doReductionSequence innerRedn1 cpsExpr maxSteps) expr,
      addMacrosToSteps (doReductionSequence outerRedn1 cpsExpr maxSteps) expr
    )
  where
    lamExpr = replaceDefs expr 
    -- Using snd here because transformLamExpr returns a tuple for fresh variables
    cpsExpr = LamApp (snd(transformLamExpr 10 lamExpr)) (LamAbs 0 (LamVar 0))

-- Add on number of macro definitions to final result
addMacrosToSteps :: Maybe Int -> LamMacroExpr -> Maybe Int
addMacrosToSteps (Just steps) expr = Just (steps + countMacros expr)
addMacrosToSteps Nothing _ = Nothing

-- How many macros are there
countMacros :: LamMacroExpr -> Int
countMacros (LamDef macros _) = length macros


-- Perform the reduction sequence on an expression
doReductionSequence :: (LamExpr -> Maybe LamExpr) -> LamExpr -> Int -> Maybe Int
doReductionSequence reductionFunc expr maxSteps = countSteps expr 0
  where
    countSteps e steps
    -- Over the max number of steps
      | steps > maxSteps =
         Nothing  
      | otherwise = case reductionFunc e of
        -- Done reducing
          Nothing -> Just steps 
        -- One more step
          Just e' -> countSteps e' (steps + 1)
--------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Replace Macro Definitions-------------------------------------------------------------------------
-- This section turns the LamMacroExpr to a LamExpr, putting the macros into the expression, instead of just their names

-- Function to replace LamMacros with their definitions in the body
replaceDefs :: LamMacroExpr -> LamExpr
replaceDefs (LamDef macros body) =
  replaceInBody macros body

-- Helper function to replace macros in the body
replaceInBody :: [(String, LamExpr)] -> LamExpr -> LamExpr
replaceInBody macros (LamVar x) =
  LamVar x
replaceInBody macros (LamAbs x body) =
  LamAbs x (replaceInBody macros body)
replaceInBody macros (LamApp e1 e2) =
  LamApp (replaceInBody macros e1) (replaceInBody macros e2)
replaceInBody macros (LamMacro name) =
  case lookup name macros of
    Just expr -> expr
    Nothing -> LamMacro name

--------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Reductions-------------------------------------------------------------------------

rename :: Int -> LamExpr -> Int
rename x e
 | free (x + 1) e = rename (x + 1) e
 | otherwise = x + 1

free :: Int -> LamExpr -> Bool
free x (LamVar y) = x == y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x /= y = free x e
free x (LamApp e1 e2) = free x e1 || free x e2

subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e |
 x /= y && not (free x e) = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e |
 x /=y && (free x e) = let x' = (rename x e1) in
 subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e | x == y = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e)

outerRedn1 :: LamExpr -> Maybe LamExpr
outerRedn1 (LamAbs x e) = 
  case outerRedn1 e of
    Just e' -> Just (LamAbs x e')
    Nothing -> Nothing
outerRedn1 (LamApp (LamAbs x e1) e2) = Just (subst e1 x e2)
outerRedn1 (LamApp e1 e2) =
  case outerRedn1 e1 of
    Just e1' -> Just (LamApp e1' e2) 
    Nothing  ->
      case outerRedn1 e2 of
        Just e2' -> Just (LamApp e1 e2')
        Nothing  -> Nothing
outerRedn1 (LamVar x) = Nothing 
outerRedn1 _ = Nothing


innerRedn1 :: LamExpr -> Maybe LamExpr
innerRedn1 (LamAbs x e) = 
    case innerRedn1 e of
        Just e' -> Just (LamAbs x e')
        Nothing -> Nothing
innerRedn1 (LamApp (LamAbs x e1) e2) = 
    Just (subst e1 x e2)
innerRedn1 (LamApp e1 e2) =
    case innerRedn1 e1 of
        Just e1' -> Just (LamApp e1' e2)
        Nothing  -> 
            case innerRedn1 e2 of
                Just e2' -> Just (LamApp e1 e2')
                Nothing  -> Nothing
innerRedn1 (LamVar x) = Nothing
innerRedn1 _ = Nothing
---------------------------------------------------------------------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------Code from Exercise 5---------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Transform LamMacroExpr----------------------------------------------------------------------------

-------------------------------------------------------------Transform LamMacroExpr----------------------------------------------------------------------------
-- This section splits the input into a list of macros, and the body of the expression
-- It transforms both parts and puts them together with the LamDef

-- Main function
cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform (LamDef macros body) = 
    LamDef (transformMacros macros) (snd(transformLamExpr 10 body))

-- Transform a list of macros
transformMacros :: [(String, LamExpr)] -> [(String, LamExpr)]
transformMacros macros = 
    [(name, snd (transformLamExpr 10 expr)) | (name, expr) <- macros]
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Transform LamExpr---------------------------------------------------------------------------------
-- This section transforms LamExprs
-- I put each type of LamExpr in a seperate function to make it look simpler
-- The transformLamExpr brings them together by using pattern matching to call the relevant function


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
    let (newNewVar , transformedExpr1) = transformLamExpr (newVar + 1) expr1 
        (newNewNewVar , transformedExpr2) = transformLamExpr (newNewVar + 1) expr2 
        result = LamAbs newNewVar (LamApp transformedExpr1 (LamAbs newNewNewVar (LamApp transformedExpr2 (LamAbs (newNewNewVar + 1) (LamApp (LamApp (LamVar newNewNewVar) (LamVar (newNewNewVar + 1))) (LamVar newNewVar))))))
    in (newNewNewVar + 2, result)
---------------------------------------------------------------------------------------------------------------------------------------------------------------

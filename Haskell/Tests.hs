-- James Palmer
-- jp14g23@soton.ac.uk
-- Copyright of the University of Southampton
-- 09/01/2025

------ Functions used for testing

---- Exercise 1
-- Check whether the two interaction lists are equal disregarding order
amIRight :: [(EdgePoint, EdgePoint)] -> [(EdgePoint, EdgePoint)] -> Bool
amIRight xs ys = all (`elem` ys) xs && all (`elem` xs) ys

---- Exercise 2
-- Check whether two lists of atoms are equal disregarding order
amIRightE2 :: [Atom] -> [Atom] -> Bool
amIRightE2 xs ys = all (`elem` ys) xs && all (`elem` xs) ys

------ Tests

---- Exercise 1

-- Test 1 - Grid size 4 with 3 atoms
  let 
    test1GridSize = 4
    test1Atoms = [ (2,2) , (3,1) , (4,5) ]
    test1ExpectedInteractions = [ (EP West 1 L,EP East 1 R),(EP West 2 L,EP West 4 R),(EP West 3 L,EP West 3 R),(EP West 4 L,EP South 1 R),(EP West 1 R,EP East 1 L),(EP West 2 R,EP East 3 L),(EP West 3 R,EP West 3 L),(EP West 4 R,EP West 2 L),(EP East 1 L,EP West 1 R),(EP East 2 L,EP South 4 R),(EP East 3 L,EP West 2 R),(EP East 4 L,EP East 3 R),(EP East 1 R,EP West 1 L),(EP East 2 R,EP South 2 L),(EP East 3 R,EP East 4 L),(EP East 4 R,EP South 4 L),(EP South 1 L,EP South 2 R),(EP South 2 L,EP East 2 R),(EP South 3 L,EP South 3 R),(EP South 4 L,EP East 4 R),(EP South 1 R,EP West 4 L),(EP South 2 R,EP South 1 L),(EP South 3 R,EP South 3 L),(EP South 4 R,EP East 2 L)]
    test1CalculatedInteractions = calcInteractions test1GridSize test1Atoms

  if amIRight test1ExpectedInteractions test1CalculatedInteractions
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

-- Test 2 - Grid size 6 with 6 atoms
  let 
    test2GridSize = 6
    test2Atoms = [ (1,1) , (2,2) , (3,3) , (4,4) , (5,5) , (6,6) ]
    test2ExpectedInteractions = [(EP East 1 L,EP East 1 R),(EP East 1 R,EP East 1 L),(EP East 2 L,EP South 6 R),(EP East 2 R,EP East 3 L),(EP East 3 L,EP East 2 R),(EP East 3 R,EP East 5 L),(EP East 4 L,EP South 5 R),(EP East 4 R,EP South 4 L),(EP East 5 L,EP East 3 R),(EP East 5 R,EP South 5 L),(EP East 6 L,EP South 4 R),(EP East 6 R,EP South 6 L),(EP West 1 L,EP West 1 R),(EP West 1 R,EP West 1 L),(EP West 2 L,EP West 3 R),(EP West 2 R,EP South 1 L),(EP West 3 L,EP West 5 R),(EP West 3 R,EP West 2 L),(EP West 4 L,EP South 3 R),(EP West 4 R,EP South 2 L),(EP West 5 L,EP South 2 R),(EP West 5 R,EP West 3 L),(EP West 6 L,EP South 1 R),(EP West 6 R,EP South 3 L),(EP South 1 L,EP West 2 R),(EP South 1 R,EP West 6 L),(EP South 2 L,EP West 4 R),(EP South 2 R,EP West 5 L),(EP South 3 L,EP West 6 R),(EP South 3 R,EP West 4 L),(EP South 4 L,EP East 4 R),(EP South 4 R,EP East 6 L),(EP South 5 L,EP East 5 R),(EP South 5 R,EP East 4 L),(EP South 6 L,EP East 6 R),(EP South 6 R,EP East 2 L)]
    test2CalculatedInteractions = calcInteractions test2GridSize test2Atoms

  if amIRight test2ExpectedInteractions test2CalculatedInteractions
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

-- Test 3 - Grid size -1
  let 
    test3GridSize = -1
    test3Atoms = [ (1,1) , (2,2) , (3,3) , (4,4) , (5,5) , (6,6) ]

  putStrLn (calcInteractions test3GridSize test3Atoms)

-- Test 4 - Atom out of bounds
  let
    test4GridSize = 5
    test4Atoms = [ (1,1) , (2,2) , (3,3) , (4,4) , (5,5) , (6,6) ]

  print (calcInteractions test4GridSize test4Atoms)

-- Test 5 - Empty Grid
  let
    test5GridSize = 8
    test5Atoms = []
    test5ExpectedInteractions = emptyRays test5GridSize
    test5CalculatedInteractions = calcInteractions test5GridSize test5Atoms

  if amIRight test5ExpectedInteractions test5CalculatedInteractions
    then putStrLn "Test Passed"

-- Test 6 - Atom border
  let 
    test6GridSize = 4
    test6Atoms = [ (1,1) , (2,1) , (2,3) , (3,1) , (3,5) , (4,1) , (4,3) , (4,5) , (4,7) ]
    test6ExpectedInteractions = [ (EP West 1 L,EP West 1 R),(EP West 2 L,EP West 2 R),(EP West 3 L,EP West 3 R),(EP West 4 L,EP West 4 R),(EP West 1 R,EP West 1 L),(EP West 2 R,EP West 2 L),(EP West 3 R,EP West 3 L),(EP West 4 R,EP West 4 L),(EP East 1 L,EP East 1 R),(EP East 2 L,EP East 2 R),(EP East 3 L,EP East 3 R),(EP East 4 L,EP East 4 R),(EP East 1 R,EP East 1 L),(EP East 2 R,EP East 2 L),(EP East 3 R,EP East 3 L),(EP East 4 R,EP East 4 L),(EP South 1 L,EP South 1 R),(EP South 2 L,EP South 2 R),(EP South 3 L,EP South 3 R),(EP South 4 L,EP South 4 R),(EP South 1 R,EP South 1 L),(EP South 2 R,EP South 2 L),(EP South 3 R,EP South 3 L),(EP South 4 R,EP South 4 L )]
    test6CalculatedInteractions = calcInteractions test6GridSize test6Atoms


---- Exercise 2


-- Test 1 - Grid size 4 with 3 atoms

  let 
    test1AtomNum = 3
    test1ExpectedAtoms = [ (2,2) , (3,1) , (4,5) ]
    test1Interactions = [ (EP West 1 L,EP East 1 R),(EP West 2 L,EP West 4 R),(EP West 3 L,EP West 3 R),(EP West 4 L,EP South 1 R),(EP West 1 R,EP East 1 L),(EP West 2 R,EP East 3 L),(EP West 3 R,EP West 3 L),(EP West 4 R,EP West 2 L),(EP East 1 L,EP West 1 R),(EP East 2 L,EP South 4 R),(EP East 3 L,EP West 2 R),(EP East 4 L,EP East 3 R),(EP East 1 R,EP West 1 L),(EP East 2 R,EP South 2 L),(EP East 3 R,EP East 4 L),(EP East 4 R,EP South 4 L),(EP South 1 L,EP South 2 R),(EP South 2 L,EP East 2 R),(EP South 3 L,EP South 3 R),(EP South 4 L,EP East 4 R),(EP South 1 R,EP West 4 L),(EP South 2 R,EP South 1 L),(EP South 3 R,EP South 3 L),(EP South 4 R,EP East 2 L)]
    test1CalculatedAtoms = solveTBB test1AtomNum test1Interactions

-- Test 2 - Grid size 6 with 6 atoms

  let 
    test2AtomNum = 6
    test2ExpectedAtoms = [ (1,1) , (2,2) , (3,3) , (4,4) , (5,5) , (6,6) ]
    test2Interactions = [(EP East 1 L,EP East 1 R),(EP East 1 R,EP East 1 L),(EP East 2 L,EP South 6 R),(EP East 2 R,EP East 3 L),(EP East 3 L,EP East 2 R),(EP East 3 R,EP East 5 L),(EP East 4 L,EP South 5 R),(EP East 4 R,EP South 4 L),(EP East 5 L,EP East 3 R),(EP East 5 R,EP South 5 L),(EP East 6 L,EP South 4 R),(EP East 6 R,EP South 6 L),(EP West 1 L,EP West 1 R),(EP West 1 R,EP West 1 L),(EP West 2 L,EP West 3 R),(EP West 2 R,EP South 1 L),(EP West 3 L,EP West 5 R),(EP West 3 R,EP West 2 L),(EP West 4 L,EP South 3 R),(EP West 4 R,EP South 2 L),(EP West 5 L,EP South 2 R),(EP West 5 R,EP West 3 L),(EP West 6 L,EP South 1 R),(EP West 6 R,EP South 3 L),(EP South 1 L,EP West 2 R),(EP South 1 R,EP West 6 L),(EP South 2 L,EP West 4 R),(EP South 2 R,EP West 5 L),(EP South 3 L,EP West 6 R),(EP South 3 R,EP West 4 L),(EP South 4 L,EP East 4 R),(EP South 4 R,EP East 6 L),(EP South 5 L,EP East 5 R),(EP South 5 R,EP East 4 L),(EP South 6 L,EP East 6 R),(EP South 6 R,EP East 2 L)]
    test2CalculatedAtoms = solveTBB test2AtomNum test2Interactions

  if amIRightE2 test2CalculatedAtoms test2ExpectedAtoms
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

-- Test 3 - Negative number of atoms

  let 
    test3AtomNum = -1
    test3Interactions = emptyRays 5

  print (solveTBB test3AtomNum test3Interactions)

---- Exercise 3

-- Test 1
    let
      expression = LamDef [] (LamApp (LamVar 1) (LamApp (LamVar 2) (LamVar 3)))
      expectedUnparsedExpression = "x1x2x3"
      calculatedUnparsedExpression = unparse expression

    if calculatedUnparsedExpression == expectedUnparsedExpression
        then putStrLn "Test Passed"
        else putStrLn "Test Failed"

-- Test 2
    let
      expression = LamDef [] (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamMacro "F"))
      expectedUnparsedExpression = "x1x2F"
      calculatedUnparsedExpression = unparse expression

    if calculatedUnparsedExpression == expectedUnparsedExpression
        then putStrLn "Test Passed"
        else putStrLn "Test Failed"

-- Test 3
    let
      expression = LamDef [("F",LamAbs 1 (LamVar 1))] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
      expectedUnparsedExpression = "defF=λx1→x1inλx2→x2F"
      calculatedUnparsedExpression = unparse expression

    if calculatedUnparsedExpression == expectedUnparsedExpression
        then putStrLn "Test Passed"
        else putStrLn "Test Failed"

---- Exercise 4

-- Test 1
    let
      expression = "def F = λx1 → λx2 → x1 x2 in F"
      expectedParsedExpression = Just (LamDef [("F", LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2))))] (LamMacro "F"))
      calculatedParsedExpression = parseLamMacro expression

    if calculatedParsedExpression == expectedParsedExpression
        then putStrLn "Test Passed"
        else putStrLn "Test Failed"

-- Test 2
    let
      expression = "def F = λx1 → x1 in def F = λx2 → x2 in F"
      expectedParsedExpression = Nothing 
      calculatedParsedExpression = parseLamMacro expression

    if calculatedParsedExpression == expectedParsedExpression
        then putStrLn "Test Passed"
        else putStrLn "Test Failed"

-- Test 3
    let
      expression = "def F = λx1 → x1 in def G = λx2 → x2 F in G"
      expectedParsedExpression = Just (LamDef [("F", LamAbs 1 (LamVar 1)), ("G", LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))] (LamApp (LamVar 2) (LamMacro "G")))
      calculatedParsedExpression = parseLamMacro expression

    if calculatedParsedExpression == expectedParsedExpression
        then putStrLn "Test Passed"
        else putStrLn "Test Failed"

-- Test 4
    let
      expression = "x1 x2"
      expectedParsedExpression = Just (LamDef [] (LamApp (LamVar 1) (LamVar 2)))
      calculatedParsedExpression = parseLamMacro expression

    if calculatedParsedExpression == expectedParsedExpression
        then putStrLn "Test Passed"
        else putStrLn "Test Failed"

-- Test 5
    let
      expression = "(λx1 → x1) x1"
      expectedParsedExpression = Just (LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))
      calculatedParsedExpression = parseLamMacro expression

    if calculatedParsedExpression == expectedParsedExpression
        then putStrLn "Test Passed"
        else putStrLn "Test Failed"

---- Exercise 5

-- Test 1
let expression = LamDef [("F", LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2))))] (LamMacro "F")
let expectedCpsExpression = LamDef [("F", LamAbs 0 (LamApp (LamVar 0) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 1) (LamVar 0))))))] (LamMacro "F")
let calculatedCpsExpression = cpsTransform expression

if calculatedCpsExpression == expectedCpsExpression
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

-- Test 2
let expression = LamDef [("F", LamAbs 1 (LamVar 1))] (LamDef [("F", LamAbs 2 (LamVar 2))] (LamMacro "F"))
let expectedCpsExpression = Nothing 
let calculatedCpsExpression = cpsTransform expression

if calculatedCpsExpression == expectedCpsExpression
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

-- Test 3
let expression = LamDef [("F", LamAbs 1 (LamVar 1)), ("G", LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))] (LamApp (LamVar 2) (LamMacro "G"))
let expectedCpsExpression = LamDef [("F", LamAbs 0 (LamApp (LamVar 0) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 1) (LamVar 0))))))] (LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamApp (LamMacro "F") (LamAbs 2 (LamApp (LamVar 2) (LamVar 1))))))))
let calculatedCpsExpression = cpsTransform expression

if calculatedCpsExpression == expectedCpsExpression
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

-- Test 4
let expression = LamDef [] (LamApp (LamVar 1) (LamVar 2))
let expectedCpsExpression = LamDef [] (LamAbs 0 (LamApp (LamAbs 0 (LamApp (LamVar 0) (LamVar 1))) (LamAbs 1 (LamApp (LamAbs 0 (LamApp (LamVar 0) (LamVar 2))) (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 0)))))))
let calculatedCpsExpression = cpsTransform expression

if calculatedCpsExpression == expectedCpsExpression
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

-- Test 5
let expression = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2))
let expectedCpsExpression = LamDef [] (LamAbs 0 (LamApp (LamAbs 0 (LamApp (LamVar 0) (LamVar 1))) (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))))
let calculatedCpsExpression = cpsTransform expression

if calculatedCpsExpression == expectedCpsExpression
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

-- Test 6
    let
      expression = LamDef [("F",LamAbs 1 (LamVar 1)) , ("G" , LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2))))] (LamApp (LamMacro "F") (LamMacro "G"))
      expectedTransformedExpression = LamDef [("F", LamAbs 0 (LamApp (LamVar 0) (LamAbs 0 (LamAbs 1 (LamApp (LamVar 1) (LamVar 0))))))]
       (LamAbs 0 (LamApp (LamMacro "F") (LamAbs 1 (LamApp (LamMacro "G")
       (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 0)))))))
      calculatedTransformedExpression = cpsTransform expression

    print expectedTransformedExpression
    print calculatedTransformedExpression

---- Exercise 6

-- Test 1
  let expression = LamDef [("F", LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2))))] (LamMacro "F")
  let expectedStepNums = (Just 1, Just 1, Just 5, Just 5)
  let calculatedStepNums = compareInnerOuter expression 10

  if expectedStepNums == calculatedStepNums
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

-- Test 2
  let expression = LamDef [("F", LamAbs 1 (LamVar 1))] (LamDef [("F", LamAbs 2 (LamVar 2))] (LamMacro "F"))
  let expectedStepNums = (Nothing, Nothing, Nothing, Nothing)
  let calculatedStepNums = compareInnerOuter expression 10

  if expectedStepNums == calculatedStepNums
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

-- Test 3
  let expression = LamDef [("F", LamAbs 1 (LamVar 1)), ("G", LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))] (LamApp (LamVar 2) (LamMacro "G"))
  let expectedStepNums = (Just 3, Just 3, Just 7, Just 7)
  let calculatedStepNums = compareInnerOuter expression 10

  if expectedStepNums == calculatedStepNums
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

-- Test 4
  let expression = LamDef [] (LamApp (LamVar 1) (LamVar 2))
  let expectedStepNums = (Just 1, Just 1, Just 3, Just 3)
  let calculatedStepNums = compareInnerOuter expression 10

  if expectedStepNums == calculatedStepNums
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

-- Test 5
  let expression = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2))
  let expectedStepNums = (Just 1, Just 1, Just 3, Just 3)
  let calculatedStepNums = compareInnerOuter expression 10

  if expectedStepNums == calculatedStepNums
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"
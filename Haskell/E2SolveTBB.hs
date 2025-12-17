-- James Palmer
-- jp14g23@soton.ac.uk
-- Copyright of the University of Southampton
-- 09/01/2025

--Your import statements here
import Data.List (sortBy,sort)
import Data.Ord (comparing)

-- DO NOT MODIFY THESE DATATYPES OTHER THAN TO ADD TO THE DERIVING LIST
data EdgeDir = L | R deriving (Eq,Show)
data Face = East | West | South deriving (Eq,Show)
data EdgePoint = EP Face Int EdgeDir deriving (Eq,Show)
type Atom = (Int,Int)
--  DO NOT MODIFY THESE DATATYPES OTHER THAN TO ADD TO THE DERIVING LIST

-------------------------------------------------------------Grid creation and manipulation--------------------------------------------------------------------
-- In this section I create a grid of scores that rank how likely there is to be an atom at a certain location
-- First you need to find how large the grid is based on the list of interactions
-- Then you make the grid with each level have 2n-1 locations
-- There are two functions for changing the grid, either increment the value at a specified location, or pass a value to change it 

-- Finds the size of the grid from the input
-- Just gets the first int from the last tuple in the list
findGridSize :: [(EdgePoint, EdgePoint)] -> Int
findGridSize list =
    let (EP _ n _) = fst (last list)
    in n

-- Make a grid of likelihood scores, all starting at 0
generateBlankScoreGrid :: Int -> [[Int]]
generateBlankScoreGrid n
    | n == 1    = [[0]]
    | otherwise = generateBlankScoreGrid (n-1) ++ [replicate (2*n-1) 0]

-- Update a value in the score grid
-- Takes the score grid
-- Takes a location
-- Takes a new score
-- Returns updated score grid
updateGridValue :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
updateGridValue grid (row, offset) newVal =
    let (beforeRow, targetRow:afterRow) = splitAt (row - 1) grid
        (beforeOffset, _:afterOffset) = splitAt (offset - 1) targetRow
    in beforeRow ++ [beforeOffset ++ [newVal] ++ afterOffset] ++ afterRow

-- Increment a value in the score grid
incrementGridValue :: [[Int]] -> (Int, Int) -> [[Int]]
incrementGridValue grid (row, offset) =
    let (beforeRow, targetRow:afterRow) = splitAt (row - 1) grid
        (beforeOffset, oldValue:afterOffset) = splitAt (offset - 1) targetRow
        newValue = oldValue + 1
    in beforeRow ++ [beforeOffset ++ [newValue] ++ afterOffset] ++ afterRow
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Initial atom discovery from corners---------------------------------------------------------------
-- You can figure out whether theres an atom in each corner 
-- This allows you to be absoluteley certain whether theres a atom in each corner

-- Takes the size of the grid, the current grid, and the list of interactions
-- Calls determineCorner on the list of interactions
updateScoreGridFromCorners :: Int -> [[Int]] -> [(EdgePoint, EdgePoint)] -> [[Int]]
updateScoreGridFromCorners gridSize scoreGrid [] =
  scoreGrid
updateScoreGridFromCorners gridSize scoreGrid (edge:edges) =
  let updatedGrid = determineCorner gridSize scoreGrid edge
  in updateScoreGridFromCorners gridSize updatedGrid edges

-- Logic for each corner
-- You shoot a ray through each corner, and it comes out the other side, there cannot be an atom there
-- Updates the grid with -10000, or 10000 to show absolute certainty
determineCorner :: Int -> [[Int]] -> (EdgePoint, EdgePoint) -> [[Int]]

-- No atom at the top
determineCorner gridSize scoreGrid (EP West 1 R, EP East 1 L) =
  updateGridValue scoreGrid (1, 1) (-10000)
-- No atom in bottom left
determineCorner gridSize scoreGrid (EP West x L, EP South 1 R)
 | x == gridSize = updateGridValue scoreGrid (gridSize, 1) (-10000)
-- No atom in bottom right
determineCorner gridSize scoreGrid (EP East x R, EP South y L)
 | x == gridSize && y == gridSize = updateGridValue scoreGrid (gridSize, (2*gridSize)-1) (-10000)

-- Yes atom at top
determineCorner gridSize scoreGrid (EP West 1 R, _) =
  updateGridValue scoreGrid (1, 1) 10000
-- Yes atom in bottom left
determineCorner gridSize scoreGrid (EP West x L, _) 
 | x == gridSize = updateGridValue scoreGrid (gridSize, 1) 10000
-- Yes bottom in bottom right
determineCorner gridSize scoreGrid (EP East x R, _) 
 | x == gridSize = updateGridValue scoreGrid (gridSize, (2*gridSize)-1) 10000
-- Otherwise just don't do anything
determineCorner _ scoreGrid _ = 
  scoreGrid
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Creating grid of likelihood scores----------------------------------------------------------------
-- This section goes through the list of interactions, paying attetion to whether the rays make it through to the other side, or get deflected elsewhere
-- This provides a semi-accurate idea of where the atoms are
-- Which makes it a lot faster to brute force guess the final answer

-- Produces a grid of likelihood scores for each location
findLikelihoods :: [(EdgePoint,EdgePoint)] -> [[Int]]
findLikelihoods interactions = 
 updateGridWithPaths grid edgepoints gridSize
 where 
    gridSize = findGridSize interactions
    emptyInteractions = emptyRays gridSize
    blankScoreGrid = generateBlankScoreGrid gridSize
    -- Start off with corner logic
    grid = updateScoreGridFromCorners gridSize blankScoreGrid interactions
    -- The list of edgepoints that when you fire a ray through them, the ray is deflected
    edgepoints = findMismatchedEdgePoints emptyInteractions interactions

-- Makes a list of interactions, as if there were no atoms in the grid
emptyRays :: Int -> [(EdgePoint, EdgePoint)]
emptyRays n = 
  westRays ++ eastRays ++ southRays
  where
    westRays = [(EP West i L, EP South (n - (i - 1)) R) | i <- [1..n]] ++
               [(EP West i R, EP East i L) | i <- [1..n]]
    eastRays = [(EP East i L, EP South i L) | i <- [1..n]] ++
               [(EP East i R, EP West i R) | i <- [1..n]]
    southRays = [(EP South i L, EP East i R) | i <- [1..n]] ++
                [(EP South i R, EP West (n - (i - 1)) L) | i <- [1..n]]

--Finds the list of edgepoints whose rays dont go straight through the grid
--Pass this the list generated by emptyRays, and the input.
findMismatchedEdgePoints :: [(EdgePoint, EdgePoint)] -> [(EdgePoint, EdgePoint)] -> [EdgePoint]
findMismatchedEdgePoints list1 list2 =
    [ ep1 -- Just want first edgepoint
    | (ep1, ep2) <- list1,
      let matches = [ep2' | (ep1', ep2') <- list2, ep1 == ep1'],
      not (null matches), -- Checking if theres a match 
      let ep2' = head matches,
      ep2 /= ep2'
    ]

-- Get the list of atoms along the expected path of a ray
-- This is so I know what scores to increment
-- Pass this an edgepoint and the size of the grid
tracePath :: EdgePoint -> Int -> [(Int, Int)]
tracePath ep n = 
  go (edgepointToCoord ep n)
  where
    go coord
      | isOutOfBounds coord n = []
      | otherwise = coord : go (moveRay coord ep)

-- Take a list of edgepoints whose rays dont go straight through 
-- Updates the grid by incrementing the locations along those paths
updateGridWithPaths :: [[Int]] -> [EdgePoint] -> Int -> [[Int]]
updateGridWithPaths grid edgePoints n =
    foldl updateGrid grid allCoords
  where
    allCoords = concatMap (`tracePath` n) edgePoints
    updateGrid = incrementGridValue
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Finding solutions---------------------------------------------------------------------------------
-- This section uses the likelihood scores to try different potential atom locations, until it finds the right one

-- Take the grid of scores and put each score in a tuple with its coordinate
coupleWithLocation :: [[Int]] -> [[(Int, (Int, Int))]]
coupleWithLocation xs = 
  [ [ (val, (row, col)) 
    | (col, val) <- zip [1..] rowVals ] 
  | (row, rowVals) <- zip [1..] xs ]

-- Turn it into one big list and sort it by the most likely places
mostLikely :: [[(Int, (Int, Int))]] -> [(Int, (Int, Int))]
mostLikely xs = 
  let flattened = concat xs -- Turn it to one list
  in sortBy (flip (comparing fst)) flattened -- Sort it by comparing first part of tuple

-- Check whether the two interaction lists are equal, disregarding order
-- This is to check that a solution that ive come up with matches the input
amIRight :: [(EdgePoint, EdgePoint)] -> [(EdgePoint, EdgePoint)] -> Bool
amIRight xs ys = 
  all (`elem` ys) xs && all (`elem` xs) ys

-- Function that tries an atom configuration and checks it against the input
tryAtomConfig :: [Atom] -> [(EdgePoint, EdgePoint)] -> Bool
tryAtomConfig locations interactions =
    let gridSize = findGridSize interactions
        calculatedInteractions = calcInteractions gridSize locations
    in amIRight calculatedInteractions interactions

-- Makes all of the different combinations of elements in a list 
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations numAtoms (x:xs) = map (x:) (combinations (numAtoms-1) xs) ++ combinations numAtoms xs

-- Tries all the different locations of atoms and checks if they produce a valid solution
tryPermutations :: Int -> [(EdgePoint, EdgePoint)] -> [(Int, (Int, Int))] -> [Atom]
tryPermutations nAtoms interactions scoredLocations =
  let atomLocations = map snd scoredLocations  
      possibleCombinations = combinations nAtoms atomLocations 
  in head $ filter (\config -> tryAtomConfig config interactions) possibleCombinations

-- Calls tryPermutations, giving the list of the most likely atom locations
solveTBB :: Int -> [(EdgePoint,EdgePoint)] -> [Atom]
solveTBB numAtoms interactions 
 | numAtoms < 0 = error "Number of atoms cannot be negative"
 | otherwise = tryPermutations numAtoms interactions (mostLikely(coupleWithLocation(findLikelihoods interactions)))
---------------------------------------------------------------------------------------------------------------------------------------------------------------


---------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------Code from Exercise 1---------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------


-------------------------------------------------------------Main function-------------------------------------------------------------------------------------
calcInteractions :: Int -> [Atom] -> [(EdgePoint, EdgePoint)]
-- Takes grid size and list of atom locations
calcInteractions n atoms
  | n < 1 = error "The grid size cannot be negative or 0"
  | existsOutOfBoundsAtoms n atoms = error "Atoms cannot be out of bounds"
  | otherwise = let
              -- Generate all possible entry edgepoints
              entryPoints = generateEntryPoints n

              -- Generate the grid with the atom locations
              grid = generateGrid n atoms

              -- Find the corresponding exit edgepoint for each entry
              findInteraction ep =
                let
                  startCoord = edgepointToCoord ep n
                  exitPoint =
                    -- Case when ray immediately hits atom
                    if isAtom startCoord grid
                    then mirrorEdgePoint ep
                    -- Other cases
                    else atomTravel ep startCoord n grid
                in (ep, exitPoint)

              -- Function for when ray immediately hits atom leading to direction simply switching
              mirrorEdgePoint (EP face int dir) =
                EP face int (if dir == L then R else L)

              -- Map every entry point to its exit point
              interactions = map findInteraction entryPoints
            in
              interactions

-- Change Edgepoint to be same but different direction
mirrorEdgePoint :: EdgePoint -> EdgePoint
mirrorEdgePoint (EP face int dir) = 
  EP face int (if dir == L then R else L)

existsOutOfBoundsAtoms :: Int -> [Atom] -> Bool
existsOutOfBoundsAtoms n atoms =
  any (isAtomOutOfBounds n) atoms

isAtomOutOfBounds :: Int -> Atom -> Bool
isAtomOutOfBounds n (x,y)
 | x < 1 = True
 | x > n = True
 | y < 1 = True
 | y > (2*n)-1 = True
 | otherwise = False
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Generate Edgepoints-------------------------------------------------------------------------------
-- Generate all of the edgpoints around the grid
generateEntryPoints :: Int -> [EdgePoint]
generateEntryPoints n =
  [EP face i dir | face <- [East, West, South], i <- [1 .. n], dir <- [L, R]]
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Generate Boolean grid-----------------------------------------------------------------------------
-------------Main function 

-- Generate a triangle shaped grid of Trues and Falses
generateGrid :: Int -> [Atom] -> [[Bool]]
generateGrid n atoms =
  insertAtomList (generateEmptyGrid n) atoms

-------------Helper functions

-- Make a grid with only falses
generateEmptyGrid :: Int -> [[Bool]]
generateEmptyGrid n
    | n == 1    = [[False]]
    | otherwise = generateEmptyGrid (n-1) ++ [replicate (2*n-1) False]

-- Insert an atom into its place by making it True
insertAtom :: [[Bool]] -> (Int, Int) -> [[Bool]]
insertAtom grid (x, y) =
  let -- Making sure grid is aligned as its indexed from 0
      x' = x - 1
      y' = y - 1
  in take x' grid ++
     [take y' (grid !! x') ++ 
     [True] ++ 
     drop (y' + 1) (grid !! x')] ++
     drop (x' + 1) grid

-- Call insert atom over the whole grid
insertAtomList :: [[Bool]] -> [Atom] -> [[Bool]]
insertAtomList grid atoms =
  foldl insertAtom grid atoms
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Represent the triangle with coordinates-----------------------------------------------------------
-- Make a list of lists to represent the triangle as (Row,Offset)
generateCoordinateTriangle :: Int -> [[(Int, Int)]]
generateCoordinateTriangle n =
  [ [(row, col) | col <- [1..(2*row-1)]] | row <- [1..n] ]
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Ray Movement--------------------------------------------------------------------------------------
-- Take a coordinate, and the edgepoint it came from, and return the next location it moves to
moveRay :: (Int, Int) -> EdgePoint -> (Int, Int)

--Rays coming from West
moveRay (r, o) (EP West _ L)
  | odd o     = (r + 1, o + 1)
  | otherwise = (r, o + 1)
moveRay (r, o) (EP West _ R) = (r, o + 1)

--Rays coming from East
moveRay (r, o) (EP East _ R)
  | odd o     = (r + 1, o + 1)
  | otherwise = (r, o - 1)
moveRay (r, o) (EP East _ L) = (r, o - 1)

--Rays coming from South
moveRay (r, o) (EP South _ L)
  | odd o     = (r, o + 1)
  | otherwise = (r - 1, o - 1)
moveRay (r, o) (EP South _ R)
  | odd o     = (r, o - 1)
  | otherwise = (r - 1, o - 1)

-- Change the direction that a ray is coming from based on its current direction and where it hit an atom
changeRayDir :: EdgePoint -> (Int,Int) -> EdgePoint
changeRayDir (EP face int dir) (r,o)
  | face == West && dir == L && odd o = EP East int L
  | face == West && dir == L && even o = EP South int L
  | face == West && dir == R && odd o = EP South int R
  | face == West && dir == R && even o = EP East int R
  | face == East && dir == L && odd o = EP South int L
  | face == East && dir == L && even o = EP West int L
  | face == East && dir == R && odd o = EP West int R
  | face == East && dir == R && even o = EP South int R
  | face == South && dir == L && odd o = EP West int L
  | face == South && dir == L && even o = EP East int L
  | face == South && dir == R && odd o = EP East int R
  | face == South && dir == R && even o = EP West int R

-- Checks whether a location is out of bounds
isOutOfBounds :: (Int,Int) -> Int -> Bool
isOutOfBounds (r,o) n
  | r > n || o >= (2*r) || o < 1 || r < 1 = True
  | otherwise = False

-- Converts edgepoint to coordinate
edgepointToCoord :: EdgePoint -> Int -> (Int,Int)
edgepointToCoord (EP face int dir) n
  | face == West = (int,1)
  | face == East = (int,((2*int)-1))
  | face == South = (n,((2*int)-1))

-- Takes where the ray was coming from, and where it is now, to determine the exit edgepoint
entranceToExit :: EdgePoint -> (Int,Int) -> EdgePoint
entranceToExit (EP face int dir) (r,o)
  | face == West && dir == L = EP South ((o+1) `div` 2) R
  | face == West && dir == R = EP East r L
  | face == East && dir == L = EP West r R
  | face == East && dir == R = EP South ((o+1) `div` 2) L
  | face == South && dir == L = EP East r R
  | face == South && dir == R = EP West r L

-- Moves ray 
atomTravel :: EdgePoint -> (Int, Int) -> Int -> [[Bool]] -> EdgePoint
atomTravel ep currentCoord n grid =
    let
        nextCoord = moveRay currentCoord ep
    in
        if isOutOfBounds nextCoord n
        then entranceToExit ep currentCoord
        else if isAtom nextCoord grid
        then
            let newEp = changeRayDir ep nextCoord
            in atomTravel newEp nextCoord n grid
        else atomTravel ep nextCoord n grid
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------Atom collision------------------------------------------------------------------------------------
-- Check if theres an atom at the specified location
isAtom :: (Int, Int) -> [[Bool]] -> Bool
isAtom (x, y) grid =
  (grid !! x') !! y'
  where
    x' = x - 1
    y' = y - 1
---------------------------------------------------------------------------------------------------------------------------------------------------------------
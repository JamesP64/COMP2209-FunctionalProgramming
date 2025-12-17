-- James Palmer
-- jp14g23@soton.ac.uk
-- Copyright of the University of Southampton
-- 09/01/2025

--Your import statements here
import Control.Monad (void)
import Debug.Trace

-- DO NOT MODIFY THESE DATATYPES OTHER THAN TO ADD TO THE DERIVING LIST
data EdgeDir = L | R deriving (Eq,Show,Ord)
data Face = East | West | South deriving (Eq,Show,Ord)
data EdgePoint = EP Face Int EdgeDir deriving (Eq,Show,Ord)
type Atom = (Int,Int)
--  DO NOT MODIFY THESE DATATYPES OTHER THAN TO ADD TO THE DERIVING LIST

-------------------------------------------------------------Main function-------------------------------------------------------------------------------------
-- calcInteractions takes the size of the grid and a list of atom locations
-- It first handles errors in the input
-- Then it simulates where each ray moves
-- The rays carry the direction they were moving from, and the next square they will travel to
-- If the next square has an atom, then the direction that the ray was coming from is changed
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
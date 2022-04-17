module Automata where

import CodeWorld

-- | A 'Grid' of some cell type c consists of a pair of positive ints
-- describing its width and height, along with a list of cs that is
-- exactly (width * height) long.
data Grid c = Grid Int Int [c]

-- | Type alias for grid coordinates. This makes it clear when we are
-- talking about grid coordinates specifically, as opposed to some
-- other pair of integers.
type GridCoord = (Int, Int)


-- | Type of cells used in QR World.
-- Task 1A
data QRCell = Alive | Dead
    deriving (Show,Eq)

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- Task 1C
cycleQR :: QRCell -> QRCell
cycleQR Alive = Dead
cycleQR Dead = Alive

-- Task 1D
renderQR :: QRCell -> Picture
renderQR Alive = coloured blue  (solidRectangle 1 1)
renderQR Dead  = coloured black (rectangle 1 1)


-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- Task2X
nextGenQR :: Grid QRCell -> Grid QRCell
nextGenQR = undefined -- TODO

-- Task2Y
evolveQR :: Int -> Grid QRCell -> Grid QRCell
evolveQR = undefined -- TODO


-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- Task 1E
get :: Grid c -> GridCoord -> Maybe c
get (Grid a b cells) (x,y)
    | ((x+1) <= a) && ((y+1) <= b) = Just (cells !! nth)
    | otherwise                    = Nothing
    where nth = nthElem a (x,y)

-- >> Helper to calculate the number element that (x,y) is in the list
nthElem :: Int -> GridCoord -> Int
nthElem a (x,y) = (y+1)*a - (a-x)



-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- Task 1F
allCoords :: Int -> Int -> [GridCoord]
allCoords a b = allPairs (b-1) (nList (a-1)) 

-- >> Helper creates a list of all (x,y) combos
-- >> Example 1 and [0,1,2,3] -> [(0,0),(1,0),(0,1),(1,1)]
allPairs :: Int -> [Int] -> [GridCoord] 
allPairs y cols
    | y==0      = nPair 0 cols
    | otherwise = (allPairs (y-1) cols)++(nPair y cols)

-- >> Helper pairs a list of y's with an x
-- >> Example x and [0,1,2,3] -> [(x,0),(x,1)...]
nPair :: Int -> [Int] -> [GridCoord]
nPair y list = case list of
    []   -> []
    x:xs -> (x,y):(nPair y xs)

-- >> Helper returns List of from [0,1,2,...,n]
nList :: Int -> [Int]
nList n
    |      n==0 = [0]
    | otherwise = (nList (n-1))++[n]
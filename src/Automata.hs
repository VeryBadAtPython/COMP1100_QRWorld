module Automata where

import CodeWorld

-- | A 'Grid' of some cell type c consists of a pair of positive ints
-- describing its width and height, along with a list of cs that is
-- exactly (width * height) long.
data Grid c = Grid Int Int [c]
    deriving (Show,Eq)

-- | Type alias for grid coordinates. This makes it clear when we are
-- talking about grid coordinates specifically, as opposed to some
-- other pair of integers.
type GridCoord = (Int, Int)




-- ======================== Task 1A ======================== --
-- | Type of cells used in QR World.
data QRCell = Alive | Dead
    deriving (Show,Eq)




-- ======================== Task 1C ======================== --
-- | Swaps state of a cell
-- | Test exists in AutomataTest
cycleQR :: QRCell -> QRCell
cycleQR Alive = Dead
cycleQR Dead  = Alive




-- ======================== Task 1D ======================== --
-- | Renders a cell as appropriate dead or alive rectangles
renderQR :: QRCell -> Picture
renderQR Alive = coloured blue (solidRectangle 1 1)
renderQR Dead  = coloured black (rectangle 1 1)






-- ======================== Task 2A ======================== --
-- ========================================================= --
-- | Evolves the state of a cell depending on it's neighborhood
-- | Test exists in AutomataTest
-- |>> Helper nextGenQrGrid that is used by nextGenQR function
-- |   Calls the appropriate helpers
-- |   Produces a the evolved state list to feed into nextGenQR
-- |>> Helper decideEvolve to decide what evolution to make
-- |   Based on the state and the number of alive states in the neighborhood
-- |>> Helper findHood that finds the states of the four neighbors to (x,y)
-- |   Outputs a list ordered as [Above,Right,Below,Left]
-- |>> Helper countStates sees how many neighbors in the findHood list are alive

nextGenQR :: Grid QRCell -> Grid QRCell
nextGenQR (Grid a b cells) =
    Grid a b (nextGenQrGrid (Grid a b cells) (allCoords a b))

nextGenQrGrid :: Grid QRCell -> [GridCoord] -> [QRCell]
nextGenQrGrid (Grid a b cells) coordList = case coordList of
    []   -> []
    z:zs -> (decideEvolve hood state) : (nextGenQrGrid (Grid a b cells) zs)
        where state = get (Grid a b cells) z
              hood  = findHood (Grid a b cells) z

decideEvolve :: [Maybe QRCell] -> Maybe QRCell -> QRCell
decideEvolve nbrs state = case state of
    Just Alive
      | alive < 2  -> Dead
      | alive == 4 -> Dead
      | otherwise  -> Alive
    Just Dead
      | alive == 2 -> Alive
      | alive == 4 -> Alive
      | otherwise  -> Dead
    _              -> Dead
    where alive = countAlive nbrs

findHood :: Grid QRCell -> GridCoord -> [Maybe QRCell]
findHood (Grid a b cells) (x,y) = 
    [(get (Grid a b cells) (x,y-1))
    ,(get (Grid a b cells) (x+1,y))
    ,(get (Grid a b cells) (x,y+1))
    ,(get (Grid a b cells) (x-1,y))]

countAlive :: [Maybe QRCell] -> Int
countAlive nbrs = case nbrs of
    []                    -> 0
    y:ys
      | y == (Just Alive) -> 1 + (countAlive ys)
      | otherwise         -> countAlive ys




-- ======================== Task 2B ======================== --
-- | Evolves the grid through n::Int interpretations
-- | Test exists in AutomataTest

evolveQR :: Int -> Grid QRCell -> Grid QRCell
evolveQR n state
    | n<=0      = state
    | otherwise = evolveQR (n-1) (nextGenQR state)




-- ======================== Task 1E ======================== --
-- ========================================================= --
-- | Returns the state of the cell at (x,y)
-- | Test exists in AutomataTest

get :: Grid c -> GridCoord -> Maybe c
get (Grid a b cells) (x,y)
    | ((x<0) || (y<0))     = Nothing
    | ((x < a) && (y < b)) = Just (cells !! nth)
    | otherwise            = Nothing
    where nth = y*a+x




-- ======================== Task 1F ======================== --
-- ========================================================= --
-- | Generates a row-major list of all grid coordinates in an axb grid.
-- | Does not accept dimensions <=0 as they are nonsensical
-- | Test exists in AutomataTest
-- |>> Helper allPairs creates a list of all (x,y) combos
-- |   Example 1 and [0,1,2,3] -> [(0,0),(1,0),(0,1),(1,1)]
-- |>> Helper npair pairs a list of x's with a y
-- |   Example y and [0,1,2,3] -> [(0,y),(1,y)...]
-- |>> Helper nList returns List of from [0,1,2,...,n]

allCoords :: Int -> Int -> [GridCoord]
allCoords a b 
    | a<=0 || b<=0 = error "Invalid grid dimensions"
    | otherwise    = allPairs (b-1) (nList (a-1)) 

allPairs :: Int -> [Int] -> [GridCoord] 
allPairs y cols
    | y<=0      = nPair 0 cols
    | otherwise = (allPairs (y-1) cols)++(nPair y cols)

nPair :: Int -> [Int] -> [GridCoord]
nPair y list = case list of
    []   -> []
    x:xs -> (x,y):(nPair y xs)

nList :: Int -> [Int]
nList n
    |      n<=0 = [0]
    | otherwise = (nList (n-1))++[n]
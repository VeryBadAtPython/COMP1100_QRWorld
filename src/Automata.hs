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


-- | Task 1A
-- | Type of cells used in QR World.
data QRCell = Alive | Dead
    deriving (Show,Eq)






-- | Task 1C
-- | Swaps state of a cell
cycleQR :: QRCell -> QRCell
cycleQR Alive = Dead
cycleQR Dead = Alive

-- | Task 1D
-- | Renders a cell as appropriate dead or alive rectangles
renderQR :: QRCell -> Picture
renderQR Alive = coloured blue  (solidRectangle 1 1)
renderQR Dead  = coloured black (rectangle 1 1)






-- | Task2A
-- | Evolves the state of a cell depending on it's neighborhood
nextGenQR :: Grid QRCell -> Grid QRCell
nextGenQR (Grid a b cells) = Grid a b (nextGenQrGrid (Grid a b cells) (allCoords a b))


-- |>> Helper that is used by nextGenQR function to call the appropriate helpers
-- |>> Produces a the evolved state list to feed into nextGenQR
nextGenQrGrid :: Grid QRCell -> [GridCoord] -> [QRCell]
nextGenQrGrid (Grid a b cells) coordList = case coordList of
    []   -> []
    z:zs -> (decideEvolve hood state) : (nextGenQrGrid (Grid a b cells) zs)
        where state = get (Grid a b cells) z
              hood  = findHood (Grid a b cells) z



-- |>> Helper to decide what evolution to make
-- |>> Based on the state and the number of alive states in the neighborhood
decideEvolve :: [Maybe QRCell] -> Maybe QRCell -> QRCell
decideEvolve nbrs state = case state of
    Just Alive
      | (countAlive nbrs)<=2 -> Dead
      | (countAlive nbrs)==4 -> Dead
      | otherwise            -> Alive
    Just Dead
      | (countAlive nbrs)==2 -> Alive
      | (countAlive nbrs)==4 -> Alive
      | otherwise            -> Dead
    Nothing                  -> Dead

-- |>> Helper that finds the states of the four neighbors to (x,y) as a list
-- |>> Ordered as [Above,Right,Below,Left]
findHood :: Grid QRCell -> GridCoord -> [Maybe QRCell]
findHood (Grid a b cells) (x,y) = [(get (Grid a b cells) (x,y-1))
                                  ,(get (Grid a b cells) (x+1,y))
                                  ,(get (Grid a b cells) (x,y+1))
                                  ,(get (Grid a b cells) (x-1,y))]

    
-- |>> countStates sees how many neighbors in the findHood list are alive
countAlive :: [Maybe QRCell] -> Int
countAlive nbrs = case nbrs of
    []                    -> 0
    y:ys
      | y == (Just Alive) -> 1 + (countAlive ys)
      | otherwise         -> countAlive ys






-- | Task2B
-- | Evolves the grid through n::Int interpretations
evolveQR :: Int -> Grid QRCell -> Grid QRCell
evolveQR n state
    | n<=0      = state
    | otherwise = evolveQR (n-1) (nextGenQR state)







-- | Task 1E
-- | Returns the state of the cell at (x,y)
get :: Grid c -> GridCoord -> Maybe c
get (Grid a b cells) (x,y)
    | ((x<0) || (y<0))     = Nothing
    | ((x < a) && (y < b)) = Just (cells !! nth)
    | otherwise            = Nothing
    where nth = nthElem a (x,y)

-- |>> Helper to calculate the number element that (x,y) is in the list
nthElem :: Int -> GridCoord -> Int
nthElem a (x,y) = (y+1)*a - (a-x)






-- | Task 1F
-- | Generates a row-major list of all grid coordinates in an axb grid.
allCoords :: Int -> Int -> [GridCoord]
allCoords a b = allPairs (b-1) (nList (a-1)) 

-- |>> Helper creates a list of all (x,y) combos
-- |>> Example 1 and [0,1,2,3] -> [(0,0),(1,0),(0,1),(1,1)]
allPairs :: Int -> [Int] -> [GridCoord] 
allPairs y cols
    | y==0      = nPair 0 cols
    | otherwise = (allPairs (y-1) cols)++(nPair y cols)

-- |>> Helper pairs a list of y's with an x
-- |>> Example x and [0,1,2,3] -> [(x,0),(x,1)...]
nPair :: Int -> [Int] -> [GridCoord]
nPair y list = case list of
    []   -> []
    x:xs -> (x,y):(nPair y xs)

-- |>> Helper returns List of from [0,1,2,...,n]
nList :: Int -> [Int]
nList n
    |      n==0 = [0]
    | otherwise = (nList (n-1))++[n]
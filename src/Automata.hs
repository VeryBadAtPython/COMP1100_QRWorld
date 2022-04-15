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
data QRCell -- TODO

cycleQR :: QRCell -> QRCell
cycleQR = undefined -- TODO

renderQR :: QRCell -> Picture
renderQR = undefined -- TODO

nextGenQR :: Grid QRCell -> Grid QRCell
nextGenQR = undefined -- TODO

evolveQR :: Int -> Grid QRCell -> Grid QRCell
evolveQR = undefined -- TODO


get :: Grid c -> GridCoord -> Maybe c
get = undefined -- TODO

allCoords :: Int -> Int -> [GridCoord]
allCoords = undefined -- TODO

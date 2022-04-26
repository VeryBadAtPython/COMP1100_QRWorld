module AutomataTest where

import Automata
import Testing
import TestPatterns -- To test toQR and use test patterns

-- | The list of tests to run. When you define additional test cases,
-- you must list them here or they will not be checked. You should
-- remove these examples when submitting the assignment.
tests :: [Test]
tests =
  cycleQRTests
  ++ getTests
  ++ allCoordsTest
  ++ toQRTests
  ++ evolveQRTests
  ++ countAliveTests
  



-- ================== cycleQR Tests  ================= --
-- | Tests both possible inputs to cycleQR
cycleQRTests :: [Test]
cycleQRTests = [Test "cycleQR Alive" (assertEqual (cycleQR Dead) (Alive::QRCell))
 ,Test "cycleQR Dead" (assertEqual (cycleQR Alive) (Dead::QRCell))]


-- ================== get Tests      ================= --
-- | Ensure get works in many cases
-- |>> (1) Get something outside of grid x,y<0 -> Nothing
-- |>> (2) Get something outside of grid x>a y>b -> Nothing
-- |>> (3) Check it reads the correct value for bottom left in the first iteration of 1100 test pattern
-- |>> (4) Check it reads the correct value for (1,0) in the first iteration of 1100 test pattern
-- |>> (5) Check can get (0,1 )from a 2x2
-- |>> (6) Check something outside 2x2 -> Nothing
-- |       3->5 are tests of a typical inputs whiles 1,2,6 are edge cases
getTests :: [Test]
getTests = [Test "get: (-1,-1)" (assertEqual (get (Grid 2 2 [Alive, Alive, Dead, Dead]) (-1,1)) (Nothing::Maybe QRCell))
 ,Test "get: (500,30)" (assertEqual (get (Grid 2 2 [Alive, Alive, Dead, Dead]) (500,30)) (Nothing::Maybe QRCell))
 ,Test "get: Bottom L 1100" (assertEqual (get pattern1100 (10,2)) ((Just Alive) :: Maybe QRCell))
 ,Test "get: (1,0) 1100" (assertEqual (get pattern1100 (1,0)) ((Just Dead) :: Maybe QRCell))
 ,Test "get: (0,1) in 2x2" (assertEqual (get (Grid 2 2 [Alive, Alive, Dead, Dead]) (0,1)) ((Just Dead) ::Maybe QRCell))
 ,Test "get: (0,3) in 2x2" (assertEqual (get (Grid 2 2 [Alive, Alive, Dead, Dead]) (0,3)) (Nothing ::Maybe c)) ]


-- ================== allCoords Test ================= --
-- | Tests it produces an appropriate grid
-- |>> (1) Tests a produce a typical basic grid
-- |>> (2) Tests a typical input to helper nPair
-- |>> (3) Tests a typical input to helper nList
-- |>> (4) Tests allCoords 0 0 -> []
-- |>> (5) Tests allCoords (-1) (-1) -> []
-- |       (4)&(5) are edge cases of nonsensical inputs
allCoordsTest :: [Test]
allCoordsTest = [Test "allCoords 2 2" (assertEqual (allCoords 2 2) ([(0,0),(1,0),(0,1),(1,1)] :: [GridCoord]))
 ,Test "nPair 2 [1,2,3]" (assertEqual (nPair 2 [0,1,2]) ([(0,2),(1,2),(2,2)] :: [GridCoord]))
 ,Test "nList 5" (assertEqual (nList 5) ([0,1,2,3,4,5] :: [Int]))
 ,Test "allCoords 0 0" (assertEqual (allCoords 0 0) ([] :: [GridCoord]))
 ,Test "allCoords (-1) (-1)" (assertEqual (allCoords (-1) (-1)) ([] :: [GridCoord]))]


-- ================== toQR Tests     ================= --
-- | Tests it produces right output for the two main input cases 'A' and ' '
-- |>> (1)&(2) for above
-- |>> (3) tests edge case of '9' -> Dead
toQRTests :: [Test]
toQRTests = [Test "toQR \'A\'" (assertEqual (toQR 'A') (Alive::QRCell))
 ,Test "toQR \' \'" (assertEqual (toQR ' ') (Dead::QRCell))
 ,Test "toQR \'9\'" (assertEqual (toQR '9') (Dead::QRCell))]


-- ================== evolveQR Tests ================= --
-- | Checks that 1100 reaches the alternating pattern and that 1130 reaches steady state as specified
-- | These tests are dependant on nextGenQR working and so also test nextGenQR
-- |>> (1) Checks evolutions 12 and 14 of 1100 pattern are equal
-- |>> (2) Checks evolutions 13 and 15 of 1100 pattern are equal
-- |>> (3) Checks evolutions 20 and 22 of 1130 pattern are equal
evolveQRTests :: [Test]
evolveQRTests = [Test "evolveQR: 1100 12 and 14 ==" (assertEqual (evolveQR 12 pattern1100) ((evolveQR 14 pattern1100) :: Grid QRCell))
 ,Test "evolveQR: 1100 13 and 15 ==" (assertEqual (evolveQR 13 pattern1100) ((evolveQR 15 pattern1100) :: Grid QRCell))
 ,Test "evolveQR: 1130 20 and 22 ==" (assertEqual (evolveQR 20 pattern1130) ((evolveQR 22 pattern1130) :: Grid QRCell))]

-- ================== countAlive     ================= --
-- | Tests that countAlive is working for typical inputs
-- | Due to typing there are no edge cases
-- |>> (1) all Nothing whereas (2) mixed values
countAliveTests :: [Test]
countAliveTests = [Test "countAlive: all nothing" (assertEqual (countAlive [Nothing, Nothing, Nothing, Nothing]) (0 :: Int))
 ,Test "countAlive: mixed grill" (assertEqual (countAlive [Just Dead, Nothing, Just Alive, Just Alive]) (2 :: Int))]

-- ================== renderQR        ================= --
-- | Decided could be tested by just looking at the GUI
{-
renderQRTests :: [Test]
renderQRTests = [Test "renderQR Alive" (assertEqual (renderQR: Alive) ((coloured blue  (solidRectangle 1 1))::Picture))
 ,Test "renderQR Dead" (assertEqual (renderQR: Dead) ((coloured black  (rectangle 1 1))::Picture))]
-}
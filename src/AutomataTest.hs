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
getTests :: [Test]
getTests = [Test "get: Bottom L 1100" (assertEqual (get pattern1100 (10,2)) ((Just Alive) :: Maybe QRCell))
 ,Test "get: (1,0) 1100" (assertEqual (get pattern1100 (1,0)) ((Just Dead) :: Maybe QRCell))
 ,Test "get: (0,1)" (assertEqual (get (Grid 2 2 [Alive, Alive, Dead, Dead]) (0,1)) ((Just Dead) ::Maybe QRCell))
 ,Test "get: (0,1)" (assertEqual (get (Grid 2 2 [Alive, Alive, Dead, Dead]) (0,3)) (Nothing ::Maybe c)) ]


-- ================== allCoords Test ================= --
-- | Tests it produces an appropriate grid
allCoordsTest :: [Test]
allCoordsTest = [Test "allCoords 2 2" (assertEqual (allCoords 2 2) ([(0,0),(1,0),(0,1),(1,1)] :: [GridCoord]))
 ,Test "nList 5" (assertEqual (nList 5) ([0,1,2,3,4,5] :: [Int]))]


-- ================== toQR Tests     ================= --
-- | Tests it produces right output for the two main input cases
toQRTests :: [Test]
toQRTests = [Test "toQR \'A\'" (assertEqual (toQR 'A') (Alive::QRCell))
 ,Test "toQR \' \'" (assertEqual (toQR ' ') (Dead::QRCell))]


-- ================== evolveQR Tests ================= --
-- | Checks evolutions 10 and 12 of 1100 pattern are equal
evolveQRTests :: [Test]
evolveQRTests = [Test "evolveQR: 1100 12 and 14 ==" (assertEqual (evolveQR 12 pattern1100) ((evolveQR 14 pattern1100) :: Grid QRCell))
 ,Test "evolveQR: 1130 20 and 22 ==" (assertEqual (evolveQR 20 pattern1130) ((evolveQR 22 pattern1130) :: Grid QRCell))]

-- ================== countAlive     ================= --
countAliveTests :: [Test]
countAliveTests = [Test "countAlive: all nothing" (assertEqual (countAlive [Nothing, Nothing, Nothing, Nothing]) (0 :: Int))
 ,Test "countAlive: mixed grill" (assertEqual (countAlive [Just Dead, Nothing, Just Alive, Just Alive]) (2 :: Int))]

-- renderQR Tests
{-
renderQRTests :: [Test]
renderQRTests = [Test "renderQR Alive" (assertEqual (renderQR: Alive) ((coloured blue  (solidRectangle 1 1))::Picture))
 ,Test "renderQR Dead" (assertEqual (renderQR: Dead) ((coloured black  (rectangle 1 1))::Picture))]
-}

-- | Example test case. The String argument to 'Test' is a label that
-- identifies the test, and gives the reader some idea about what's
-- being tested. For simple arithmetic, these should be obvious, but
-- when you write tests for your code, you can use this space to say
-- things like "the next state for a cell with 3 live neighbours is
-- 'Alive'".
--
-- You should remove this test before submitting your assignment.

{-
exampleTest :: Test
exampleTest = Test "2 + 2 == 4" (assertEqual (2 + 2) (4 :: Int))
-}

-- | This test will fail, so you can see what a failing test looks
-- like.
--
-- You should remove this test before submitting your assignment.

{-
exampleFailure :: Test
exampleFailure = Test "0.1 + 0.2 == 0.3" (assertEqual (0.1 + 0.2) (0.3 :: Double))
-}
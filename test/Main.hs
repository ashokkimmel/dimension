{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Main where 
import System.Exit (exitFailure)
import Dimensions.Units
import Dimensions.GetTermLevel as TT
import TypeLevelTests (TypeLevelTests)

test :: (Eq a,Show a) => String -> a -> a -> IO ()
test name expect got = 
  if expect == got 
    then putStrLn ("The test \"" <> name <> "\" succeeded!") 
    else (putStrLn $ "Expected " <> show expect <> "\nbut got: " <> show got <> "\n while running " <> name) *> exitFailure

testFloating :: forall k (n :: [(k,Int')]) a. (Fractional a,Ord a,Show a) => a -> String -> Dimension n a -> Dimension n a -> IO ()
testFloating epsilon name (MkDimension expect) (MkDimension got) = 
  if abs (expect - got) < epsilon
    then putStrLn ("The test \"" <> name <> "\" succeeded!") 
    else (putStrLn $ "Expected " <> show expect <> "\nbut got: " <> show got <> "\n while running " <> name) *> exitFailure

testFloatingDef :: (n~m,Double ~ a) => String -> Dimension n a -> Dimension m a -> IO ()
testFloatingDef = testFloating 0.00001

main :: TypeLevelTests => IO () 
main = do 
  putStrLn "\nTests starting: \n\n\n"
  applyPosTest
  applyNegTest
  transformPosTest
  plusTest
  minusTest
  timesTest
  fracTest
  divDTest
  combineD2Test
  combineInvD2Test
  undimensionTest
  getDimensionTest
  dimensionTest
  specialCharsTest
  longStringTest
  complexUnitsTest
  
  putStrLn "\n\n\nTests finished\n"


-- Individual test functions for easier compartmentalization
applyPosTest :: IO ()
applyPosTest = do
  test
    "applyPos works correct amount of times (test 1)"
    (applyPos "check" succ (0 `dim` "check^10"))
    10
  test
    "applyPos works correct amount of times (test 2)"
    (applyPos "x" succ (5 `dim` "x^3"))
    8

applyNegTest :: IO ()
applyNegTest = do
  test
    "applyNeg works correct amount of times (test 1)"
    (applyNeg "check" pred (0 `dim` "check^-10"))
    (-10)
  test
    "applyNeg works correct amount of times (test 2)"
    (applyNeg "y" pred (100 `dim` "y^-5"))
    95

transformPosTest :: IO ()
transformPosTest = do
  test
    "transformPos works the correct amount of times (test 1)"
    (transformPos
      "check"
      "result"
      succ
      (0 `dim` "check^10"))
    (10 `dim` "result^10")
  test
    "transformPos works the correct amount of times (test 2)"
    (transformPos
      "a"
      "b"
      (*2)
      (1 `dim` "a^3"))
    (8 `dim` "b^3")

plusTest :: IO ()
plusTest = do
  test
    "!+ works correctly (test 1)"
    ( (3 `dim` "m") !+ (4 `dim` "m") )
    (7 `dim` "m")
  test
    "!+ works correctly (test 2)"
    ( (10 `dim` "s") !+ (15 `dim` "s") )
    (25 `dim` "s")

minusTest :: IO ()
minusTest = do
  test
    "!- works correctly (test 1)"
    ( (10 `dim` "m") !- (4 `dim` "m") )
    (6 `dim` "m")
  test
    "!- works correctly (test 2)"
    ( (100 `dim` "kg") !- (25 `dim` "kg") )
    (75 `dim` "kg")

timesTest :: IO ()
timesTest = do
  test
    "!* works correctly (test 1)"
    ( (3 `dim` "m") !* (4 `dim` "m") )
    (12 `dim` "m^2")
  test
    "!* works correctly (test 2)"
    ( (5 `dim` "s") !* (6 `dim` "s") )
    (30 `dim` "s^2")

fracTest :: IO ()
fracTest = do
  testFloatingDef
    "!/ works correctly (test 1)"
    ( (12 `dim` "m^2") !/ (4 `dim` "m") )
    (3 `dim` "m")
  testFloatingDef
    "!/ works correctly (test 2)"
    ( (20 `dim` "kg*m") !/ (5 `dim` "kg") )
    (4 `dim` "m")

divDTest :: IO ()
divDTest = do
  test
    "divD works properly (test 1)"
    ((10 `dim` "m^2") `divD` (2 `dim` "m") )
    (5 `dim` "m")
  test
    "divD works properly (test 2)"
    ((24 `dim` "s^3") `divD` (4 `dim` "s") )
    (6 `dim` "s^2")

combineD2Test :: IO ()
combineD2Test = do
  test
    "combineD2 works correctly (test 1)"
    ( combineD2 (+) (3 `dim` "m") (4 `dim` "m") )
    (7 `dim` "m^2")
  test
    "combineD2 works correctly (test 2)"
    ( combineD2 (*) (2 `dim` "s") (5 `dim` "s") )
    (10 `dim` "s^2")

combineInvD2Test :: IO ()
combineInvD2Test = do
  test
    "combineInvD2 works correctly (test 1)"
    ( combineInvD2 (+) (3 `dim` "m^2") (4 `dim` "m") )
    (7 `dim` "m")
  test
    "combineInvD2 works correctly (test 2)"
    ( combineInvD2 (*) (15 `dim` "kg^3") (3 `dim` "kg") )
    (45 `dim` "kg^2")

undimensionTest :: IO ()
undimensionTest = do
  test
    "undimension works correctly (test 1)"
    ( undimension (3 `dim` "") )
    (3 :: Int)
  test
    "undimension works correctly (test 2)"
    ( undimension (42 `dim` "") )
    (42 :: Int)

getDimensionTest :: IO ()
getDimensionTest = do
  test
    "getDimension works correctly (test 1)"
    ( getDimension @"m" (3 `dim` "m") )
    (3 :: Int)
  test
    "getDimension works correctly (test 2)"
    ( getDimension @"s" (99 `dim` "s") )
    (99 :: Int)

dimensionTest :: IO ()
dimensionTest = do
  test
    "dimension/type helpers work (test 1)"
    ( dimension @"m" (5 :: Int) )
    (5 `dim` "m")
  test
    "dimension/type helpers work (test 2)"
    ( dimension @"kg" (17 :: Int) )
    (17 `dim` "kg")

specialCharsTest :: IO ()
specialCharsTest = do
  test
    "Special characters - underscore (test 1)"
    ( (5 `dim` "my_unit") !+ (3 `dim` "my_unit") )
    (8 `dim` "my_unit")
  test
    "Special characters - numbers (test 2)"
    ( (10 `dim` "unit123") !* (2 `dim` "unit123") )
    (20 `dim` "unit123^2")

longStringTest :: IO ()
longStringTest = do
  test
    "Long string units (test 1)"
    ( (7 `dim` "kilogram") !+ (3 `dim` "kilogram") )
    (10 `dim` "kilogram")
  test
    "Long string units (test 2)"
    ( (15 `dim` "meter") !- (5 `dim` "meter") )
    (10 `dim` "meter")

complexUnitsTest :: IO ()
complexUnitsTest = do
  test
    "Complex units - high exponents (test 1)"
    ( (2 `dim` "x^10") !* (3 `dim` "x^5") )
    (6 `dim` "x^15")
  testFloatingDef
    "Complex units - division with long names (test 2)"
    ( (100 `dim` "Newton*meter") !/ (10 `dim` "Newton") )
    (10 `dim` "meter")

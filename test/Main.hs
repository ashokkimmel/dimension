{-# LANGUAGE TypeOperators #-}
module Main where 
import System.Exit (exitFailure)
import Dimensions.Units
test :: (Eq a,Show a) => String -> a -> a -> IO ()
test name expect got = 
  if expect == got 
    then putStrLn ("The test \"" <> name <> "\" succeeded!") 
    else (putStrLn $ "Expected " <> show expect <> "\nbut got: " <> show got <> "\n while running " <> name) *> exitFailure
testFloating :: (Fractional a,Ord a,Show a) => a -> String -> Dimension n a -> Dimension n a -> IO ()
testFloating epsilon name (MkDimension expect) (MkDimension got) = 
  if abs (expect - got) < epsilon
    then putStrLn ("The test \"" <> name <> "\" succeeded!") 
    else (putStrLn $ "Expected " <> show expect <> "\nbut got: " <> show got <> "\n while running " <> name)*> exitFailure
testFloatingDef :: (n~m,Double ~ a) => String -> Dimension n a -> Dimension m a -> IO ()
testFloatingDef = testFloating 0.00001

main = do 
  putStrLn "\nTests starting: \n\n\n"
  
  test 
    "applyPos works correct amount of times" 
    (applyPos "check" succ (0 `dim` "check^10")) 
    10
  test 
    "applyNeg works correct amount of times" 
    (applyNeg "check" pred (0 `dim` "check^-10")) 
    (-10)
  test
    "transformPos works the correct amount of times"
    (transformPos 
      "check"
      "result"
      succ 
      (0 `dim` "check^10"))
    (10 `dim` "result^10")
  test 
    "!+ works correctly"
    ( (3 `dim` "m") !+ (4 `dim` "m") )
    (7 `dim` "m")
  test 
    "!- works correctly"
    ( (10 `dim` "m") !- (4 `dim` "m") )
    (6 `dim` "m")
  test 
    "!* works correctly"
    ( (3 `dim` "m") !* (4 `dim` "m") )
    (12 `dim` "m^2")
--  testFloatingDef
--    "!/ works correctly"
--    ( (12 `dim` "m^2") !/ (4 `dim` "m") )
--    (3 `dim` "m")
  test
    "divD works properly"
    ((10 `dim` "m^2") `divD` (2 `dim` "m") )
    (5 `dim` "m")
  
  putStrLn "\n\n\nTests finished\n"

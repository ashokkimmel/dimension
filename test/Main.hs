module Main where 
import Dimensions.Units
test :: (Eq a,Show a) => String -> a -> a -> IO ()
test name expect got = 
  if expect == got 
    then putStrLn ("The test \"" <> name <> "\" succeeded!") 
    else putStrLn $ "Expected " <> show expect <> "\nbut got: " <> show got <> "\n while running " <> name



main = do 
  putStrLn "\nTests starting: \n\n\n"
  test "ApplyPos works correct amount of times" (applypos "check" succ (0 `dim` "check^10")) 10
  putStrLn "\n\n\nTests finished\n"

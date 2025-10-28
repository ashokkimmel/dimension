module Main where 

test :: (Eq a,Show a) => String -> a -> a -> IO ()
test name expect got = if expect = got then putStrLn (name <> " succeeded!") else putStrLn $ "Expected " <> show expect <> "\nbut got: " show got "\n while running " <> name



main = do 
  test "ApplyPos works correct amount of times." (applyPos "check" succ (0 `dim` "check^10")) 10


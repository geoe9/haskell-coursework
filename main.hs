tupleToList :: (Int, Int, Int, Int, Int, Int) -> [Int]
tupleToList (a, b, c, d, e, f)
  = [a, b, c, d, e, f]

-- Question 1.1
rule1 :: [Int] -> Bool
rule1 []
  = True
rule1 (x:xs)
  = x `notElem` xs && rule1 xs

main :: IO()
main
  = putStrLn (show (rule1 (tupleToList (1,2,3,3,5,6))))
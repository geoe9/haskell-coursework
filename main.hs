tupleToList :: (Int, Int, Int, Int, Int, Int) -> [Int]
tupleToList (a, b, c, d, e, f)
  = [a, b, c, d, e, f]

-- Question 1.1
rule1 :: [Int] -> Bool
rule1 []
  = True
rule1 (x:xs)
  = x `notElem` xs && rule1 xs

-- Question 1.2
rule2 :: [Int] -> Bool
rule2 []
  = True
rule2 [x]
  = True
rule2 (x:y:xs)
  = x `mod` 2 /= y `mod` 2 && rule2 (y:xs)

-- Question 1.3
rule3 :: [Int] -> Bool
rule3 []
  = True
rule3 [x]
  = True
rule3 (x:y:xs)
  = abs (x - y) > 2 && rule3 (y:xs)

main :: IO()
main
  = putStrLn (show (rule3 (tupleToList (4,9,6,3,0,7))))
tupleToList :: (Int, Int, Int, Int, Int, Int) -> [Int]
tupleToList (a, b, c, d, e, f)
  = [a, b, c, d, e, f]

-- Question 1.1
rule1 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule1
  = uniqueElems . tupleToList

uniqueElems :: [Int] -> Bool
uniqueElems []
  = True
uniqueElems (x:xs)
  = x `notElem` xs && uniqueElems xs

-- Question 1.2
rule2 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule2
  = alternatingEvens . tupleToList

alternatingEvens :: [Int] -> Bool
alternatingEvens []
  = True
alternatingEvens [x]
  = True
alternatingEvens (x:y:xs)
  = x `mod` 2 /= y `mod` 2 && alternatingEvens (y:xs)

-- Question 1.3
rule3 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule3
  = diffGreaterThanTwo . tupleToList

diffGreaterThanTwo :: [Int] -> Bool
diffGreaterThanTwo []
  = True
diffGreaterThanTwo [x]
  = True
diffGreaterThanTwo (x:y:xs)
  = abs (x - y) > 2 && diffGreaterThanTwo (y:xs)

-- Question 1.4
rule4 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule4 (a, b, c, d, e, f)
  = read (show(a) ++ show(b)) `mod` r == 0 && read (show(c) ++ show(d)) `mod` r == 0
    where
    r = read (show(e) ++ show(f))

main :: IO()
main
  = putStrLn (show (rule3 (4,9,6,3,0,7)))
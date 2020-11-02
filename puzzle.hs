import Data.Char (digitToInt)

tupleToList :: (Int, Int, Int, Int, Int, Int) -> [Int]
tupleToList (a, b, c, d, e, f)
  = [a, b, c, d, e, f]

listToTuple :: [Int] -> (Int, Int, Int, Int, Int, Int)
listToTuple [a]
  = (0, 0, 0, 0, 0, a)
listToTuple [a, b]
  = (0, 0, 0, 0, a, b)
listToTuple [a, b, c]
  = (0, 0, 0, a, b, c)
listToTuple [a, b, c, d]
  = (0, 0, a, b, c, d)
listToTuple [a, b, c, d, e]
  = (0, a, b, c, d, e)
listToTuple [a, b, c, d, e, f]
  = (a, b, c, d, e, f)

{-- Question 1.1 --}
rule1 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule1
  = uniqueElems . tupleToList

uniqueElems :: [Int] -> Bool
uniqueElems []
  = True
uniqueElems (x:xs)
  = x `notElem` xs && uniqueElems xs

{-- Question 1.2 --}
rule2 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule2
  = alternatingEvens . tupleToList

alternatingEvens :: [Int] -> Bool
alternatingEvens []
  = True
alternatingEvens [_]
  = True
alternatingEvens (x:y:xs)
  = x `mod` 2 /= y `mod` 2 && alternatingEvens (y:xs)

{-- Question 1.3 --}
rule3 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule3
  = diffGreaterThanTwo . tupleToList

diffGreaterThanTwo :: [Int] -> Bool
diffGreaterThanTwo []
  = True
diffGreaterThanTwo [_]
  = True
diffGreaterThanTwo (x:y:xs)
  = abs (x - y) > 2 && diffGreaterThanTwo (y:xs)

{-- Question 1.4 --}
rule4 :: (Int, Int, Int, Int, Int, Int) -> Bool
rule4 (a, b, c, d, e, f)
  = read (show(a) ++ show(b)) `mod` r == 0 && read (show(c) ++ show(d)) `mod` r == 0
  where
  r = read (show(e) ++ show(f))

{-- Question 1.5 --}
possibles :: [(Int, Int, Int, Int, Int, Int)]
possibles
  = map listToTuple (map (map digitToInt . show) [0..999999])

{-- Question 1.6 --}
isSolution :: (Int, Int, Int, Int, Int, Int) -> Bool
isSolution x
  = and $ map ($ x) [rule1, rule2, rule3, rule4]

main :: IO()
main
  = putStrLn (show (filter isSolution possibles))

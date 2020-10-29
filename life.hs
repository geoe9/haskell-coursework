-- Question 2.1
pretty :: [[String]] -> String
pretty xs
  = unlines $ concat $ xs

-- Question 2.2
type Point
  = (Int, Int)

glider :: [Point]
glider
  = [(0,2),(1,3),(2,1),(2,2),(2,3)]

gridPoints :: Int -> [Point] -> [[Int]]
gridPoints height xs
  = [rowPoints i xs | i <- [0..(height - 1)]]

rowPoints :: Int -> [Point] -> [Int]
rowPoints i []
  = []
rowPoints i ((a,b):xs)
  | i == b = a : (rowPoints i xs)
  | otherwise = rowPoints i xs

createGrid :: Int -> Int -> [Point] -> [String]
createGrid w h xs
  = map (createRow w) (gridPoints h xs)

createRow :: Int -> [Int] -> String
createRow width xs
  = [createPoint i xs | i <- [0..(width - 1)]]

createPoint :: Int -> [Int] -> Char
createPoint i xs
  | i `elem` xs = '#'
  | otherwise = '.'

visualisation :: Int -> Int -> [[Point]] -> [[String]]
visualisation w h xs
  = map (createGrid w h) xs

main :: IO()
main
  = putStrLn( show (visualisation 5 5 [ glider ] ))
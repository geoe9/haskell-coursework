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

createRow :: Int -> [Int] -> String
createRow width xs
  = [createPoint i xs | i <- [0..width]]

createPoint :: Int -> [Int] -> Char
createPoint i xs
  | i `elem` xs = '#'
  | otherwise = '.'

gridPoints :: Int -> [Point] -> [[Int]]
gridPoints height xs
  = [rowPoints i xs | i <- [0..height]]

rowPoints :: Int -> [Point] -> [Int]
rowPoints i []
  = []
rowPoints i ((a,b):xs)
  | i == b = a : (rowPoints i xs)
  | otherwise = rowPoints i xs

createGrid :: Int -> Int -> [Point] -> [String]
createGrid w h xs
  = map (createRow w) (gridPoints h xs)

visualisation :: Int -> Int -> [[Point]] -> [[String]]
visualisation w h xs
  = map (createGrid w h) xs

main :: IO()
main
  = putStrLn( show (visualisation 5 5 [ glider ] ))

-- Question 2.3
evolution :: [Point] -> [[Point]]
evolution xs
  = [xs] -- [xs : evolve xs]

-- Rules --
-- 1. Any live cell with two or three live neighbours survives.
-- 2. Any dead cell with three live neighbours becomes alive.
-- 3. All other live cells die, and all other dead cells stay dead.
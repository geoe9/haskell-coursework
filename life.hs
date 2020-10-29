-- Question 2.1
pretty :: [[String]] -> String
pretty xs
  = (unlines $ concat $ xs)

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

-- Question 2.3
livingNeighbourCount :: Point -> [Point] -> Int
livingNeighbourCount p ps
  = length $ filter (areNeighbours p) ps

areNeighbours :: Point -> Point -> Bool
areNeighbours (a1,b1) (a2,b2)
  = abs(a1-a2) < 2 && abs(b1-b2) < 2 && (a1,b1) /= (a2,b2)

survivingCell :: Point -> [Point] -> Bool
survivingCell p ps
  = (livingNeighbourCount p ps == 2 || livingNeighbourCount p ps == 3) && p `elem` ps

allCells :: Int -> Int -> [(Int, Int)]
allCells w h
  = [(x, y) | x <- [0..w], y <- [0..h]]

possibleCells :: [Point] -> [Point]
possibleCells ps
  = 

newCell :: Point -> [Point] -> Bool
newCell p ps
  = livingNeighbourCount p ps == 3 && p `notElem` ps

evolve :: [Point] -> [Point]
evolve ps
  = [p | p <- (allCells 5 5), (survivingCell p ps || newCell p ps)]

evolution :: [Point] -> [[Point]]
evolution ps
  = iterate evolve ps

main :: IO()
main
  -- =  putStrLn (pretty (take 8 (visualisation 5 5 (evolution glider))))
  = putStrLn (show (gridPoints 5 glider))
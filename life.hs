{-- Question 2.1 --}
pretty :: [[String]] -> String
pretty xs
  = unlines $ concat $ xs

{-- Question 2.2 --}
type Point
  = (Int, Int)

glider :: [Point]
glider
  = [(0,2),(1,3),(2,1),(2,2),(2,3)]

createRow :: Int -> [Int] -> String
createRow w xs
  = [createPoint i xs | i <- [0..w]]

createPoint :: Int -> [Int] -> Char
createPoint i xs
  | i `elem` xs = '#'
  | otherwise = '.'

gridPoints :: Int -> [Point] -> [[Int]]
gridPoints h xs
  = [rowPoints i xs | i <- [0..h]]

rowPoints :: Int -> [Point] -> [Int]
rowPoints _ []
  = []
rowPoints i ((a,b):xs)
  | i == b = a : (rowPoints i xs)
  | otherwise = rowPoints i xs

createGrid :: Int -> Int -> [Point] -> [String]
createGrid w h xs
  = map (createRow w) (gridPoints h xs)

visualisation :: Int -> Int -> [[Point]] -> [[String]]
visualisation w h
  = map $ createGrid w h

{-- Question 2.3 --}
livingNeighbourCount :: Point -> [Point] -> Int
livingNeighbourCount p ps
  = length $ filter (areNeighbours p) ps

areNeighbours :: Point -> Point -> Bool
areNeighbours (a1,b1) (a2,b2)
  = abs(a1-a2) < 2 && abs(b1-b2) < 2 && (a1,b1) /= (a2,b2)

survivingCell :: Point -> [Point] -> Bool
survivingCell p ps
  = (lnc == 2 || lnc == 3) && p `elem` ps
  where
  lnc = livingNeighbourCount p ps

possibleCells :: [Point] -> [Point]
possibleCells xs
  = [(x, y) | x <- [la..ha], y <- [lb..hb]]
  where
  (as, bs) = unzip xs
  la = foldr1 min as - 1
  ha = foldr1 max as + 1
  lb = foldr1 min bs - 1
  hb = foldr1 max bs + 1

newCell :: Point -> [Point] -> Bool
newCell p ps
  = livingNeighbourCount p ps == 3 && p `notElem` ps

evolve :: [Point] -> [Point]
evolve ps
  = [p | p <- possibleCells ps, survivingCell p ps || newCell p ps]

evolution :: [Point] -> [[Point]]
evolution
  = iterate evolve

main :: IO()
main
 =  putStrLn (pretty (take 8 (visualisation 5 5 (evolution glider))))

-- Question 2.1
pretty :: [[String]] -> String
pretty xs
  = unlines $ concat $ xs

-- Question 2.2
type Point
  = ( Int, Int )

glider :: [ Point ]
glider
  = [(0,2),(1,3),(2,1),(2,2),(2,3)]

--visualisation :: Int -> Int -> [[Point]] -> [[String]]

createGrid :: Int -> Int -> [[String]]
createGrid w h
  = replicate h [concat $ replicate w "."]

main :: IO()
main
  = putStrLn( show (createGrid 5 5))

--main :: IO()
--main
--  = putStrLn( show( pretty [[['a', 'b'], ['c', 'd']], [['e', 'f'], ['g', 'h']], [['i', 'j'], ['k', 'l']]]))
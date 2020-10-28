-- Question 2.1
pretty :: [[String]] -> String
pretty xs
  = unlines $ concat $ xs

main :: IO()
main
  = putStrLn( show( pretty [[['a', 'b'], ['c', 'd']], [['e', 'f'], ['g', 'h']], [['i', 'j'], ['k', 'l']]]))
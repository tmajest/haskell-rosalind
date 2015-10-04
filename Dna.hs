module Dna (countBases) where

countBases :: String -> [Int]
countBases [] = [0, 0, 0, 0]
countBases (x:xs) 
    | x == 'A' = [rest !! 0 + 1, rest !! 1, rest !! 2, rest !! 3]
    | x == 'C' = [rest !! 0, rest !! 1 + 1, rest !! 2, rest !! 3]
    | x == 'G' = [rest !! 0, rest !! 1, rest !! 2 + 1, rest !! 3]
    | x == 'T' = [rest !! 0, rest !! 1, rest !! 2, rest !! 3 + 1]
    where rest = (countBases xs)

main = do
    dna <- getLine 
    let baseCount = countBases dna
    putStrLn $ unwords (map show baseCount)

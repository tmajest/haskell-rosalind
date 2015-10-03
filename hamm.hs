
--
-- |Count the number of differences in the two given strings.
-- 
diffCount :: String -> String -> Int
diffCount x y = length $ filter id $ zipWith (/=) x y

main = do
    dna1 <- getLine
    dna2 <- getLine
    let
        count = diffCount dna1 dna2
    putStrLn $ show count

    
    
        

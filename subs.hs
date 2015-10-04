import Strings

--
-- |Finds all the indexes where string s is present in string t.
--
subs :: String -> String -> Int -> [Int]
subs [] _ _ = []
subs s t index
    | take (length t) s == t = index : subs (tail s) t (index + 1)
    | otherwise = subs (tail s) t (index + 1)

main = do
    s <- getLine
    t <- getLine

    let indexes = subs s t 1
    putStrLn $ listToString indexes

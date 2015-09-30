
complement :: Char -> Char
complement x
    | x == 'A' = 'T'
    | x == 'C' = 'G'
    | x == 'G' = 'C'
    | x == 'T' = 'A'

main = do
    dna <- getLine
    let revc = reverse $ map complement dna
    putStrLn revc

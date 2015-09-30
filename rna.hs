
transcribe :: Char -> Char
transcribe x = if x == 'T' then 'U' else x

main = do
    dna <- getLine
    let rna = map transcribe dna
    putStrLn rna

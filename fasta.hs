module Fasta (Entry) where

data Entry = Entry {
    identifier :: String, 
    dna :: String 
}

parseFasta :: [String] -> [Entry]
parseFasta [] = []
parseFasta (x:xs) =
    let 
        result = parseHelper xs
        dna  = fst result
        rest = snd result
        entry = Entry (tail x) dna
    in
        entry : parseFasta rest

parseHelper :: [String] -> (String, [String])
parseHelper [] = ([], [])
parseHelper (x:xs)
    | head x == '>' = ([], x:xs)
    | otherwise =
        let acc = fst rest
        in (x ++ acc, snd rest)
    where rest = parseHelper xs



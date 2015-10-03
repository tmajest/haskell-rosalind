module Fasta (Entry(..), parseFasta) where

data Entry = Entry {
    identifier :: String, 
    dna :: String 
}

parseFasta :: [String] -> [Entry]
parseFasta arr = map convertEntry $ tail (fastaHelper arr)

fastaHelper :: [String] -> [[String]]
fastaHelper [] = [[]]
fastaHelper (x:xs)
    | head x == '>' = [] : (x : hRes) : tRes
    | otherwise = (x : hRes) : tRes
    where 
        result = fastaHelper xs
        hRes = head result
        tRes = tail result

convertEntry :: [String] -> Entry
convertEntry (x:xs) = Entry (tail x) (concat xs)

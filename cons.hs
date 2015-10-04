import Fasta
import Dna

--
-- |Transpose the given matrix.
-- http://stackoverflow.com/questions/2578930/
-- understanding-this-matrix-transposition-function-in-haskell
-- 
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

--
-- Finds the consensus string (the bases that appear the most often
-- in each position of the given DNA strings).
--
-- Input is a list of Int lists, where each Int list is the number of
-- times each base (ACGT) appears in the given position of the DNA strings.
--
consensus :: [[Int]] -> String
consensus [] = []
consensus (x:xs) = maxBase : consensus xs
    where
        zipped = zip x ['A', 'C', 'G', 'T']
        maxBase = snd $ maximum zipped

--
-- Creates the DNA profile from the given base counts.
--
createProfile :: [[Int]] -> [String]
createProfile x = map (uncurry (++)) zipped
    where
        strings = map (\a -> unwords $ map show a) (transpose x)
        zipped = zip ["A: ", "C: ", "G: ", "T: "] strings

main = do
    input <- getContents
    let 
        entries = parseFasta $ lines input
        matrix = map dna entries
        transposed = transpose matrix
        dnaCounts = map countBases transposed

        cons = consensus dnaCounts
        profile = createProfile dnaCounts

    putStrLn cons
    putStrLn $ unlines profile

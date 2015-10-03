import Fasta
import Data.List
import Data.Ord

--
-- |Calculates the percentage of Cytosine or Guanine in
--  entry's DNA string.
--
gcContents :: Entry -> Float
gcContents entry = (numGc / total) * 100
    where 
        dnaStr = dna entry
        gc = filter (`elem` ['G', 'C']) $ dnaStr
        numGc = fromIntegral $ length gc
        total = fromIntegral $ length dnaStr

--
-- |Returns the Fasta entry with the highest GC content.
--
maxGcContent :: [(Entry, Float)] -> (Entry, Float)
maxGcContent = maximumBy (comparing snd)

main = do
    input <- getContents

    let 
        entries = parseFasta $ lines input
        tuples = map (\e -> (e, gcContents e)) entries
        maxGcTuple = maxGcContent tuples
        
        maxId = identifier $ fst maxGcTuple
        maxContent = snd maxGcTuple

    putStrLn maxId
    putStrLn $ show maxContent
        

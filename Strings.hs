module Strings (listToString) where

--
-- |Converts a list of Showables to a single string with a
-- space delimiter.
--
listToString :: (Show a) => [a] -> String
listToString x = unwords $ map show x

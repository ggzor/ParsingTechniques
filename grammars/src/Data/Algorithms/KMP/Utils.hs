module Data.Algorithms.KMP.Utils where
import Data.Algorithms.KMP
import Data.Maybe (listToMaybe)

subIndex :: Eq a => [a] -> [a] -> Maybe Int
subIndex sublist list = listToMaybe (subIndices sublist list)

subIndices :: Eq a => [a] -> [a] -> [Int]
subIndices = match . build
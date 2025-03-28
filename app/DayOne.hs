module DayOne (
  readInput, part1, part2
  ) where

import qualified Data.List as List (lookup)
import qualified Data.List.NonEmpty as NonEmpty


inputFile :: String
inputFile = "day-1-input.txt"


readInput :: IO (NonEmpty.NonEmpty (Int, Int))
readInput = do
  s <- readFile inputFile
  let lns = lines s
  let pairs = map splitLine lns
  return $ NonEmpty.fromList pairs


splitLine :: String -> (Int, Int)
splitLine l = 
  let a = takeWhile (/= ' ') l
      b = dropWhile (== ' ') $ dropWhile (/= ' ') l
  in (read a, read b)


sortPairs :: NonEmpty.NonEmpty (Int, Int) -> NonEmpty.NonEmpty (Int, Int)
sortPairs pairs =
  let left = NonEmpty.sort $ NonEmpty.map fst pairs
      right = NonEmpty.sort $ NonEmpty.map snd pairs
      --right = NonEmpty.sort (NonEmpty.map snd pairs)
  in NonEmpty.zip left right


runLengthEncode :: NonEmpty.NonEmpty a -> (a, Int)
runLengthEncode z = (NonEmpty.head z, NonEmpty.length z)


similarity :: NonEmpty.NonEmpty (Int, Int) -> Int -> Int
similarity dict val = maybe 0 (*val) (nonEmptyLookup val dict)


nonEmptyLookup :: Eq a => a -> NonEmpty.NonEmpty (a, b) -> Maybe b
nonEmptyLookup v d = List.lookup v (NonEmpty.toList d)


part1 :: NonEmpty.NonEmpty (Int, Int) -> Int
part1 pairs =
  let sortedPairs = sortPairs pairs
      differences = NonEmpty.map (abs . uncurry (-)) sortedPairs
  in sum differences


part2 :: NonEmpty.NonEmpty (Int, Int) -> Int
part2 pairs =
  let left = NonEmpty.sort $ NonEmpty.map fst pairs
      right = NonEmpty.sort $ NonEmpty.map snd pairs
      groups = NonEmpty.fromList $ NonEmpty.group right
      dict = NonEmpty.map runLengthEncode groups
      simF = similarity dict
      sims = NonEmpty.map simF left
  in sum sims 
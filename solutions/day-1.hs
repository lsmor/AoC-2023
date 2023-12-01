module Main where

import System.Environment (getArgs)
import Data.ByteString.Char8 qualified as B
import Data.Char qualified as Char 
import Data.Trie qualified as Trie

type Input    = [(Int, Int)]  -- We should parse our input string into a list of pairs of ints. One pair for each line
type Solution = Int           -- The requested solution is an Int

parseLine :: B.ByteString -> (Int, Int)
parseLine bs = (getNumber bs, getNumber (B.reverse bs))
  where getNumber = maybe 0 Char.digitToInt . B.find Char.isDigit
        -- | Find the first char which is a digit. B.find returns Optional value (impossible in AoC input)
        -- If Nothing, we just return 0, else we transform the Character into to the Int it represent

parseWithTrie :: B.ByteString -> (Int, Int)
parseWithTrie bs = (go trie bs, go trie_rev $ B.reverse bs)
  where 
    -- We are lazy and use a Trie data structure. You can just pattern a huge number of things
    -- Step1: "stwo1hey"  << we try to match with the Trie
    -- Step2: "two1hey"   << we drop the first "s" and match the and match again against the trie. 
    -- Step : Just 2      << The string "two1hey" matches 2 in the trie
    go tr s = 
      case tr `Trie.match` s of
          Nothing |  B.null s  -> 0
          Nothing              -> go tr (B.drop 1 s) 
          Just (_, i, _)       -> i

trie :: Trie.Trie Int
trie = Trie.fromList [("one", 1), ("two", 2), ("three", 3), ("four",4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9), ("1", 1), ("2", 2), ("3", 3), ("4",4), ("5", 5), ("6", 6), ("7", 7), ("8", 8), ("9", 9)]

trie_rev :: Trie.Trie Int
trie_rev = Trie.fromList . fmap (\(k, v) -> (B.reverse k , v)) . Trie.toList $ trie


-- | parsers take a raw bytestring, break it into lines, and map the line parser for each line
parser :: B.ByteString -> Input
parser = fmap parseLine . B.lines

parser2 :: B.ByteString -> Input
parser2 = fmap parseWithTrie . B.lines

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = sum . fmap (\(x, y) -> x*10 + y)

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = solve1

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- B.readFile filepath -- use parser <$> readFile filepath if String is better
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 (parser input)
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 $ parser2 input


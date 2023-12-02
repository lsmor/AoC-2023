module Main where

import System.Environment (getArgs)
import Data.ByteString.Char8 qualified as B
import Data.Map qualified as Map
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Data.Either (partitionEithers)
import Data.Foldable (Foldable(foldl'))

data Color = Red | Green | Blue deriving (Show, Eq, Ord)

type GameId   = Int
type Input    = [(GameId, [Map.Map Color Int])]
type Solution = Int

parserLine :: Atto.Parser (GameId, [Map.Map Color Int])
parserLine = (,) <$> parseGameId <*> parseMaps
  where 
    parseGameId = Atto.string "Game " *> Atto.decimal <* Atto.string ": "
    parseMaps   = parseMap `Atto.sepBy` Atto.string "; "
    parseMap    = Map.fromList <$> (parseKeyVal `Atto.sepBy` Atto.string ", ")
    parseKeyVal = do 
      amount <- Atto.decimal <* Atto.space
      col    <- parseColour
      pure (col, amount)
    parseColour = Atto.string "red" $> Red
              <|> Atto.string "green" $> Green 
              <|> Atto.string "blue" $> Blue

-- | parser 
parser :: B.ByteString -> Input
parser input =
    case f input of 
      ([], x) -> x            -- normal case
      (err:_, _) -> error err -- If I fail parsing, throw runtime error. You should never do this...
  where f = partitionEithers . fmap (Atto.parseOnly parserLine) . B.lines

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = sum . fmap fst . filter (all condition . snd)
  where condition m = Map.findWithDefault 0 Red m   <= 12
                   && Map.findWithDefault 0 Green m <= 13
                   && Map.findWithDefault 0 Blue m  <= 14

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = sum . fmap (power . foldl' folding neutral . snd)
  where
    folding = Map.unionWith max
    neutral = Map.fromList [(Red, 0), (Green, 0), (Blue, 0)] 
    power = product @[] @Int . fmap snd . Map.toList

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- parser <$> B.readFile filepath
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input


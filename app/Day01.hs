module Day01 (parser, partOne, partTwo) where

import Text.Megaparsec (Parsec, sepEndBy)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void (Void)
import Data.List (sort)
import Data.Function ((&))

type Parser = Parsec Void String

parseRow :: Parser (Int, Int)
parseRow = do
    first <- decimal
    _ <- space 
    second <- decimal
    return (first, second)

parser :: Parser ([Int], [Int])
parser = do
    lists <- sepEndBy parseRow (char '\n')
    return (unzip lists)

computeTotalDistance :: ([Int], [Int]) -> Int -- part 01
computeTotalDistance (firstList, secondList) = zipWith distance (sort firstList) (sort secondList) & sum
    where distance a b = abs (b - a)

count :: Eq a => [a] -> a -> Int -- part 02
count list target = length (filter ((==) target) list)

computeTotalSimilarityScore :: ([Int], [Int]) -> Int -- part 02
computeTotalSimilarityScore (firstList, secondList) =
    let occurences = map (count secondList) firstList in
    let combinedLists = zip firstList occurences in
    let similarities = map (uncurry (*)) combinedLists in
    sum similarities

partOne :: ([Int], [Int]) -> Int
partOne lists = computeTotalDistance lists

partTwo :: ([Int], [Int]) -> Int
partTwo lists = computeTotalSimilarityScore lists
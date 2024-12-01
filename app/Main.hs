module Main where

import Text.Megaparsec (Parsec, sepEndBy, runParser)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void (Void)
import Data.List (sort)

type Parser = Parsec Void String

parseRow :: Parser (Int, Int)
parseRow = do
    first <- decimal
    _ <- space 
    second <- decimal
    return (first, second)

parseInput :: Parser ([Int], [Int])
parseInput = do
    lists <- sepEndBy parseRow (char '\n')
    return (unzip lists)

computeTotalDistance :: ([Int], [Int]) -> Int -- part 01
computeTotalDistance (firstList, secondList) =
    let (firstSortedList, secondSortedList) = (sort firstList, sort secondList) in
    let combinedSortedLists = zip firstSortedList secondSortedList in
    let distances = map (\(a, b) -> abs (b - a)) combinedSortedLists in
    sum distances

count :: Eq a => [a] -> a -> Int -- part 02
count [] _ = 0
count (x:xs) target = if x == target then 1 + remainingCount else remainingCount where remainingCount = count xs target 

computeTotalSimilarityScore :: ([Int], [Int]) -> Int -- part 02
computeTotalSimilarityScore (firstList, secondList) =
    let occurences = map (count secondList) firstList in
    let combinedLists = zip firstList occurences in
    let similarities = map (uncurry (*)) combinedLists in
    sum similarities

main :: IO ()
main = do
    input <- getContents
    let parseResult = runParser parseInput "<stdin>" input
    case parseResult of
        Left _ -> putStrLn "parse error"
        Right lists -> print (computeTotalSimilarityScore lists)
module Day05 where

import Text.Megaparsec (Parsec, sepBy, some)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Function ((&))
import Data.List (nub)
import Data.Void (Void)

type Parser = Parsec Void String

parseRule :: Parser (Int, Int)
parseRule = do
    lhs <- decimal
    _ <- char '|'
    rhs <- decimal
    _ <- newline
    return (lhs, rhs)

parseUpdate :: Parser [Int]
parseUpdate = sepBy decimal (char ',') <* newline

parser :: Parser ([(Int, Int)], [[Int]])
parser = do
    rules <- some parseRule
    _ <- newline
    updates <- some parseUpdate
    return (rules, updates)

makePairs :: Eq a => [a] -> [(a, a)]
makePairs [] = []
makePairs (x:xs) = map (x,) xs ++ makePairs xs

updateRespectsRules :: [Int] -> [(Int, Int)] -> Bool
updateRespectsRules update rules = all pairRespectsRules (makePairs update)
    where pairRespectsRules pair = all (pairRespectsRule pair) rules
          pairRespectsRule (pair1, pair2) rule = (pair2, pair1) /= rule

middle :: [a] -> a
middle list = list !! middleIndex where middleIndex = ((length list) - 1) `div` 2

partOne :: ([(Int, Int)], [[Int]]) -> Int
partOne (rules, updates) =
    filter (`updateRespectsRules` rules) updates
    & map middle
    & sum
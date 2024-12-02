module Day02 where

import Prelude hiding (Ordering)
import Text.Megaparsec (Parsec, runParser, sepBy, many)
import Text.Megaparsec.Char (hspace, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void (Void)

type Parser = Parsec Void String

parseRow :: Parser [Int]
parseRow = do
    digits <- sepBy decimal hspace
    _ <- newline
    return digits

parseInput :: Parser [[Int]]
parseInput = many parseRow

data Ordering = Increasing | Decreasing deriving (Eq)

isReportSafe' :: Ordering -> [Int] -> Bool
isReportSafe' _ [] = True
isReportSafe' _ [_] = True
isReportSafe' ordering (x:x':xs) =
    correct && isReportSafe' ordering (x':xs) 
    where correct = diff >= 1 && diff <= 3 
          diff = if ordering == Increasing then x' - x else x - x'

isReportSafe :: [Int] -> Bool
isReportSafe report = isReportSafe' Increasing report || isReportSafe' Decreasing report

partOne :: String -> IO ()
partOne input = do
    let parsedInput = runParser parseInput "<stdin>" input
    case parsedInput of
        Left _ -> print "parse error"
        Right reports -> print (length (filter isReportSafe reports))
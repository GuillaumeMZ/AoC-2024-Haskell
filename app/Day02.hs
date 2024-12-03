module Day02 (parser, partOne, partTwo, deleteIndex) where

import Prelude hiding (Ordering)
import Text.Megaparsec (Parsec, sepBy1, sepEndBy)
import Text.Megaparsec.Char (hspace, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.List (findIndex)
import Data.Function (on, (&))
import Data.Void (Void)

type Parser = Parsec Void String

parser :: Parser [[Int]]
parser = sepEndBy row newline
    where row = sepBy1 decimal hspace -- without 1, empty line is recognized as []

data Ordering = Increasing | Decreasing deriving (Eq)

distance :: Num a => Ordering -> (a -> a -> a)
distance Increasing = flip (-)
distance Decreasing = (-)

computeDistances :: Ordering -> [Int] -> [Int]
computeDistances ordering report = zipWith (distance ordering) (init report) (tail report) 

inRange :: Int -> Bool
inRange delta = delta >= 1 && delta <= 3

isReportSafe' :: [Int] -> Ordering -> Bool
isReportSafe' report ordering = all inRange (computeDistances ordering report)

isReportSafe :: [Int] -> Bool
isReportSafe report = ((||) `on` isReportSafe' report) Increasing Decreasing

deleteIndex :: Int -> [a] -> [a]
deleteIndex index list = (take index list) ++ (drop (index + 1) list)

isReportSafe2' :: [Int] -> Ordering -> Bool
isReportSafe2' report ordering = 
    case potentialDiscrepency of
        Just index -> isReportSafe' (deleteIndex index report) ordering || isReportSafe' (deleteIndex (index + 1) report) ordering
        Nothing -> True
    where potentialDiscrepency = computeDistances ordering report & findIndex (not . inRange)

isReportSafe2 :: [Int] -> Bool
isReportSafe2 report = ((||) `on` isReportSafe2' report) Increasing Decreasing

partOne :: [[Int]] -> Int
partOne reports = length (filter isReportSafe reports)

partTwo :: [[Int]] -> Int
partTwo reports = length (filter isReportSafe2 reports)
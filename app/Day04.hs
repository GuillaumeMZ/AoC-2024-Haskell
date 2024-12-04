module Day04 where

import Text.Megaparsec (Parsec, choice, many)
import Text.Megaparsec.Char (char, newline)
import Data.Function ((&))
import Data.Void (Void)

type Parser = Parsec Void String

data Letter = X | M | A | S deriving (Eq, Show)

parseLetter :: Parser Letter
parseLetter = choice [
    X <$ char 'X',
    M <$ char 'M',
    A <$ char 'A',
    S <$ char 'S']

parseRow :: Parser [Letter]
parseRow = many parseLetter <* newline

parser :: Parser [[Letter]]
parser = many parseRow

data Direction = NW | North | NE | W | E | SW | South | SE deriving (Eq)

directions :: [Direction]
directions = [NW, North, NE, W, E, SW, South, SE]

applyDirectionCalculation :: Direction -> (Int, Int) -> Int -> (Int, Int)
applyDirectionCalculation direction (x, y) distance = case direction of
    NW -> (x - distance, y - distance)
    North -> (x - distance, y)
    NE -> (x - distance, y + distance)
    W -> (x, y - distance)
    E -> (x, y + distance)
    SW -> (x + distance, y - distance)
    South -> (x + distance, y)
    SE -> (x + distance, y + distance)

width :: [[a]] -> Int
width grid = length (head grid)

height :: [[a]] -> Int
height grid = length grid

letterAt :: [[Letter]] -> (Int, Int) -> Maybe Letter
letterAt grid (x, y) = if outOfBounds then Nothing else Just $ grid !! x !! y
    where outOfBounds = x < 0 || x >= height grid || y < 0 || y >= width grid

isXmas :: [[Letter]] -> Int -> Int -> Direction -> Bool
isXmas grid x y direction =
    map (applyDirectionCalculation direction (x, y)) [0, 1, 2, 3]
    & map (letterAt grid)
    & (==) [Just X, Just M, Just A, Just S]

countXmases :: [[Letter]] -> Int -> Int -> Int
countXmases grid x y =
    if x >= height grid then 0 -- the whole grid was tested; end
    else if y >= width grid then countXmases grid (x + 1) 0 -- the current line was tested; go to the next line
    else (length . filter (== True) $ map (isXmas grid x y) directions) + (countXmases grid x (y + 1))

partOne :: [[Letter]] -> Int
partOne grid = countXmases grid 0 0
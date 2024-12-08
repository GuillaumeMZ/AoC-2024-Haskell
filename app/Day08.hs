module Day08 where

import Text.Megaparsec (some, choice, sepEndBy)
import Text.Megaparsec.Char (alphaNumChar, char, newline)
import Data.List (sortBy, groupBy, nub)
import Data.Function (on)
import Common

data Cell = Empty | Antenna Char deriving Show
type Grid = [[Cell]]
type Antenna = (Char, Coord2D)

parser :: Parser Grid
parser = sepEndBy row newline
    where 
        row :: Parser [Cell]
        row = some $ choice [
                Empty <$ char '.',
                Antenna <$> alphaNumChar]

antennasGroups :: Grid -> [[Antenna]]
antennasGroups grid = listAntennas' (0, 0) |> sortBy (compare `on` fst) |> groupBy ((==) `on` fst)
    where listAntennas' :: Coord2D -> [Antenna]
          listAntennas' (x, y)
            | x >= gridHeight grid = []
            | y >= gridWidth grid = listAntennas' (x + 1, 0)
            | otherwise = case grid !! x !! y of
                Empty -> otherAntennas
                Antenna name -> (name, (x, y)):otherAntennas
                where otherAntennas = listAntennas' (x, y + 1)

antinodes :: (Coord2D, Coord2D) -> (Coord2D, Coord2D)
antinodes ((x1, y1), (x2, y2)) =
    let dx = x1 - x2
        dy = y1 - y2 in
    ((x1 + dx, y1 + dy), (x2 - dx, y2 - dy))

partOne :: Grid -> Int
partOne grid = 
    (antennasGroups grid >>= Common.pairs)
    |> map (\(a1, a2) -> (snd a1, snd a2))
    |> map antinodes
    |> tupleListToList
    |> nub
    |> filter (not . Common.outOfBounds grid)
    |> length
    where tupleListToList :: [(a, a)] -> [a]
          tupleListToList [] = []
          tupleListToList ((a, b):xs) = a:b:tupleListToList xs
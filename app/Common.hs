module Common where

import Text.Megaparsec (Parsec)
import Data.Function ((&))
import Data.Void (Void)

type Parser = Parsec Void String

type Coord2D = (Int, Int)

gridWidth :: [[a]] -> Int
gridWidth grid = length (head grid)

gridHeight :: [[a]] -> Int
gridHeight grid = length grid

outOfBounds :: [[a]] -> Coord2D -> Bool
outOfBounds grid (x, y) = x < 0 || y < 0 || x >= gridHeight grid || y >= gridWidth grid

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map (x,) xs ++ pairs xs

(|>) :: a -> (a -> b) -> b
(|>) = (&)
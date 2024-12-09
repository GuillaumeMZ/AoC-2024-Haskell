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

updateListAt :: [a] -> a -> Int -> [a]
updateListAt list newValue index =
    take index list ++ [newValue] ++ drop (index + 1) list

deleteListAt :: [a] -> Int -> [a]
deleteListAt list index = take index list ++ drop (index + 1) list

indexed :: [a] -> [(Int, a)]
indexed list = zipWith (,) [0..(length list) - 1] list

(|>) :: a -> (a -> b) -> b
(|>) = (&)
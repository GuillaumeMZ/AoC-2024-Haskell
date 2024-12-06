module Day06 where

import Text.Megaparsec (Parsec, some, choice)
import Text.Megaparsec.Char (char, newline)
import Data.Function ((&))
import Data.List (nub)
import Data.Void (Void)

data Cell = Empty | Guard | Obstacle deriving Eq
data GuardDirection = Up | Down | DLeft | DRight deriving Eq
type Grid = [[Cell]]
type Coord = (Int, Int)
type Parser = Parsec Void String

parser :: Parser Grid
parser = some row
    where row = (some cell) <* newline
          cell = choice [
            Empty <$ char '.',
            Guard <$ char '^',
            Obstacle <$ char '#']

width :: Grid -> Int
width grid = length (head grid)

height :: Grid -> Int
height grid = length grid

outOfBounds :: Grid -> Coord -> Bool
outOfBounds grid (x, y) = x >= height grid || y >= width grid || x < 0 || y < 0

guardPosition :: Grid -> Coord
guardPosition grid = guardPosition' (0, 0)
    where guardPosition' (x, y)
            | x >= height grid = (-1, -1) 
            | y >= width grid = guardPosition' (x + 1, 0) 
            | grid !! x !! y == Guard = (x, y) 
            | otherwise = guardPosition' (x, y + 1)

inFrontOfGuard :: Grid -> Coord -> GuardDirection -> (Maybe Cell, Coord)
inFrontOfGuard grid (x, y) direction
    | outOfBounds grid newCoord = (Nothing, (-1, -1))
    | otherwise = (Just $ grid !! newX !! newY, newCoord) 
    where newCoord@(newX, newY) = case direction of
            Up -> (x - 1, y)
            Down -> (x + 1, y)
            DLeft -> (x, y - 1)
            DRight -> (x, y + 1)

nextDirection :: GuardDirection -> GuardDirection
nextDirection Up = DRight
nextDirection DRight = Down
nextDirection Down = DLeft
nextDirection DLeft = Up
        
nextGuardPosition :: Grid -> Coord -> GuardDirection -> Maybe (Coord, GuardDirection)
nextGuardPosition grid coord direction = 
    case nextCell of
        Nothing -> Nothing
        Just cell -> if cell == Obstacle then (nextGuardPosition grid coord (nextDirection direction)) else Just (newCoord, direction)
    where (nextCell, newCoord) = inFrontOfGuard grid coord direction

walkGuard :: Grid -> Coord -> GuardDirection -> [Coord]
walkGuard grid coord direction = 
    case nextGuardPosition grid coord direction of
        Nothing -> []
        Just (newCoord, newDirection) -> newCoord:walkGuard grid newCoord newDirection

partOne :: Grid -> Int
partOne grid = let initialPosition = guardPosition grid in
    initialPosition:walkGuard grid initialPosition Up -- up is the initial guard position
    & nub
    & length
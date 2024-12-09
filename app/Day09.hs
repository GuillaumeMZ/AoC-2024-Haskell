module Day09 where

import Text.Megaparsec (some)
import Text.Megaparsec.Char (digitChar)

import Common
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (findIndex, groupBy)
import Data.Maybe (isJust, isNothing, fromJust)

-- File Id  | EmptySpace
type DiskItem = Maybe Int

parser :: Parser [DiskItem]
parser = (toDiskItems True 0) <$> digits
    where digits :: Parser [Int]
          digits = some (digitToInt <$> digitChar)

          toDiskItems :: Bool -> Int -> [Int] -> [DiskItem]
          toDiskItems _ _ [] = []
          toDiskItems isFile nextFileId (x:xs)
            | isFile = (take x $ repeat (Just nextFileId)) ++ (toDiskItems False (nextFileId + 1) xs)
            | otherwise = (take x $ repeat Nothing) ++ (toDiskItems True nextFileId xs)

compact :: [DiskItem] -> [DiskItem]
compact filesystem
  | noGapPresent = groupBy ((&&) `on` isJust) filesystem !! 0
  | otherwise = Common.updateListAt newFilesystem lastFile firstGapIndex  |> compact
  where noGapPresent = groupBy ((&&) `on` isJust) filesystem |> length |> (<=2)
        firstGapIndex = findIndex isNothing filesystem |> fromJust
        reversedFilesystem = reverse filesystem
        lastFileIndex = findIndex isJust reversedFilesystem |> fromJust
        lastFile = reversedFilesystem !! lastFileIndex
        newFilesystem = Common.deleteListAt reversedFilesystem lastFileIndex |> reverse

partOne :: [DiskItem] -> Int
partOne filesystem =
  Common.indexed (compact filesystem)
  |> map (fromJust <$>)
  |> map (uncurry (*))
  |> sum
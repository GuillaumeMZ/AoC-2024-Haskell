module Day09 where

import Text.Megaparsec (some)
import Text.Megaparsec.Char (digitChar)

import Common
import Data.Char (digitToInt)
import Data.Maybe (isJust, fromJust)

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

-- compact' constructs the compressed filesystem reversed to save time (because (:) is O(1) whereas (++) is O(n))
-- so we reverse the reversed result in compact  
compact :: [DiskItem] -> [DiskItem]
compact filesystem = compact' filesystem (reverse filesystem) [] |> reverse
  where compact' :: [DiskItem] -> [DiskItem] -> [DiskItem] -> [DiskItem]
        compact' filesystem' reversedFs finalFs
          | isDone = finalFs
          | otherwise = case (filesystem', reversedFs) of
              ((Just x:xs), (reversedFs@(Just _:_))) -> compact' xs reversedFs (Just x:finalFs)
              ((Nothing:xs), (Just y:ys)) -> compact' xs ys (Just y:finalFs)
              ((Just x:xs), (Nothing:ys)) -> compact' xs ys (Just x:finalFs)
              (filesystem@(Nothing:_), (Nothing:ys)) -> compact' filesystem ys finalFs
              _ -> error "unreachable"
          where isDone = length finalFs == length (filter isJust filesystem)

partOne :: [DiskItem] -> Int
partOne filesystem =
  Common.indexed (compact filesystem)
  |> map (fromJust <$>)
  |> map (uncurry (*))
  |> sum
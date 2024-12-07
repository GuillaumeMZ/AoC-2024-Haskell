module Day07 where

import Text.Megaparsec (Parsec, some, sepBy)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Function ((&))
import Data.Void (Void)

type Equation = (Int, [Int])
type Parser = Parsec Void String

parser :: Parser [Equation]
parser = some equation
    where equation :: Parser Equation
          equation = do
            result <- decimal
            _ <- string ": "
            operators <- sepBy decimal (char ' ')
            _ <- newline
            return (result, operators)

operatorsCombinations :: Int -> [[(Int -> Int -> Int)]]
operatorsCombinations 1 = [[(+)], [(*)], [concatOperator]]
operatorsCombinations n = map ((+):) sublist ++ map ((*):) sublist ++ map (concatOperator:) sublist
  where sublist = operatorsCombinations $ n - 1

applyCombination :: [Int] -> [(Int -> Int -> Int)] -> Int
applyCombination [x] [] = x
applyCombination [o,o'] [f] = o `f` o' 
applyCombination (o:o':os) (f:fs) = applyCombination ((o `f` o'):os) fs
applyCombination _ _ = undefined

isSolvable :: Equation -> Bool
isSolvable (expected, operators) = expected `elem` computedSolutions
  where computedSolutions = map (applyCombination operators) (operatorsCombinations $ (length operators) - 1) 

partOne :: [Equation] -> Int
partOne equations = equations & filter isSolvable & map fst & sum

concatOperator :: Int -> Int -> Int
concatOperator lhs rhs = show lhs ++ show rhs & read
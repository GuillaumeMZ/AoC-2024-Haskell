module Day03 where

import Text.Megaparsec (Parsec, anySingle, many, try, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Data.Void (Void)

type Parser = Parsec Void String

anySingle' :: Parser (Maybe (Int, Int))
anySingle' = do
    _ <- anySingle
    return Nothing

parser :: Parser [(Int, Int)]
parser = do
    muls <- many (try mulParser <|> anySingle')
    return $ catMaybes muls

mulParser :: Parser (Maybe (Int, Int))
mulParser = do
    _ <- string "mul("
    lhs <- decimal
    _ <- char ','
    rhs <- decimal
    _ <- char ')'
    return $ Just (lhs, rhs)

partOne :: [(Int, Int)] -> Int
partOne muls = map (uncurry (*)) muls & sum

partTwo = undefined
module Day03 where

import Text.Megaparsec (Parsec, anySingle, many, try, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Function ((&))
import Data.Void (Void)

type Parser = Parsec Void String

data Instruction = Mul Int Int | Do | Dont | Nop deriving (Show, Eq)

isMul :: Instruction -> Bool
isMul (Mul _ _) = True
isMul _ = False

mulProduct :: Instruction -> Int
mulProduct (Mul a b) = a * b
mulProduct _ = undefined 

anySingle' :: Parser Instruction
anySingle' = Nop <$ anySingle

parser :: Parser [Instruction]
parser = many (
    try mulParser  <|> 
    try doParser   <|>
    try dontParser <|>
    anySingle')

mulParser :: Parser Instruction
mulParser = do
    _ <- string "mul("
    lhs <- decimal
    _ <- char ','
    rhs <- decimal
    _ <- char ')'
    return $ Mul lhs rhs

doParser :: Parser Instruction
doParser = Do <$ string "do()"

dontParser :: Parser Instruction
dontParser = Dont <$ string "don't()"

filterEnabledMuls :: [Instruction] -> Bool -> [Instruction]
filterEnabledMuls [] _ = []
filterEnabledMuls (Do:xs) _ = filterEnabledMuls xs True
filterEnabledMuls (Dont:xs) _ = filterEnabledMuls xs False
filterEnabledMuls (x@(Mul _ _):xs) True = x:filterEnabledMuls xs True
filterEnabledMuls (_:xs) isOn = filterEnabledMuls xs isOn

partOne :: [Instruction] -> Int
partOne ops = filter isMul ops & map mulProduct & sum

partTwo :: [Instruction] -> Int
partTwo ops = filterEnabledMuls ops True & partOne
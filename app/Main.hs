module Main where

import Text.Megaparsec (runParser)

import Day04 (parser, partOne)

main :: IO ()
main = do
    input <- getContents
    let parseResult = runParser parser "<stdin>" input
    case parseResult of
        Left _ -> print "parse error"
        Right parsedInput -> print (partOne parsedInput)
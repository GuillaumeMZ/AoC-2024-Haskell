module Main where

import Text.Megaparsec (runParser)

import Day05 (parser, partTwo)

main :: IO ()
main = do
    input <- getContents
    let parseResult = runParser parser "<stdin>" input
    case parseResult of
        Left _ -> print "parse error"
        Right parsedInput -> print (partTwo parsedInput)
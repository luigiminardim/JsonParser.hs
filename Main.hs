module Main where

import qualified Json
import qualified JsonParser

main = do
    json <- readFile "schedule.json"
    Just value <- return (JsonParser.parse json)
    putStrLn(show value)
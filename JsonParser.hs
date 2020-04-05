module JsonParser (
  parse
) where

import Prelude hiding (null)
import qualified Prelude (null)
import Control.Applicative
import Data.Char

import Json
import Parser hiding (string, number, boolean)
import qualified Parser (string, number, boolean)

instance Read Json.Value where
  readsPrec _ str = case run value str of
    Nothing -> []
    Just a -> [a]

parse str = do 
  result <- run value str
  return(fst result)

value = ws *> (null <|> boolean <|> number <|> string <|> array <|> object)

null = Parser.parserOfString "null" *> pure Null

boolean = Boolean <$> Parser.boolean

number = Number <$> Parser.number

string = String <$> Parser.string 

array = parserOfChar '[' *> ws
    *> (Array <$> elements)
    <* ws <* parserOfChar ']'
  where

  elements = sepBy (ws *> sep <* ws) value
    where
    
    sep = parserOfChar ','

object = parserOfChar '{' *> ws
      *> (Object <$> members)
      <* ws <* parserOfChar '}'
  where
  
  members = sepBy (ws *> sep <* ws) member
    where

      sep = parserOfChar ','

      member = (,) <$> (Parser.string <* ws <* parserOfChar ':') <*> (ws *> value)
    
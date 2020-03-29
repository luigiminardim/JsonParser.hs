module JsonParser where

import Prelude hiding (Bool, String, null)
import qualified Prelude (Bool, String, null)
import Control.Applicative
import Data.Char

import Parser hiding (string)
import qualified Parser (string)

data JsonValue
  = Null
  | Bool Prelude.Bool
  | Number Prelude.Int
  | String Prelude.String
  | Array [JsonValue]
  | Object [(Prelude.String, JsonValue)]
  deriving (Show, Eq)

jsonValue = null <|> bool <|> number <|> string <|> array <|> object

null = (\ _ -> Null) <$> parserOfString "null"

bool = convBoolToJsonValue<$> (parserOfString "true" <|> parserOfString "false")
  where

  convBoolToJsonValue str
    | str == "true"   = Bool True 
    | str == "false"  = Bool False

number = (Number . read) <$> Parser(\ str -> do
      (a, str') <- run (parserOfSpan isDigit) str
      if length a == 0
        then Nothing
        else Just(a, str')
    )

string = String <$> Parser.string 

array = parserOfChar '[' *> whiteSpaces
    *> (Array <$> elements)
    <* whiteSpaces <* parserOfChar ']'
  where

  elements = sepBy (whiteSpaces *> sep <* whiteSpaces) object
    where
    
    sep = parserOfChar ','

object = Object <$> (
      parserOfChar '{' *> whiteSpaces
      *> attributes 
      <* whiteSpaces <* parserOfChar '}'
    )
  where
  
  attributes = sepBy (whiteSpaces *> sep <* whiteSpaces) atribute
    where
    
    sep = parserOfChar ','
    atribute = (,) <$> (Parser.string <* whiteSpaces <* parserOfChar ':' <* whiteSpaces) <*> jsonValue
  
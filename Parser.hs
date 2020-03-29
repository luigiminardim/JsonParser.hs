module Parser where

import Control.Applicative
import Data.Char

newtype Parser a = Parser {run :: String -> Maybe (a, String)}

instance Functor Parser where

  fmap f p = Parser (\ str -> do
      (a, str') <- run p str
      Just(f a, str')
    )
  
instance Applicative Parser where

  pure a = Parser(\ str -> Just(a, str))

  pf <*> pa = Parser(\ str -> do
      (f, str') <- run pf str
      (a, str'') <- run pa str'
      Just (f a, str'')
    )

instance Alternative Parser where

  empty = Parser(\ str -> Nothing)

  p1 <|> p2 = Parser(\ str -> run p1 str <|> run p2 str)

string = (parserOfChar '"' *> strLiteral <* parserOfChar '"')
  where
    
  strLiteral = Parser(\ str -> case str of
      '\\':'"':str' -> do
          (a', str'') <- run strLiteral str'
          Just('"':a', str'')
      '"':str' -> Just("", '"':str')
      c:str' -> do
        (a', str'') <- run strLiteral str'
        Just(c:a', str'')
      [] -> Nothing
    )

whiteSpaces = parserOfSpan (`elem` [' ', '\n', '\r', '\t'])

sepBy sep element = (:) <$> element <*> many(sep *> element) <|> pure[]

parserOfChar c = Parser(\ str -> 
  if length str /= 0 && head str == c
    then Just(c, tail str)
    else Nothing
  )

parserOfString = sequenceA . map parserOfChar

parserOfSpan f = Parser(Just . span f)
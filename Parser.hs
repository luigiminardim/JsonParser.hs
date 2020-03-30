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

ws = (:) <$> (foldr (<|>) empty . map parserOfChar) [' ', '\n', '\r', '\t'] <*> ws <|> pure []

character = Parser(\ str -> case str of
    a:str' -> Just(a, str')
    _ -> Nothing
  )

string = (parserOfChar '"' *> characters <* parserOfChar '"')
  where
 
  characters = (:) <$> escapeCode <*> characters <|> endOfString <|> (:) <$> character <*> characters
    where

    -- NOTE: don't support unicode.
    escapeCode = (foldr (<|>) empty . map parserOfScape) ['\"', '\\', '/', '\b', '\f', '\n', '\r', '\t']
      where
      
      parserOfScape esc = esc <$ sequenceA [parserOfChar '\\', parserOfChar esc]

    endOfString = Parser(\ str -> if str!!0 == '\"'
        then Just([], str)
        else Nothing
      )

sign = parserOfString "+" *> pure id
    <|> parserOfString "-" *> pure negate
    <|> pure id

digit = parserOfChar '0' <|> onenine <|> empty

onenine = (foldr (<|>) empty . map parserOfChar) ['1' .. '9']

digits = (:) <$> digit <*> digits <|> (: []) <$> digit

integer = read <$> (
    (++) <$> sequenceA [parserOfChar '-', onenine] <*> digits
    <|> sequenceA [parserOfChar '-', digit]
    <|> (:) <$> onenine <*> digits
    <|> (: []) <$> digit
  )

number = (\ i f e -> (fromIntegral i + f) * e) <$> integer <*> fraction <*> expoent
  where

  fraction = read . ("0." ++) <$> (parserOfChar '.' *> digits) 
      <|> pure 0.0

  expoent = (10 **) <$> (
      (parserOfChar 'e' <|> parserOfChar 'E') *> sign <*> (read <$> digits)
      <|> pure 0.0)

parserOfChar c = Parser(\ str -> 
  if length str /= 0 && head str == c
    then Just(c, tail str)
    else Nothing
  )

parserOfString = sequenceA . map parserOfChar

parserOfSpan f = Parser(Just . span f)

sepBy sep element = (:) <$> element <*> many(sep *> element) <|> pure []
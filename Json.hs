module Json where

import Prelude hiding (String); import qualified Prelude (String)

data Value
  = Null
  | Boolean Bool
  | Number Double
  | String Prelude.String
  | Array [Value]
  | Object [(Prelude.String, Value)]
  deriving (Show, Eq)
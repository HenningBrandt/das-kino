module Overture.Alternative (
  optional,
) where

import Control.Applicative (Alternative (..))

optional :: Alternative m => m a -> m (Maybe a)
optional action = Just <$> action <|> pure Nothing
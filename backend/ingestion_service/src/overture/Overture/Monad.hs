module Overture.Monad (
  mapMaybeM,
  forMaybeM,
) where

import Data.Maybe (fromJust, isJust)

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f t = fmap fromJust . filter isJust <$> mapM f t

forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM = flip mapMaybeM
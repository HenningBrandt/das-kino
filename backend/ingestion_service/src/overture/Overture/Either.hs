module Overture.Either (
  rightToMaybe,
) where

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = \case
  Left _ -> Nothing
  Right x -> Just x
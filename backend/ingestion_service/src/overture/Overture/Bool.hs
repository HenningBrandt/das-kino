module Overture.Bool (
  if',
) where

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ x = x
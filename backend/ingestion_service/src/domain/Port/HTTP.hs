{-# LANGUAGE TemplateHaskell #-}

module Port.HTTP (
  HTTP (..),
  getURI,
) where

import Data.ByteString (ByteString)
import Polysemy (makeSem)
import Text.URI (URI)

data HTTP m a where
  GetURI :: URI -> HTTP m (Either () ByteString)
makeSem ''HTTP
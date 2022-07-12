module Adapter.HTTP (
  adaptHTTP,
) where

import Polysemy (Sem, Embed, Member, embed, interpret)
import qualified Port.HTTP as HTTP
import Port.HTTP (HTTP)
import qualified Network.HTTP.Req as Req
import Network.HTTP.Req (GET(..), NoReqBody(..))

-- TODO: Return better error type
adaptHTTP :: Member (Embed IO) r => Sem (HTTP : r) a -> Sem r a
adaptHTTP = interpret $ \case
  HTTP.GetURI uri -> embed $ do
    case Req.useHttpsURI uri of
      Nothing -> return $ Left ()
      Just (url, _) -> Req.runReq @IO Req.defaultHttpConfig $ do
        response <- Req.req GET url NoReqBody Req.bsResponse mempty
        case Req.responseStatusCode response of
          200 -> return . Right . Req.responseBody $ response
          _ -> return . Left $ ()
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Forma.Server
  ( module Servant.Forma
  , module Web.Forma
  ) where

import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Data.Aeson                 (Value (..), encode, object, (.=))
import           Data.ByteString.Lazy.Char8 (ByteString, fromStrict, pack)
import qualified Data.Map.Strict            as M
import           Network.HTTP.Types         (hContentType)
import           Network.Wai                (Request (requestHeaders),
                                             lazyRequestBody)
import           Servant                    ((:>), HasContextEntry (..),
                                             HasServer (..), JSON, Proxy (..),
                                             ServerError (errBody), ServerT,
                                             err400, err415)
import           Servant.API.ContentTypes   (canHandleCTypeH)
import           Servant.Forma
import           Servant.Server.Internal    (DelayedIO, addBodyCheck,
                                             delayedFail, delayedFailFatal,
                                             withRequest)
import           Web.Forma

instance ( Form form
         , HasServer api ctx
         , HasContextEntry ctx (Context form)
         ) => HasServer (Forma form :> api) ctx where

  type ServerT (Forma form :> api) m = form -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver = route (Proxy :: Proxy api) context $ addBodyCheck subserver checkContentType checkBody

    where

      checkContentType :: DelayedIO (ByteString -> Either String Value)
      checkContentType = withRequest $ \request -> do
        case canHandleCTypeH @'[JSON] @Value Proxy $ maybe "application/json" fromStrict $ lookup hContentType $ requestHeaders request of
          Nothing -> delayedFail err415
          Just f  -> pure f

      checkBody :: (ByteString -> Either String Value) -> DelayedIO form
      checkBody f = withRequest $ \request -> do
        f <$> liftIO (lazyRequestBody request) >>= \case
          Left e -> delayedFailFatal err400 { errBody = pack e }
          Right val -> do
            liftIO (runForm (rules @form (getContextEntry context)) val) >>= \result -> case result of
              Succeeded form -> pure form
              ParsingFailed Nothing msg -> delayedFailFatal err400 { errBody = encode $ object ["parse_error" .= msg] }
              ParsingFailed (Just path) msg -> delayedFailFatal err400 { errBody = encode $ object ["parse_error" .= object [ "field" .= path, "message" .= msg ]] }
              ValidationFailed errors ->
                let g (fieldName, err) = showFieldName fieldName .= err
                in delayedFailFatal err400 { errBody = encode $ object ["field_errors" .= (object . fmap g . M.toAscList) errors] }


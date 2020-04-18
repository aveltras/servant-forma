{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Servant.Forma where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Proxy             (Proxy (Proxy))
import           Data.Text              (Text)
import           GHC.TypeLits           (Symbol)
import           Servant.API            ((:>), HasLink (toLink), MkLink)
import           Web.Forma              (FormParser)

data Forma form

class Form f where
  type Fields f :: [Symbol]
  type Context f :: *
  type Context f = ()
  rules :: MonadIO m => Context f -> FormParser (Fields f) Text m f

instance HasLink sub => HasLink (Forma form :> sub) where
  type MkLink (Forma form :> sub) a = MkLink sub a
  toLink toA _ = toLink toA (Proxy :: Proxy sub)

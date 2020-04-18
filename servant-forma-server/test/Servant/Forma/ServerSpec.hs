{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Servant.Forma.ServerSpec (spec) where

import           Control.Lens             ((^.))
import           Control.Monad.Except     (ExceptT, throwError)
import           Data.Aeson               (Value, object, toJSON, (.=))
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Network.HTTP.Client      as HCli
import           Network.HTTP.Types       (Status, status400)
import           Network.Wai.Handler.Warp (testWithApplication)
import           Network.Wreq             (post, responseBody)
import           Servant
import           Servant.Forma.Server
import           Test.Hspec

spec :: Spec
spec = do
  describe "test" $
    around (testWithApplication . pure $ app) $ do
      it "returns a 400 error if field is missing" $ \port -> do
        post (url port) missingField `shouldHTTPErrorWith` status400
      it "returns a 400 error if field validation is not satisfied" $ \port -> do
        post (url port) emptyRequiredField `shouldHTTPErrorWith` status400
      it "returns expected result if form is valid" $ \port -> do
        response <- post (url port) validPayload
        response ^. responseBody `shouldBe` "success"

shouldHTTPErrorWith :: IO a -> Status -> Expectation
shouldHTTPErrorWith act stat = act `shouldThrow` \e -> case e of
  HCli.HttpExceptionRequest _ (HCli.StatusCodeException resp _)
    -> HCli.responseStatus resp == stat
  _ -> False

missingField :: Value
missingField = object [ "username" .= toJSON @Text "myUsername"
                      , "password" .= toJSON @Text "myPassword"
                      ]

emptyRequiredField :: Value
emptyRequiredField = object [ "username" .= toJSON @Text "myUsername"
                            , "password" .= toJSON @Text ""
                            , "remember_me" .= toJSON True
                            ]

validPayload :: Value
validPayload = object [ "username" .= toJSON @Text "myUsername"
                      , "password" .= toJSON @Text "myPassword"
                      , "remember_me" .= toJSON True
                      ]

data LoginForm = LoginForm
  { _loginUsername   :: Text
  , _loginPassword   :: Text
  , _loginRememberMe :: Bool
  }

instance Form LoginForm where
  type Fields LoginForm = '["username", "password", "remember_me"]
  rules _ = LoginForm
    <$> field #username notEmpty
    <*> field #password notEmpty
    <*> field' #remember_me

notEmpty :: Monad m => Text -> ExceptT Text m Text
notEmpty txt =
  if T.null txt
    then throwError "This field cannot be empty."
    else return txt

type API = Forma LoginForm :> Post '[PlainText] Text

app :: Application
app = serveWithContext (Proxy @API) (() :. EmptyContext) server

server :: Server API
server = handler
  where
    handler :: LoginForm -> Handler Text
    handler _ = pure "success"

url :: Int -> String
url port = "http://localhost:" <> show port

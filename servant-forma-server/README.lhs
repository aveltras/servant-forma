# servant-forma

This package provides a servant combinator to validate your [Forma](https://hackage.haskell.org/package/forma) based forms before they reach your handlers.

~~~ haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.Except (ExceptT, throwError)
import Data.Text (Text)
import qualified Data.Text as T
import Servant
import Servant.Forma.Server
~~~

First define your form datatype

~~~ haskell
data LoginForm = LoginForm
  { _loginUsername   :: Text
  , _loginPassword   :: Text
  , _loginRememberMe :: Bool
  }
~~~

Define your custom validators

~~~ haskell
notEmpty :: Monad m => Text -> ExceptT Text m Text
notEmpty txt =
  if T.null txt
    then throwError "This field cannot be empty."
    else return txt
~~~

Then make your form an instance of the Form typeclass.
You can define a custom context then your form will need form validation (by default ()).

~~~ haskell
data MyFormContext = MyFormContext

instance Form LoginForm where
  type Fields LoginForm = '["username", "password", "remember_me"]
  type Context LoginForm = MyFormContext
  rules MyFormContext = LoginForm
    <$> field #username notEmpty
    <*> field #password notEmpty
    <*> field' #remember_me
~~~

Then simply use the provided "Forma" combinator in your API providing the required context.

~~~ haskell
type API = Forma LoginForm :> Post '[PlainText] Text

app :: Application
app = serveWithContext (Proxy @API) (MyFormContext :. EmptyContext) server

server :: Server API
server = handler
  where
    handler :: LoginForm -> Handler Text
    handler _loginForm = pure "success"
~~~

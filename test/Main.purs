module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Node.Express.Passport (DeserializeUser, DeserializedUser(..), Passport, SerializeUser, SerializedUser(..), addDeserializeUser, addSerializeUser, getPassport)
import Node.Express.Passport.Strategy (setStrategy)
import Node.Express.Passport.Strategy.Local (CredentialsVerified, passportStrategyLocal')
import Node.Express.Types (Request)


main :: Effect Unit
main = do
  log "You should add some tests."


type UserString = String

passportSerializeString :: SerializeUser UserString
passportSerializeString req user =
  pure $ SerializedUser $ Just $ encodeJson user


passportDeserializeString :: DeserializeUser UserString
passportDeserializeString req obj = pure $ either onError onSuccess $ decodeJson obj
  where
  onError = const DeserializePass
  onSuccess = DeserializedUser <<< Just

type UserNumber = Number

passportSerializeNumber :: SerializeUser UserNumber
passportSerializeNumber req user =
  pure $ SerializedUser $ Just $ encodeJson user

verify  :: forall info.
        Request
        -> String -> String
        -> CredentialsVerified UserString info
        -> Effect Unit
verify req username password verified = do
  void $ verified Nothing (Just username) Nothing


initPassport  :: Effect Passport
initPassport = do
  passport <- getPassport
  setStrategy passport "local" $ passportStrategyLocal' $ verify
  addDeserializeUser passport passportDeserializeString
  addSerializeUser passport passportSerializeString
  -- This line should cause type error when uncommented
  -- addSerializeUser passport passportSerializeNumber
  pure passport

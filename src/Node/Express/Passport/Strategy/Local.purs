module Node.Express.Passport.Strategy.Local
  ( PassportStrategyLocalOptions
  , passportStrategyLocalOptions

  , CredentialsVerified
  , PassportVerify

  , Username
  , Password

  , passportStrategyLocal
  , passportStrategyLocal'
  )
  where

import Prelude
import Effect (Effect)
import Effect.Exception (Error)
import Data.Function.Uncurried (Fn3, Fn4, runFn3, mkFn4)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Node.Express.Passport.Strategy.Common (PassportStrategy)
import Node.Express.Types (Request)


type PassportStrategyLocalOptions =
  { usernameField :: String
  , passwordField :: String
  }

passportStrategyLocalOptions :: PassportStrategyLocalOptions
passportStrategyLocalOptions =
  { usernameField: "username", passwordField: "password" }


type Username = String
type Password = String


type CredentialsVerifiedImpl user info =
  Fn3 (Nullable Error) (Nullable user) (Nullable info) (Effect Unit)
type PassportVerifyImpl user info =
  Fn4 Request Username Password (CredentialsVerifiedImpl user info) (Effect Unit)

type CredentialsVerified user info =
  Maybe Error -> Maybe user -> Maybe info -> Effect Unit
type PassportVerify user info =
  Request -> Username -> Password -> CredentialsVerified user info
  -> Effect Unit

foreign import _passportStrategyLocal :: forall user info. PassportStrategyLocalOptions
                                      -> PassportVerifyImpl user info
                                      -> PassportStrategy

passportStrategyLocal :: forall user info.
                      PassportStrategyLocalOptions
                      -> PassportVerify user info
                      -> PassportStrategy
passportStrategyLocal options verify =
  let
    curryVerified verified error user info =
      runFn3 verified (toNullable error) (toNullable user) (toNullable info)
    verify' req username password verified =
      verify req username password (curryVerified verified)
    verify'' = mkFn4 verify'
  in
  _passportStrategyLocal options verify''

passportStrategyLocal'  :: forall user info.
                        PassportVerify user info
                        -> PassportStrategy
passportStrategyLocal' = passportStrategyLocal passportStrategyLocalOptions

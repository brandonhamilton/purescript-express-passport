module Node.Express.Passport
  ( module Node.Express.Passport.Common

  , PassportInitializeMiddleware
  , PassportInitializeOptions

  , PassportSessionMiddleware
  , PassportSessionOptions

  , SerializeUser
  , SerializedUser(..)

  , DeserializeUser
  , DeserializedUser(..)

  , getPassport
  , passportInitializeOptions
  , passportInitialize
  , passportInitialize'

  , passportSessionOptions
  , passportSession
  , passportSession'

  , addSerializeUser
  , addDeserializeUser

  , authenticate
  , authenticateOptions
  , AuthenticationMessage(..)
  , OnAuthenticate

  , isAuthenticated
  , logOut
  , logIn
  , loginOptions
  , OnLogin
  , getUser
  )
  where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Data.Argonaut (Json)
import Data.Either (either)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, mkFn3, mkFn4, runFn1, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Foreign (Foreign, unsafeToForeign)
import Node.Express.Handler (HandlerM(..), Handler, runHandlerM)
import Node.Express.Passport.Common (Passport)
import Node.Express.Types (Request, Response)
import Unsafe.Coerce (unsafeCoerce)


foreign import _getPassport :: Effect Passport

-- | Initialize and obtain a Passport singleton instance
getPassport :: Effect Passport
getPassport = _getPassport


type PassportInitializeMiddleware = Fn3 Request Response (Effect Unit) (Effect Unit)
-- | Type of options for `passport.initialize(options);` call
type PassportInitializeOptions = { userProperty :: String }

-- | Default options for `passport.initialize(options);`
-- | By default user object would be stored in the `req.user` property
passportInitializeOptions :: PassportInitializeOptions
passportInitializeOptions = { userProperty: "user" }

foreign import _passportInitialize  :: Fn2 Passport PassportInitializeOptions
                                        (Effect PassportInitializeMiddleware)

-- | Binding for `passport.initialize(options);` call
passportInitialize  ::
                    Passport
                    -> PassportInitializeOptions
                    -> Effect PassportInitializeMiddleware
passportInitialize = runFn2 _passportInitialize

-- | Binding for `passport.initialize(options);` with default options
passportInitialize' ::
                    Passport
                    -> Effect PassportInitializeMiddleware
passportInitialize' passport = passportInitialize passport passportInitializeOptions


type PassportSessionMiddleware = Fn3 Request Response (Effect Unit) (Effect Unit)
-- | Type of options for `passport.session(options);` call
type PassportSessionOptions = { pauseStream :: Boolean }

foreign import _passportSession :: Fn2
                                      Passport
                                      PassportSessionOptions
                                      (Effect PassportSessionMiddleware)

-- | Default options for `passport.session(options);` call
passportSessionOptions :: PassportSessionOptions
passportSessionOptions = { pauseStream: false }

-- | Binding for `passport.session(options);` call
passportSession  :: Passport
                    -> PassportSessionOptions
                    -> Effect PassportSessionMiddleware
passportSession = runFn2 _passportSession

-- | Binding for `passport.session(options); with default options
passportSession' :: Passport
                    -> Effect PassportSessionMiddleware
passportSession' passport = passportSession passport passportSessionOptions


type UserSerializedImpl = Fn2 (Nullable Error) (Nullable Json) (Effect Unit)
type SerializeUserImpl user = Fn3 Request user UserSerializedImpl (Effect Unit)

foreign import _addSerializeUser  :: forall user. Fn2
                                      Passport
                                      (SerializeUserImpl user)
                                      (Effect Unit)

type SerializeUser user = Request -> user -> Aff SerializedUser

data SerializedUser
  = SerializedUser (Maybe Json)
  | SerializePass

addSerializeUser  :: forall user.
                  Passport
                  -> SerializeUser user
                  -> Effect Unit
addSerializeUser passport serialize = do
  let
    curryOnSerialized onSerialized error result =
      runFn2 onSerialized (toNullable error) (toNullable result)
    serialize' req user onSerialized =
      void $ runAff_ (either onError onSuccess) $ serialize req user
      where
      onError :: Error -> Effect Unit
      onError error = (curryOnSerialized onSerialized) (Just error) Nothing
      onSuccess :: SerializedUser -> Effect Unit
      onSuccess (SerializedUser result) =
        (curryOnSerialized onSerialized) Nothing result
      onSuccess SerializePass =
        (curryOnSerialized onSerialized) (Just $ unsafeCoerce "pass") Nothing
    serialize'' = mkFn3 serialize'
  runFn2 _addSerializeUser passport serialize''


type UserDeserializedImpl user =
  Fn2 (Nullable Error) (Nullable user) (Effect Unit)
type DeserializeUserImpl user =
  Fn3 Request Json (UserDeserializedImpl user) (Effect Unit)

foreign import _addDeserializeUser  :: forall user.
                                    Fn2
                                      Passport
                                      (DeserializeUserImpl user)
                                      (Effect Unit)

type DeserializeUser user =
  Request -> Json -> Aff (DeserializedUser user)

data DeserializedUser user
  = DeserializedUser (Maybe user)
  | DeserializePass

addDeserializeUser  :: forall user.
                    Passport
                    -> DeserializeUser user
                    -> Effect Unit
addDeserializeUser passport deserialize = do
  let
    onDeserialized' onDeserialized error user =
      runFn2 onDeserialized (toNullable error) (toNullable user)
    deserialize' req serialized onDeserialized =
      void $ runAff_ (either onError onSuccess) $ deserialize req serialized
      where
      onError :: Error -> Effect Unit
      onError error =
        (onDeserialized' onDeserialized) (Just error) Nothing
      onSuccess :: DeserializedUser user -> Effect Unit
      onSuccess (DeserializedUser user) =
        (onDeserialized' onDeserialized) Nothing user
      onSuccess DeserializePass =
        (onDeserialized' onDeserialized) (Just $ unsafeCoerce "pass") Nothing
    deserialize'' = mkFn3 deserialize'
  runFn2 _addDeserializeUser passport deserialize''


foreign import _authenticate  :: forall user info.
                              Fn4
                                Passport
                                String
                                AuthenticateOptionsImpl
                                (Nullable (OnAuthenticateImpl user info))
                                (Fn3 Request Response (Effect Unit) (Effect Unit))

type OnAuthenticateImpl user info =
  Fn4 (Nullable Error) (Nullable user) (Nullable info) (Nullable Number) (Effect Unit)

type AuthenticateOptionsImpl =
  { session         :: Boolean
  , successRedirect :: Nullable String
  , successMessage  :: Foreign
  , successFlash    :: Foreign
  , failureRedirect :: Nullable String
  , failureMessage  :: Foreign
  , failureFlash    :: Foreign
  , assignProperty  :: Nullable String
  }

type OnAuthenticate user info =
  Maybe Error -> Maybe user -> Maybe info -> Maybe Number -> Handler

data AuthenticationMessage
  = AuthenticationMessage String
  | StrategyAuthenticationMessage
  | NoAuthenticationMessage

type AuthenticateOptions =
  { session         :: Boolean
  , successRedirect :: Maybe String
  , successMessage  :: AuthenticationMessage
  , successFlash    :: AuthenticationMessage
  , failureRedirect :: Maybe String
  , failureMessage  :: AuthenticationMessage
  , failureFlash    :: AuthenticationMessage
  , assignProperty  :: Maybe String
  }

authenticateOptions :: AuthenticateOptions
authenticateOptions =
  { session:          true
  , successRedirect:  Nothing
  , successMessage:   NoAuthenticationMessage
  , successFlash:     NoAuthenticationMessage
  , failureRedirect:  Nothing
  , failureMessage:   NoAuthenticationMessage
  , failureFlash:     NoAuthenticationMessage
  , assignProperty:   Nothing
  }

authenticate  :: forall info user.
              Passport
              -> String
              -> AuthenticateOptions
              -> Maybe (OnAuthenticate user info)
              -> Handler
authenticate passport strategy options onAuthenticate = HandlerM \req res nxt -> do
  let
    foreignMessage (AuthenticationMessage msg) = unsafeToForeign msg
    foreignMessage StrategyAuthenticationMessage = unsafeToForeign true
    foreignMessage NoAuthenticationMessage = unsafeToForeign $ toNullable Nothing
    optionsImpl =
      { session:          options.session
      , successRedirect:  toNullable options.successRedirect
      , successMessage:   foreignMessage options.successMessage
      , successFlash:     foreignMessage options.successFlash
      , failureRedirect:  toNullable options.failureRedirect
      , failureMessage:   foreignMessage options.failureMessage
      , failureFlash:     foreignMessage options.failureFlash
      , assignProperty:   toNullable options.assignProperty
      }
    convertOnAuthenticate cb err user info status =
      runHandlerM (cb (toMaybe err) (toMaybe user) (toMaybe info) (toMaybe status)) req res nxt
    onAuthenticateImpl = do
      cb <- onAuthenticate
      pure $ mkFn4 $ convertOnAuthenticate cb
    authMiddleware =
      runFn4 _authenticate passport strategy optionsImpl (toNullable onAuthenticateImpl)
  liftEffect $ runFn3 authMiddleware req res nxt
  pure unit


foreign import _isAuthenticated :: Fn1 Request (Effect Boolean)

isAuthenticated :: HandlerM Boolean
isAuthenticated = HandlerM \req _ _ -> do
  authenticated <- liftEffect $ runFn1 _isAuthenticated req
  pure authenticated


foreign import _logIn :: forall user.
                      Fn4
                        Request
                        user
                        LoginOptions
                        (Nullable OnLoginImpl)
                        (Effect Unit)

type OnLoginImpl = Fn1 (Nullable Error) (Effect Unit)

type OnLogin = Maybe Error -> Handler

type LoginOptions = { session :: Boolean }

loginOptions :: LoginOptions
loginOptions = { session: true }

logIn :: forall user.
      user
      -> LoginOptions
      -> Maybe OnLogin
      -> Handler
logIn user options onLogin = HandlerM \req res nxt -> do
  let
    onLoginImpl cb = toNullable $ do
      onLoginCb <- cb
      pure $ \err -> runHandlerM (onLoginCb $ toMaybe err) req res nxt
  liftEffect $ runFn4 _logIn req user options (onLoginImpl onLogin)
  pure unit


foreign import _logOut :: Fn1 Request (Effect Unit)

logOut :: HandlerM Unit
logOut = HandlerM \req _ _ -> do
  liftEffect $ runFn1 _logOut req
  pure unit


foreign import _getUser :: forall user. Fn1 Request (Effect (Nullable user))

getUser :: forall user. HandlerM (Maybe user)
getUser = HandlerM \req _ _ -> do
  user <- liftEffect $ runFn1 _getUser req
  pure $ toMaybe user

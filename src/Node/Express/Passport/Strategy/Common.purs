module Node.Express.Passport.Strategy.Common
  ( PassportStrategy
  , setStrategy
  )
  where

import Prelude
import Effect (Effect)
import Data.Function.Uncurried (Fn3, runFn3)
import Node.Express.Passport.Common (Passport)

foreign import data PassportStrategy :: Type

foreign import _setStrategy :: Fn3
                                Passport
                                String
                                PassportStrategy
                                (Effect Unit)


setStrategy ::
            Passport
            -> String
            -> PassportStrategy
            -> Effect Unit
setStrategy = runFn3 _setStrategy

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Example of a typed DSL
module DSL (temperature, say, fib, send, DSLSession(..)) where

import qualified Control.Remote.Monad.JSON as R
import qualified Control.Remote.Monad.JSON.Types as R 
import Data.Aeson
import Data.Text (Text)
import Control.Natural

newtype DSLSession = DSLSession R.Session

newtype DSL a = DSL (R.RPC a)
  deriving (Monad, Applicative, Functor)

method :: FromJSON a => Text -> [Value] -> DSL a
method nm args = DSL (R.method nm (R.List args))

notification :: Text -> [Value] -> DSL ()
notification nm = DSL . R.notification nm . R.List

temperature :: DSL Int
temperature = method "temperature" []

say :: Text -> DSL ()
say msg = notification "say" [toJSON msg]

fib :: Int -> DSL Int
fib x = method "fib" [toJSON x]


send :: DSLSession -> DSL a -> IO a
send (DSLSession s) (DSL m) = R.send s m

instance Transformation DSL IO DSLSession where
   (#) = send


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Example of a typed DSL
module DSL (temperature, say, fib, send, Session(..)) where
        
import Control.Monad        
import qualified Control.Monad.Remote.JSON.Strong as R
import Control.Applicative
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as IO
import System.Random
import Control.Transformation

newtype Session = Session R.Session 

newtype RPC a = RPC (R.RPC a)
  deriving (Monad, Applicative, Functor)


method :: FromJSON a => Text -> [Value] -> RPC a
method nm args = (RPC . R.result) (R.method nm args)

notification :: Text -> [Value] -> RPC ()
notification nm = RPC . R.notification nm

temperature :: RPC Int
temperature = method "temperature" [] 
                       
say :: Text -> RPC ()
say msg = notification "say" [toJSON msg]

fib :: Int -> RPC Int
fib x = method "fib" [toJSON x]


send :: Session -> RPC a -> IO a
send (Session s) (RPC m) = R.send s m

instance Transformation RPC IO Session where
   (#) = send


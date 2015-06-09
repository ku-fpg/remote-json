{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Example of a typed DSL
module DSL (temperature, say, send, Session(..)) where
        
import Control.Monad        
import qualified Control.Monad.Remote.JSON_Strong as R
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


procedure :: FromJSON a => Text -> [Value] -> Value ->RPC a
procedure nm args id = (RPC . R.result) (R.procedure nm args id)

command :: Text -> [Value] -> RPC ()
command nm = RPC . R.command nm

temperature ::Int -> RPC Int
temperature id = procedure "temperature" [] (toJSON id) 
                       
say :: Text -> RPC ()
say msg = command "say" [toJSON msg]

send :: Session -> RPC a -> IO a
send (Session s) (RPC m) = R.send s m

instance Transformation RPC IO Session where
   (#) = send


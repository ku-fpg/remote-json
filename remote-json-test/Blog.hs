{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}


module Main where
        
import Session       

import Control.Remote.Monad.JSON
import Control.Remote.Monad.JSON.Client
import Control.Remote.Monad.JSON.Router
import Control.Remote.Monad.JSON.Server
import Control.Remote.Monad.JSON.Trace
import Control.Remote.Monad.JSON.Types
import Control.Natural
import Control.Concurrent (forkIO,threadDelay)

import Data.Aeson.Types
import Data.Monoid
import Data.Text (Text, pack)
import qualified Data.Text.IO as IO

import System.Random
import Data.Scientific as Scientific

main = do
  forkIO $ serverReceiveAPI 4001 "/wobble" $ router sequence $ nat remote
  threadDelay (1000 * 1000)

  putStrLn "Weak Bundle"
  let s = weakSession (traceSendAPI "" $ clientSendAPI "http://localhost:4001/wobble")
  (t,u) <- send s $ do
          say "Hello, "
          t <- temperature
          say "World!"
          u <- uptime "orange"
          return (t,u)
  print t
  print u  

  putStrLn "Strong Bundle"
  let s = strongSession (traceSendAPI "" $ clientSendAPI "http://localhost:4001/wobble")
  (t,u) <- send s $ do
          say "Hello, "
          t <- temperature
          say "World!"
          u <- uptime "orange"
          return (t,u)
  print t
  print u  

  putStrLn "Ap Bundle"
  let s = applicativeSession (traceSendAPI "" $ clientSendAPI "http://localhost:4001/wobble")
  (t,u) <- send s $ 
          say "Hello, " *>
          (pure (,) <*> temperature   
                    <*  say "World!"
                    <*> uptime "orange")
  print t
  print u  

     

------------------------------------------------------------------------------
-- Client API

say :: Text -> RPC ()
say msg = notification "say" $ List [String msg]

temperature :: RPC Int
temperature = result $ method "temperature" None

uptime :: Text -> RPC Double
uptime nm = result $ method "uptime" $ List [String nm]


------------------------------------------------------------------------------
-- Server API

remote :: Call a -> IO a
remote (CallNotification nm args)    = remoteCommand (Notification nm args)
remote (CallMethod nm args)          = remoteProcedure (Method nm args)

remoteCommand :: Notification -> IO ()
remoteCommand (Notification "say" (List msg)) = print msg
remoteCommand _ = methodNotFound

remoteProcedure :: Method a -> IO a
remoteProcedure (Method "temperature" _) = do
        t <- randomRIO (50, 100 :: Int)
        IO.putStrLn $ "temperature: " <> pack (show t)
        return $ toJSON t
remoteProcedure (Method "uptime" (List [String nm])) = do
        t <- randomRIO (50, 100 :: Double)
        IO.putStrLn $ nm <> "'s uptime: " <> pack (show t)
        return $ toJSON t
remoteProcedure _ = methodNotFound

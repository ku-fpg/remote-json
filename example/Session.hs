{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Session (sessions) where

import Control.Applicative
import Control.Monad        
import Control.Monad.Remote.JSON
import Control.Monad.Remote.JSON.Types -- TODO RM
import Control.Monad.Remote.JSON.Debug
import Control.Monad.Remote.JSON.Router
import Control.Monad.Remote.JSON.Types (transport)
import Control.Remote.Monad.Packet.Weak (WeakPacket(..))
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Data.Text (Text,pack)
import qualified Data.Text.IO as IO
import System.Random
import Data.Scientific as Scientific

sessions :: [Session]
sessions = 
  [ weakSession $ transport $ router sequence $ remote ]
{-
  , session $ traceSendAPI "session"
            $ transport                     $ router sequence $ remote
  , session $ transport $ traceReceiveAPI "transport"
                        $ router sequence $ remote
  , session $ transport $ router sequence $ traceCallAPI "call"
                                          $ remote ]
-}

remote :: WeakPacket Command Procedure a -> IO a
remote (Command c)   = remoteCommand c
remote (Procedure p) = remoteProcedure p

remoteCommand :: Command -> IO ()
remoteCommand (Notification "say" args) = case args of
    List [String txt] -> do
        IO.putStrLn $ "remote: " <> txt
        return ()
    _ ->  invalidParams
remoteCommand _ = methodNotFound

remoteProcedure :: Procedure a -> IO a
remoteProcedure (Method "temperature" _) = do
        t <- randomRIO (50, 100 :: Int)
        IO.putStrLn $ "temperature: " <> pack (show t)
        return $ toJSON t
remoteProcedure (Method "fib" args) = case args of
    List [Number n] -> do
        case toBoundedInteger n of
          Just i -> return $ Number $ fromIntegral $ fib $ i
          _ -> invalidParams
    _ ->  invalidParams
  where fib :: Int -> Int
        fib n = if n < 2 then 1 else fib(n-1)+fib(n-2)
remoteProcedure _ = methodNotFound


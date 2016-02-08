{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Session (sessions) where

import Control.Applicative
import Control.Monad        
import Control.Remote.Monad.JSON
import Control.Remote.Monad.JSON.Types -- TODO RM
import Control.Remote.Monad.JSON.Trace
import Control.Remote.Monad.JSON.Router
import Control.Remote.Monad.JSON.Server
import Control.Remote.Monad.JSON.Client
import Control.Remote.Monad.Packet.Weak (WeakPacket(..))
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Data.Text (Text,pack)
import qualified Data.Text.IO as IO
import System.Random
import Data.Scientific as Scientific
import Control.Natural
import Control.Concurrent (forkIO,threadDelay)

sessions :: [IO Session]
sessions = 
  [ do let port = 4000 + n + m
       print port
       forkIO $ serverReceiveAPI port "/test-remote-json" r
       threadDelay (1000 * 1000)
       return $ f $ clientSendAPI ("http://localhost:" ++ show port ++ "/test-remote-json")
       
  | (f :: (SendAPI ~> IO) -> Session,n) <- [ weakSession, strongSession ] `zip` [100,200..]
  , (Nat r :: ReceiveAPI :~> IO,m) 
         <- [ Nat $ router sequence $ remote
            , Nat $ traceReceiveAPI "transport"
                  $ router sequence $ remote
            , Nat $ router sequence $ traceCallAPI "call"
                                    $ remote 
            ] `zip` [0..]
  ]

remote :: WeakPacket Notification Method a -> IO a
remote (Command c)   = remoteCommand c
remote (Procedure p) = remoteProcedure p

remoteCommand :: Notification -> IO ()
remoteCommand (Notification "say" args) = case args of
    List [String txt] -> do
        IO.putStrLn $ "remote: " <> txt
        return ()
    _ ->  invalidParams
remoteCommand _ = methodNotFound

remoteProcedure :: Method a -> IO a
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


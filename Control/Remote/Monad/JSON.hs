{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}

{-|
Module:      Control.Remote.Monad.JSON where
Copyright:   (C) 2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Justin Dawson
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.JSON (
        -- * JSON-RPC DSL
        RPC,  -- abstract
        method,
        notification,
        -- * Invoke the JSON RPC Remote Monad
        send,
        Session,
        weakSession,
        strongSession,
        applicativeSession,
        SendAPI(..),
        -- * Types
        Args(..)
  ) where

import           Control.Monad.Fail() 
import           Control.Remote.Monad.JSON.Types
import           Control.Monad.Catch()
import           Control.Natural

import           Data.Aeson
import           Data.Text(Text)
import           Control.Remote.Monad
import qualified Control.Remote.Monad.Packet.Weak as WP
import qualified Control.Remote.Monad.Packet.Strong as SP
import qualified Control.Remote.Monad.Packet.Applicative as AP
import qualified Data.HashMap.Strict as HM

-- | Sets up a JSON-RPC method call with the function name and arguments                                  
method :: FromJSON a => Text -> Args -> RPC a
method nm args = RPC $ procedure $ Method nm args

-- | Sets up a JSON-RPC notification call with the function name and arguments
notification :: Text -> Args -> RPC ()
notification nm args = RPC $ command $ Notification nm args

runWeakRPC :: (SendAPI ~> IO) -> WP.WeakPacket Notification Method a -> IO a
runWeakRPC f (WP.Command n)   = f (Async (toJSON $ NotificationCall $ n))
runWeakRPC f (WP.Procedure m) = do
          let tid = 1
          v <- f (Sync (toJSON $ mkMethodCall m  tid))
          res <- parseReply v
          parseMethodResult m tid res 

runStrongRPC :: (SendAPI ~> IO) -> SP.StrongPacket Notification Method a ->  IO a
runStrongRPC f packet = go  packet ([]++)
      where
            go :: forall a . SP.StrongPacket Notification Method a -> ([Notification]->[Notification]) -> IO a
            go  (SP.Command n cs) ls =  go cs (ls . ([n] ++))
            go (SP.Done) ls = do
                             let toSend = (map(toJSON . NotificationCall) (ls [])) 
                             () <- sendBatchAsync f toSend
                             return ()
            go (SP.Procedure m) ls = do 
                            let tid = 1
                            let toSend = (map (toJSON . NotificationCall) (ls []) ) ++ [toJSON $ mkMethodCall m tid]
                            res <- sendBatchSync f toSend
                            parseMethodResult m tid res


sendBatchAsync :: (SendAPI ~> IO) -> [Value] -> IO ()
sendBatchAsync _ []  = return ()             -- never send empty packet
sendBatchAsync f [x] = f (Async x)           -- send singleton packet
sendBatchAsync f xs  = f (Async (toJSON xs)) -- send batch packet

-- There must be at least one command in the list
sendBatchSync :: (SendAPI ~> IO) -> [Value] -> IO (HM.HashMap IDTag Value)
sendBatchSync f xs  = f (Sync (toJSON xs)) >>= parseReply -- send batch packet

runApplicativeRPC :: (SendAPI ~> IO) -> AP.ApplicativePacket Notification Method a -> IO a
runApplicativeRPC f packet = do 
                   case AP.superCommand packet of
                     Just a -> do () <- sendBatchAsync f (map toJSON $ ls0 [])
                                  return a
                     Nothing ->  do
                           rs <- sendBatchSync f (map toJSON $ ls0 [])
                           ff0 rs 
           
      where 
            (ls0,ff0) = go packet 1 

            go :: forall a . AP.ApplicativePacket Notification Method a -> IDTag
               -> ([JSONCall]->[JSONCall], (HM.HashMap IDTag Value -> IO a))
            go (AP.Pure a )         _tid = (id,  \ _ -> return a)
            go (AP.Command aps n)    tid = (ls . ([(NotificationCall n)] ++), ff)
                                      where  (ls,ff) = go aps tid 
            go (AP.Procedure aps m ) tid = ( ls . ([mkMethodCall m tid]++)
                                           , \ mp -> ff mp <*> parseMethodResult m tid mp
                                           )
                                      where (ls, ff) = go aps (tid + 1)
        
-- | Takes a function that handles the sending of Async and Sync messages,
-- and sends each Notification and Method one at a time     
weakSession :: (SendAPI :~> IO) -> Session
weakSession f = Session $ runMonad (nat $ runWeakRPC (run f))

-- | Takes a function that handles the sending of Async and Sync messages,
-- and bundles Notifications together terminated by an optional Method
strongSession :: (SendAPI :~> IO) -> Session
strongSession f = Session $ runMonad (nat $ runStrongRPC (run f))

-- | Takes a function that handles the sending of Async and Sync messages,
-- and bundles together Notifications and Procedures that are used in
-- Applicative calls
applicativeSession :: (SendAPI :~> IO) -> Session
applicativeSession f = Session $ runMonad (nat $ runApplicativeRPC (run f))

-- | Send RPC Notifications and Methods by using the given session
send :: Session -> RPC a -> IO a
send (Session f) (RPC m) = f # m


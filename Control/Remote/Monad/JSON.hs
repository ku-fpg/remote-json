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
Maintainer:  Andy Gill
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
        -- * Utility to help parse the result 'Value' into a native Haskell value.
        result,
        -- * Types
        Args(..)
  ) where

import           Control.Monad
import           Control.Monad.Fail() 
import           Control.Remote.Monad.JSON.Types
import           Control.Monad.State
import           Control.Monad.Catch()

import           Data.Aeson
import           Data.Text(Text)
import           Control.Remote.Monad
import qualified Control.Remote.Monad.Packet.Weak as WP
import qualified Control.Remote.Monad.Packet.Strong as SP
import qualified Control.Remote.Monad.Packet.Applicative as AP
import qualified Data.HashMap.Strict as HM

{-
procedureToJSON :: Procedure a -> Value -> Value
procedureToJSON (Method nm args) tag  = object $      
 [ "jsonrpc" .= ("2.0" :: Text)      
 , "method" .= nm                    
 , "id" .= tag                       
 ] ++ case args of                   
        None -> []                   
        _    -> [ "params" .= args ] 


commandToJSON ::Command -> Value
commandToJSON (Notification nm args) = object $    
 [ "jsonrpc" .= ("2.0" :: Text)      
 , "method" .= nm                    
 ] ++ case args of                   
        None -> []                   
        _    -> [ "params" .= args ] 
-}
                                     
-- parse the result of sending/executing a procedure, and return the result and the id
-- TODO: think about how to handle ErrorResponse.
{-parseMethodResult :: Monad m => Method a -> Value -> m (a,Value)
parseMethodResult (Method {}) repl = do
    case fromJSON repl of
      Success (Response v tag) -> return (v,tag)
      _ -> error $ "bad packet in parseMethodResult:" ++  show repl
-}
method :: Text -> Args -> RPC Value
method nm args = RPC $ procedure $ Method nm args

notification :: Text -> Args -> RPC ()
notification nm args = RPC $ command $ Notification nm args

-- | Utility for parsing the result, or failing
result :: (Monad m, FromJSON a) => m Value -> m a
result m = do
        Success r <- liftM fromJSON m
        return r

runWeakRPC :: (forall a . SendAPI a -> IO a) -> WP.WeakPacket Notification Method a -> IO a
runWeakRPC f (WP.Command n)   = f (Async (toJSON $ NotificationCall $ n))
runWeakRPC f (WP.Procedure m) = do
          let tid = 1
          v <- f (Sync (toJSON $ mkMethodCall m  tid))
          res <- parseReply v
          parseMethodResult m tid res 

runStrongRPC :: (forall a . SendAPI a -> IO a) -> SP.StrongPacket Notification Method a ->  IO a
runStrongRPC f packet = evalStateT (go  packet) ([]++)
      where
            go :: forall a . SP.StrongPacket Notification Method a -> StateT ([Notification]->[Notification]) IO a
            go  (SP.Command n cs) = do 
                                      modify $ \st -> st . ([n] ++)
                                      go cs
            go (SP.Done) = do
                             st <- get
                             put ([] ++)
                             let toSend = (map(toJSON . NotificationCall) (st [])) 
                             liftIO $ f (Async $ toJSON toSend)
                             return ()
            go (SP.Procedure m) = do 
                            st <- get
                            put ([]++)
                            let tid = 1
                            let toSend = (map (toJSON . NotificationCall) (st []) ) ++ [toJSON $ mkMethodCall m tid]
                            v <- liftIO $ f (Sync $ toJSON toSend)
                            res <- parseReply v 
                            parseMethodResult m tid res


runApplicativeRPC :: (forall a . SendAPI a -> IO a) -> AP.ApplicativePacket Notification Method a -> IO a
runApplicativeRPC f packet = do 
                    
                   let (ls,ff) = go packet 1 
                   case AP.superCommand packet of
                     Just r -> do f (Async $ toJSON $ ls [])
                                  ff HM.empty
                     Nothing ->  do
                           rr <- f (Sync $ toJSON $ ls [])
                           rs <- parseReply rr
                           ff rs 
           
      where 
            --TODO Consider removing Base IO in monad (StateT-> State)
            go :: forall a . AP.ApplicativePacket Notification Method a -> IDTag
               -> ([JSONCall]->[JSONCall], (HM.HashMap IDTag Value -> IO a))
            go (AP.Pure a ) tid =  (id,  \ _ -> return a)
            go (AP.Command aps n) tid = (ls . ([(NotificationCall n)] ++), ff)
                                      where  (ls,ff) = go aps tid 
            go (AP.Procedure aps m ) tid = ( ls . ([mkMethodCall m tid]++)
                                           , \ mp -> ff mp <*> parseMethodResult m tid mp
                                           )
                                      where (ls, ff) = go aps (tid + 1)
        
-- TODO: Add an IO here, to allow setup
weakSession :: (forall a . SendAPI a -> IO a) -> Session
weakSession f = Session $ \ m -> runMonad (runWeakRPC f) m

strongSession :: (forall a . SendAPI a -> IO a) -> Session
strongSession f = Session $ \ m -> runMonad (runStrongRPC f) m

applicativeSession :: (forall a . SendAPI a -> IO a) -> Session
applicativeSession f = Session $ \ m -> runMonad (runApplicativeRPC f) m

send :: Session -> RPC a -> IO a
send (Session f) (RPC m) = f m

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

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
parseMethodResult :: Monad m => Method a -> Value -> m (a,Value)
parseMethodResult (Method {}) repl = do
    case fromJSON repl of
      Success (Response v tag) -> return (v,tag)
      _ -> error $ "bad packet in parseMethodResult:" ++  show repl

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
          v <- f (Sync (toJSON $ mkMethodCall m $ Number 1))
          (a,_) <- parseMethodResult m v
          return a

runStrongRPC :: (forall a . SendAPI a -> IO a) -> SP.StrongPacket Notification Method a ->  IO a
runStrongRPC f packet = evalStateT (go f packet) ([]++)
      where
            go :: (forall a . SendAPI a -> IO a) -> SP.StrongPacket Notification Method a -> StateT ([Notification]->[Notification]) IO a
            go f (SP.Command n cs) = do 
                                      modify $ \st -> st . ([n] ++)
                                      go f cs
            go _ (SP.Done) = do
                             st <- get
                             put ([] ++)
                             let toSend = (map(toJSON . NotificationCall) (st [])) 
                             liftIO $ f (Async $ toJSON toSend)
                             return ()
            go f (SP.Procedure m) = do 
                            st <- get
                            put ([]++)
                            let toSend = (map (toJSON . NotificationCall) (st []) ) ++ [toJSON $ mkMethodCall m $ Number 1]
                            v <- liftIO $ f (Sync $ toJSON toSend)
                            -- Expecting an array, always
                            case fromJSON v of
                              Success [v0 :: Value] -> do
                                  (a,_) <- parseMethodResult m v0
                                  return a
                              _ -> fail "non singleton result from strong packet result"

-- TODO: Add an IO here, to allow setup
weakSession :: (forall a . SendAPI a -> IO a) -> Session
weakSession f = Session $ \ m -> runMonad (runWeakRPC f) m

strongSession :: (forall a . SendAPI a -> IO a) -> Session
strongSession f = Session $ \ m -> (runMonad (runStrongRPC f) m)

send :: Session -> RPC a -> IO a
send (Session f) (RPC m) = f m



{-
data RPC :: * -> * where
    Pure         :: a ->                     RPC a
    Bind         :: RPC a -> (a -> RPC b) -> RPC b
    Ap           :: RPC (a -> b) -> RPC a -> RPC b
    Procedure    :: Text -> Args ->          RPC Value
    Command      :: Text -> Args ->          RPC ()
    Throw        :: (Exception e) => e ->    RPC a
  deriving Typeable

instance Functor RPC where
  fmap f m = pure f <*> m

instance Applicative RPC where
  pure  = Pure
  (<*>) = Ap

instance Monad RPC where
  return     = pure
  (>>=)      = Bind
  (>>) m1 m2 = flip const <$> m1 <*> m2  -- so that the SAF can take advantage of this
  fail       = Fail.fail

instance Fail.MonadFail RPC where
  fail = Throw . userError

instance MonadThrow RPC where
  throwM = Throw

-- We use the terms method and notification because this is the terminology
-- used by JSON-RPC. They *are* remote monad procedures and commands.

method :: Text -> Args -> RPC Value
method nm args = Procedure nm args

notification :: Text -> Args -> RPC ()
notification nm args = Command nm args

-- | Utility for parsing the result, or failing
result :: (Monad m, FromJSON a) => m Value -> m a
result m = do
        Success r <- liftM fromJSON m
        return r

data Session = Session
        { remoteMonad       :: RemoteType
        , remoteSession     :: SendAPI ~> IO
        }

session :: (SendAPI ~> IO) -> Session
session = Session Weak

-- | 'send' the remote monad `RPC` to the remote site, for execution.
send :: Session -> RPC a -> IO a
send session m = case remoteMonad session of
        Weak   -> sendWeak session m
        Strong -> do (a,st) <- runStateT (sendStrong session m) []
                     when (not (null st)) $ do
                        void $ remoteSession session $ Async $ toJSON st
                     return a

data SendState :: RemoteType -> * where
 WeakState :: Int -> SendState Weak

initWeakState :: SendState Weak
initWeakState = WeakState 1

sendWeak :: Session -> RPC a -> IO a
sendWeak _ (Pure a)   = return a
sendWeak s (Bind f k) = sendWeak s f >>= sendWeak s . k
sendWeak s (Ap f a)   = sendWeak s f <*> sendWeak s a
sendWeak _ (Throw e) = throwM e
sendWeak s (Command nm args) =
      remoteSession s $ Async $ toJSON $ Notification nm args
sendWeak s (Procedure nm args) = do
        r <- remoteSession s $ Sync $ toJSON $ Method nm args (toJSON i)
        case fromJSON r of
                  Success (Response v tag)
                          | tag == toJSON i -> return v
                          | otherwise       -> fail "remote error: tag numbers do not match"
                  _ -> fail "remote error: failing response returned"
  where i = 1 :: Int

-- it might be cleaner to have the state be the Value, not the Call ().
sendStrong :: Session -> RPC a -> StateT [JSONCall ()] IO a
sendStrong _ (Pure a)   = return a
sendStrong s (Bind f k) = sendStrong s f >>= sendStrong s . k
sendStrong s (Ap f a)   = sendStrong s f <*> sendStrong s a
sendStrong s (Throw e) = do
                      st <- get
                      put []
                      let toSend = map toJSON st
                      liftIO $ remoteSession s $ Async $ toJSON $ toSend
                      throwM e

sendStrong s (Command nm args) = do
      modify $ \ st -> st ++ [Notification nm args]
      return ()
sendStrong s (Procedure nm args) = do
        st <- get
        put []
        let toSend = map toJSON st ++ [toJSON $ Method nm args (toJSON i)]
        -- This use of 'toJSON' is really building an Array
        r <- liftIO $ remoteSession s $ Sync $ toJSON $ toSend
        case fromJSON r of
                  Success [Response v tag]
                          | tag == toJSON i -> return v
                          | otherwise       -> fail "remote error: tag numbers do not match"
                  _ -> fail "remote error: failing response returned"
  where i = 1 :: Int

-}

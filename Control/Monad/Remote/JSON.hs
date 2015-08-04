{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

{-|
Module:      Control.Monad.Remote.JSON where
Copyright:   (C) 2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Monad.Remote.JSON(
        -- * JSON-RPC DSL
        RPC,  -- abstract
        method,
        notification,
        result,
        -- * Invoke the JSON RPC Remote Monad
        send,
        Session,
	remoteSession,
	remoteMonad,
        RemoteType(..),
        session,
        -- * Types
        Args(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Remote.JSON.Types
import           Control.Monad.State
import           Control.Monad.Catch

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text(Text, append, pack, unpack)
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding(decodeUtf8)
import           Data.Typeable
import qualified Data.Vector as V


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
  fail       = Throw . userError 

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
        , remoteSession     :: forall a. SessionAPI a -> IO a
        }
   
session :: (forall a . SessionAPI a -> IO a) -> Session
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
sendStrong :: Session -> RPC a -> StateT [Call ()] IO a
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

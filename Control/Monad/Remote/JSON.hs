{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

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
        Session(..),
        RemoteType(..),
        session,
        -- * Types
        Args(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Remote.JSON.Types
import           Control.Monad.State

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text(Text, append, pack, unpack)
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding(decodeUtf8)
import           Data.Typeable
import qualified Data.Vector as V


data RPC :: * -> * where
    Pure         :: a ->                              RPC a
    Bind         :: RPC a -> (a -> RPC b) ->          RPC b
    Ap           :: RPC (a -> b) -> RPC a ->          RPC b
    Procedure       :: Text -> Args -> RPC Value
    Command :: Text -> Args ->                RPC ()
    Fail         :: String ->                         RPC a
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
  fail       = Fail

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

type CommandList = [Value]
type SessionID = Int
type MyState = (CommandList, SessionID)

data Session = Session 
        { remoteMonad       :: RemoteType
        , remoteApplicative :: RemoteType
        , remoteSession     :: forall a. SessionAPI a -> IO a
        }
   
session :: (forall a . SessionAPI a -> IO a) -> Session
session = Session Weak Weak

-- 
send :: Session -> RPC a -> IO a
send session@(Session Weak   _ interp) m = evalStateT (send' session m) ([],1)
send session@(Session Strong _ interp) m =
        do (a,s) <- runStateT (send' session m) ([],1)
           case s of
             ([],_) -> return ()
             (xs,_) -> do void $ interp (Async (toJSON xs))
           return a

send' :: Session -> RPC a -> StateT MyState IO a
send' _ (Pure a) = return a
send' s (Bind f k) = send' s f >>= send' s . k
send' s (Ap f a) = send' s f <*> send' s a
send' (Session t _ interp) (Procedure nm args) = do

      (q,sessionId) <- get
      when ((q /= []) && t == Strong) $ liftIO $ interp (Async (toJSON q))

      put ([],sessionId + 1)
      
      v <- liftIO $ interp $ Sync $ toJSON $ Method nm args $ Number $ fromIntegral $ sessionId

      let p :: Object -> Parser (Text,Value, Maybe Value)
          p o =  (,,) <$> o .: "jsonrpc"
                      <*> o .: "result"
                      <*> o .: "id"

      let p2 :: Object -> Parser (Text,Value)
          p2 o =  (,) <$> o .: "jsonrpc"
                     <*> o .: "error"

      let p3 :: Object -> Parser (Value,Value)
          p3 o =  (,) <$> o .: "code"
                     <*> o .: "message"
      let package = (append . (flip append (" ")) . pack . show)
      let handleError:: Object -> IO Value
          handleError o = do putStr "Error: "
                             case parseMaybe p3 o of
                               Just ((Number code),(String mesg)) ->
                                   -- Create error message
                                   return $ String $ package code mesg
                               _  -> return Null
      case v of
        Object o -> case parseMaybe p o of
                  Just ("2.0",v', (Just retId)) -> do
                                               if retId == (toJSON sessionId) then
                                                 return v'
                                               else
                                                 fail "ID's didn't match"
                  _               -> case parseMaybe p2 o of
                                      Just("2.0",(Object v')) -> liftIO $ handleError v'

                                      _              -> do return Null
        _ -> return Null


send' (Session t _ interp) (Command nm args) = do
      let m = toJSON $ Notification nm args
      case t of
         Strong -> do (list, id') <-get
                      put (list ++ [m], id')
         Weak -> liftIO $ interp (Async m)




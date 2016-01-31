{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module:      Control.Monad.Remote.JSON.Debug where
Copyright:   (C) 2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Monad.Remote.JSON.Debug where
        
import           Control.Monad.Remote.JSON.Types
import           Control.Monad.Remote.JSON (Session(..))
import           Control.Monad.Remote.Fresh
import           Control.Monad.State
import           Control.Natural
import           Control.Remote.Monad.Packet.Weak (WeakPacket(..))

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding(decodeUtf8)


-- | A tracing natural transformation morphism over the Session API.
traceSendAPI :: MonadIO m => String -> (SendAPI ~> m) -> (SendAPI ~> m)
traceSendAPI msg f (Sync v)  = do
          liftIO $ putStrLn $ msg ++ "--> " ++ LT.unpack (decodeUtf8 (encode v))
          r <- f (Sync v)
          liftIO $ putStrLn $ msg ++ "<-- " ++ LT.unpack (decodeUtf8 (encode r))
          return r
traceSendAPI msg f (Async v) = do
          liftIO $ putStrLn $ msg ++ "<-- " ++ LT.unpack (decodeUtf8 (encode v))
          r <- f (Async v)
          liftIO $ putStrLn $ msg ++ "// No response"
          return r

-- | A tracing natural transformation morphism over the Transport API.
traceReceiveAPI :: MonadIO m => String -> (ReceiveAPI ~> m) -> (ReceiveAPI ~> m)
traceReceiveAPI msg f (Receive v)  = do
          liftIO $ putStrLn $ msg ++ "--> " ++ LT.unpack (decodeUtf8 (encode v))
          r <- f (Receive v)
          case r of
            Nothing -> liftIO $ putStrLn $ msg ++ "// No response"
            Just v -> liftIO $ putStrLn $ msg ++ "<-- " ++ LT.unpack (decodeUtf8 (encode r))
          return r

-- | A tracing version of the 'Packet a -> m a' natural transformation morphism.
traceWeakPacketAPI :: MonadIO m => String -> (WeakPacket Notification Method ~> m) -> (WeakPacket Notification Method ~> m)
traceWeakPacketAPI msg f p@(Procedure c@(Method nm args)) = do
          liftIO $ putStrLn $ msg ++ " method " ++ show c
          r <- f p
          liftIO $ putStrLn $ msg ++ " return " ++ LT.unpack (decodeUtf8 (encode r))
          return r
traceWeakPacketAPI msg f p@(Command n@(Notification nm args)) = do
          liftIO $ putStrLn $ msg ++ " notification " ++ show n
          f p


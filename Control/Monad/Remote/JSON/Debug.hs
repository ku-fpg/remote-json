{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Control.Monad.Remote.JSON
import           Control.Monad.State

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding(decodeUtf8)


-- | A tracing natural transformation morphism over the Session API.
traceSessionAPI :: MonadIO m => String -> (SessionAPI a -> m a) -> (SessionAPI a -> m a)
traceSessionAPI msg f (Sync v)  = do
          liftIO $ putStrLn $ msg ++ "--> " ++ LT.unpack (decodeUtf8 (encode v))
          r <- f (Sync v)
          liftIO $ putStrLn $ msg ++ "<-- " ++ LT.unpack (decodeUtf8 (encode r))
          return r
traceSessionAPI msg f (Async v) = do
          liftIO $ putStrLn $ msg ++ "<-- " ++ LT.unpack (decodeUtf8 (encode v))
          r <- f (Async v)
          liftIO $ putStrLn $ msg ++ "// No response"
          return r

-- | A tracing natural transformation morphism over the Transport API.
traceTransportAPI :: MonadIO m => String -> (TransportAPI a -> m a) -> (TransportAPI a -> m a)
traceTransportAPI msg f (Send v)  = do
          liftIO $ putStrLn $ msg ++ "<-- " ++ LT.unpack (decodeUtf8 (encode v))
          r <- f (Send v)
          case r of
            Nothing -> liftIO $ putStrLn $ msg ++ "// No response"
            Just v -> liftIO $ putStrLn $ msg ++ "--> " ++ LT.unpack (decodeUtf8 (encode r))
          return r

-- | A tracing version of the Session, that states, as JSON objects, what is sent and received.
traceSession :: String -> Session -> Session
traceSession msg s = s { remoteSession = traceSessionAPI msg (remoteSession s) }


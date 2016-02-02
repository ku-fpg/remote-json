{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module:      Control.Remote.Monad.JSON.Debug where
Copyright:   (C) 2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.JSON.Trace where
        
import           Control.Remote.Monad.JSON.Types
import           Control.Remote.Monad.JSON.Router (Call(..))
import           Control.Monad.State
import           Control.Natural


import           Data.Aeson
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
            Just _ -> liftIO $ putStrLn $ msg ++ "<-- " ++ LT.unpack (decodeUtf8 (encode r))
          return r

-- | A tracing version of the 'Packet a -> m a' natural transformation morphism.
traceCallAPI :: MonadIO m => String -> (Call ~> m) -> (Call ~> m)
traceCallAPI msg f p@(CallMethod nm args) = do
          let method = Method nm args
          liftIO $ putStrLn $ msg ++ " method " ++ show method
          r <- f p
          liftIO $ putStrLn $ msg ++ " return " ++ LT.unpack (decodeUtf8 (encode r))
          return r
traceCallAPI msg f p@(CallNotification nm args) = do
          let n = Notification nm args
          liftIO $ putStrLn $ msg ++ " notification " ++ show n
          f p

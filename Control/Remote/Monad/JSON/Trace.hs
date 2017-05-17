{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module:      Control.Remote.Monad.JSON.Debug where
Copyright:   (C) 2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Justin Dawson
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.JSON.Trace where

import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Natural
import           Control.Remote.Monad.JSON.Router (Call (..))
import           Control.Remote.Monad.JSON.Types


import           Data.Aeson
import qualified Data.Text.Lazy                   as LT
import           Data.Text.Lazy.Encoding          (decodeUtf8)


-- | A tracing natural transformation morphism over the Session API.
traceSendAPI :: MonadIO m => String -> (SendAPI :~> m) -> (SendAPI :~> m)
traceSendAPI msg f = wrapNT $ \ case
  (Sync v) -> do
          liftIO $ putStrLn $ msg ++ "--> " ++ LT.unpack (decodeUtf8 (encode v))
          r <- f # (Sync v)
          liftIO $ putStrLn $ msg ++ "<-- " ++ LT.unpack (decodeUtf8 (encode r))
          return r
  (Async v) -> do
          liftIO $ putStrLn $ msg ++ "--> " ++ LT.unpack (decodeUtf8 (encode v))
          () <- f # (Async v)
          liftIO $ putStrLn $ msg ++ "// No response"
          return ()

-- | A tracing natural transformation morphism over the Receive API.
traceReceiveAPI :: MonadIO m => String -> (ReceiveAPI :~> m) -> (ReceiveAPI :~> m)
traceReceiveAPI msg f = wrapNT $ \ (Receive v) -> do
          liftIO $ putStrLn $ msg ++ "--> " ++ LT.unpack (decodeUtf8 (encode v))
          r <- f # (Receive v)
          case r of
            Nothing -> liftIO $ putStrLn $ msg ++ "// No response"
            Just _ -> liftIO $ putStrLn $ msg ++ "<-- " ++ LT.unpack (decodeUtf8 (encode r))
          return r

-- | A tracing natural transformation morphism over the Call API.
traceCallAPI :: MonadIO m => String -> (Call :~> m) -> (Call :~> m)
traceCallAPI msg f = wrapNT $ \ case
  p@(CallMethod nm args) -> do
          let method = Method nm args :: Prim Value
          liftIO $ putStrLn $ msg ++ " method " ++ show method
          r <- f # p
          liftIO $ putStrLn $ msg ++ " return " ++ LT.unpack (decodeUtf8 (encode r))
          return r
  p@(CallNotification nm args) -> do
          let n = Notification nm args
          liftIO $ putStrLn $ msg ++ " notification " ++ show n
          f # p

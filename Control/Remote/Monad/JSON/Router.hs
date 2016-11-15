{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module:      Control.Remote.Monad.JSON.Router where
Copyright:   (C) 2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Justin Dawson
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.JSON.Router
        ( -- * The server RPC router
          router
          -- * The datatype that represents what we receive and what we dispatch
        , ReceiveAPI(..)
        , Call(..)
          -- * Utilty methods
        , transport
        , methodNotFound
        , invalidParams
        , parseError
        ) where

import           Control.Monad.Catch
import           Control.Remote.Monad.JSON.Types
import           Control.Natural

import           Data.Aeson
import           Data.Text(Text)
import           Data.Typeable
import qualified Data.Vector as V

-- | 'Call' is a user-visable deep embedding of a method or notification call.
-- Server's provide transformations on this to implement remote-side call dispatching.
data Call :: * -> * where
  CallMethod       :: Text -> Args -> Call Value
  CallNotification :: Text -> Args -> Call ()

-- | "The Server MAY process a batch rpc call as a set of concurrent tasks,
--    processing them in any order and with any width of parallelism."
--   We control this using the first argument.
router :: MonadCatch m
       => (forall a. [m a] -> m [a])
       -> (Call :~> m) -> (ReceiveAPI :~> m)
router s f = wrapNT $ \ case
  (Receive v@(Object {})) -> simpleRouter f v
  (Receive (Array a))
    | V.null a -> return $ Just $ invalidRequest
    | otherwise -> do
          rs <- s (map (simpleRouter f) $ V.toList a)
          case [ v | Just v <- rs ] of
            [] -> return Nothing -- If there are no Response objects contained within the
                                 -- Response array as it is to be sent to the client,
                                 -- the server MUST NOT return an empty Array and should
                                 -- return nothing at all.
            vs -> return (Just (toJSON vs))
  (Receive _) -> return $ Just $ invalidRequest

-- The simple router handle a single call.
simpleRouter :: forall m . MonadCatch m
       => (Call :~> m)
       -> Value -> m (Maybe Value)
simpleRouter (NT f) v = case call <$> fromJSON v of
    Success m -> m
    Error _ ->  return $ Just $ invalidRequest
  where
        call :: JSONCall -> m (Maybe Value)
        call (MethodCall (Method nm args) tag) = (do
                r <- f (CallMethod nm args :: Call Value)
                return $ Just $ object
                       [ "jsonrpc" .= ("2.0" :: Text)
                       , "result" .= toJSON r
                       , "id" .= tag
                       ]) `catches`
                          [ Handler $ \ (_ :: MethodNotFound) ->
                               return $ Just $ toJSON
                                      $ errorResponse (-32601) "Method not found" tag
                          , Handler $ \ (_ :: InvalidParams) ->
                               return $ Just $ toJSON
                                      $ errorResponse (-32602) "Invalid params" tag
                          , Handler $ \ (_ :: SomeException) ->
                               return $ Just $ toJSON
                                      $ errorResponse (-32603) "Internal error" tag
                          ]
        call (NotificationCall (Notification nm args)) =
            (f (CallNotification nm args) >> return Nothing) `catchAll` \ _ -> return Nothing

-- | 'transport' connects the ability to recieve a message with the ability
-- to send a message. Typically this is done using TCP/IP and HTTP,
-- but we can simulate the connection here.

transport :: (Monad f) => (ReceiveAPI :~> f) -> (SendAPI :~> f)
transport f = wrapNT $ \ case
  Sync v -> do
    r <- f # Receive v
    case r of
      Nothing -> fail "no result returned in transport"
      Just v0 -> return v0
  Async v -> do
    r <- f # Receive v
    case r of
      Nothing -> return ()
      Just v0 -> fail $ "unexpected result in transport: " ++ show v0


errorResponse :: Int -> Text -> Value -> Value
errorResponse code msg theId = toJSON $
        ErrorResponse (ErrorMessage code msg) theId

invalidRequest :: Value
invalidRequest = errorResponse (-32600) "Invalid Request" Null


-- | For use when parsing to a JSON value fails inside a server,
--   before calling the router
parseError :: Value
parseError = errorResponse (-32700) "Parse error" Null

data MethodNotFound = MethodNotFound
  deriving (Show, Typeable)

instance Exception MethodNotFound

-- | Throw this exception when a 'JSONCall a -> IO a' fails to match a method
--   or notification.
methodNotFound :: MonadThrow m => m a
methodNotFound = throwM $ MethodNotFound

data InvalidParams = InvalidParams
  deriving (Show, Typeable)

instance Exception InvalidParams

-- | Throw this for when a 'JSONCall a -> IO a' method matches, but has invalid params.
invalidParams :: MonadThrow m => m a
invalidParams = throwM $ InvalidParams

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module:      Control.Monad.Remote.JSON.Router where
Copyright:   (C) 2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Monad.Remote.JSON.Router 
        ( router
        , methodNotFound
        , invalidParams
        , parseError
        , Call(..)
        , Args(..)
        , ReceiveAPI(..)
        ) where
        
import           Control.Applicative
import           Control.Exception.Base (PatternMatchFail(..))
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Remote.JSON.Types
import           Control.Monad.State
import           Control.Natural

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text(Text, append, pack, unpack)
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding(decodeUtf8)
import           Data.Typeable
import qualified Data.Vector as V
import           Control.Remote.Monad.Packet.Weak (WeakPacket(..))

-- | "The Server MAY process a batch rpc call as a set of concurrent tasks,
--    processing them in any order and with any width of parallelism."
--   We control this using the first argument.         
router :: MonadCatch m 
       => (forall a. [m a] -> m [a])
       -> (WeakPacket Command Procedure ~> m) -> (ReceiveAPI ~> m)
router s f (Receive v@(Object {})) = simpleRouter f v
router s f (Receive v@(Array a)) 
  | V.null a = return $ Just $ invalidRequest
  | otherwise = do
        rs <- s (map (simpleRouter f) $ V.toList a)
        case [ v | Just v <- rs ] of
          [] -> return Nothing -- If there are no Response objects contained within the
                               -- Response array as it is to be sent to the client,
                               -- the server MUST NOT return an empty Array and should
                               -- return nothing at all.
          vs -> return (Just (toJSON vs))
rounter s f _ = return $ Just $ invalidRequest
        
-- The simple router handle a single call.
simpleRouter :: forall m . MonadCatch m 
       => (WeakPacket Command Procedure ~> m) 
       -> Value -> m (Maybe Value)
simpleRouter f v = case call <$> fromJSON v of
    Success m -> m
    Error e1 ->  return $ Just $ invalidRequest
  where
        call :: Call -> m (Maybe Value)
        call (MethodCall (Method nm args) tag) = (do
                v <- f (Procedure (Method nm args))
                return $ Just $ object
                       [ "jsonrpc" .= ("2.0" :: Text)
                       , "result" .= v
                       , "id" .= tag
                       ]) `catches` 
                          [ Handler $ \ (e :: MethodNotFound) -> 
                               return $ Just $ toJSON 
                                      $ errorResponse (-32601) "Method not found" tag
                          , Handler $ \ (e :: InvalidParams) -> 
                               return $ Just $ toJSON 
                                      $ errorResponse (-32602) "Invalid params" tag
                          , Handler $ \ (e :: SomeException) ->
                               return $ Just $ toJSON 
                                      $ errorResponse (-32603) "Internal error" tag                                
                          ]
        call (NotificationCall n) =
            (f (Command n) >> return Nothing) `catchAll` \ _ -> return Nothing
  

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

-- | Throw this exception when a 'Call a -> IO a' fails to match a method
--   or notification.
methodNotFound :: MonadThrow m => m a
methodNotFound = throwM $ MethodNotFound

data InvalidParams = InvalidParams 
  deriving (Show, Typeable)

instance Exception InvalidParams

-- | Throw this for when a 'Call a -> IO a' method matches, but has invalid params.
invalidParams :: MonadThrow m => m a
invalidParams = throwM $ InvalidParams
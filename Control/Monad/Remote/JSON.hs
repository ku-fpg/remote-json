{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module:      Control.Monad.Remote.JSON where
Copyright:   (C) 2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC



-}

module Control.Monad.Remote.JSON (
        -- * JSON-RPC DSL
        RPC,
        method,
        notification,
        result,
        -- * Invoke the JSON RPC Remote Monad
        send,
        Session(..),
        -- * Route the server-side JSON RPC calls
        router
  ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Text(Text)
import Control.Transformation
import Control.Applicative
import qualified Data.Vector as V

data RPC :: * -> * where
 Pure         :: a ->                     RPC a
 Bind         :: RPC a -> (a -> RPC b) -> RPC b
 Ap           :: RPC (a -> b) -> RPC a -> RPC b
 Method       :: Text -> [Value] ->       RPC Value
 Notification :: Text -> [Value] ->       RPC ()
 Fail         :: String ->                RPC a

instance Functor RPC where
  fmap f m = pure f <*> m 

instance Applicative RPC where
  pure = Pure
  (<*>) = Ap

instance Monad RPC where
  return = pure
  (>>=) = Bind
  fail  = Fail

method :: Text -> [Value] -> RPC Value
method = Method

notification :: Text -> [Value] -> RPC ()
notification = Notification

-- | Utility for parsing the result, or failing
result :: (Monad m, FromJSON a) => m Value -> m a
result m = do
        Success r <- liftM fromJSON m
        return r

-- 'Session' are the representation of how to send a message,
-- to a JSON-RPC service, where the sender chooses if they are listening
-- for a reply.

data Session = Session
  { sync  :: Value -> IO Value
  , async :: Value -> IO ()
  }

-- 'send' the JSON-RPC call, using a weak remote monad.
send :: Session -> RPC a -> IO a
send s (Pure a)   = return a
send s (Bind f k) = send s f >>= send s . k
send s (Ap f a)   = send s f <*> send s a
send s (Method nm args) = do
     let m = object [ "jsonrpc" .= ("2.0" :: Text)
     	       	    , "method" .= nm
		    , "params" .= args
		    , "id" .= Null 
		    ]
     v <- sync s m
     let p :: Object -> Parser (Text,Value)
         p o =  (,) <$> o .: "jsonrpc" 
       	      	    <*> o .: "result"
     case v of
       Object o -> case parseMaybe p o of
	             Just ("2.0",v) -> return v
                     _              -> return Null
       _ -> return Null
send s (Notification nm args) = do
     let m = object [ "jsonrpc" .= ("2.0" :: Text)
     	       	    , "method" .= nm
		    , "params" .= args
		    ]
     async s m
     return ()

-- | Allow 'Session' to use '(#)' as a generic alias for 'send'.
instance Transformation RPC IO Session where
   (#) = send

-- | 'router' takes a list of name/function pairs,
-- and dispatches them, using the JSON-RPC protocol.
router :: [ (Text, [Value] -> IO Value) ] -> Value -> IO (Maybe Value)
router db (Object o) = do
     print $ (o,parseMaybe p o)
     case parseMaybe p o of
        Just ("2.0",nm,Just (Array args),theId) -> call nm (V.toList args) theId
        Just (_,_,_,theId) -> return $ Just $ invalidRequest theId
        _ -> return $ Just $ invalidRequest Nothing
  where
        -- Handles both method and notification, depending on the id value.
        call :: Text -> [Value] -> Maybe Value -> IO (Maybe Value)
        call nm args (Just theId) = case lookup nm db of
               Just fn -> do v <- fn args
                             return $ Just $ object 
                                    [ "jsonrpc" .= ("2.0" :: Text)
                                    , "result" .= v
                                    , "id" .= theId
                                    ]
               Nothing -> return $ Just $ methodNotFound theId
        call nm args Nothing = case lookup nm db of
               Just fn -> do _ <- fn args
                             return $ Nothing
               Nothing -> return $ Nothing

        p :: Object -> Parser (Text,Text,Maybe Value,Maybe Value)
        p o =  (,,,) <$> o .:  "jsonrpc" 
                     <*> o .:  "method"
                     <*> o .:? "params"
                     -- We parse "id" directly, because "id":null is
                     -- not the same as having no "id" tag in JSON-RPC.
                     <*> optional (o .: "id")

server db _  = return $ Just $ invalidRequest

errorResponse :: Int -> Text -> Value -> Value
errorResponse code msg theId = object 
        [ "jsonrpc" .= ("2.0" :: Text)
        , "error" .= object [ "code"  .= code
                            , "message" .= msg
                            ]
        , "id" .= theId
        ]

invalidRequest :: Maybe Value -> Value
invalidRequest e = errorResponse (-32600) "Invalid Request" $ case e of
        Nothing -> Null
        Just v  -> v

methodNotFound :: Value -> Value
methodNotFound = errorResponse (-32601) "Method not found"
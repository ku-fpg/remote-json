{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

{-|
Module:      Control.Remote.Monad.JSON where
Copyright:   (C) 2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Justin Dawson
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.JSON.Types (
    -- * RPC Monad
    RPC(..)
    -- * 'Notification', 'Method' and 'Args'
  , Prim(..)
  , Args(..)
    -- * Non-GADT combination of 'Notification' and 'Method'
  , JSONCall(..)
  , mkMethodCall
    -- * Sending and Receiving APIs
  , SendAPI(..)
  , ReceiveAPI(..)
    -- * Session abstraction
  , Session(..)
    -- * Internal datatypes
  , ErrorMessage(..)
  , Response(..)
  , IDTag
  , Replies
    -- * Parsing Result
  , parseReply
  , parseMethodResult
  ) where

import           Control.Applicative
import           Control.Natural
import           Control.Remote.Monad    (KnownResult (..), RemoteMonad)

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict     as HM
import           Data.Text               (Text, unpack)

import qualified Data.Text.Lazy          as LT
import           Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Vector             as V


data Prim :: * -> * where
  Notification ::               Text -> Args -> Prim ()
  Method       :: FromJSON a => Text -> Args -> Prim a


instance KnownResult Prim where
  knownResult (Notification {}) = Just ()
  knownResult (Method {})       = Nothing


deriving instance Show (Prim a)

data JSONCall :: * where
  NotificationCall :: Prim a             -> JSONCall
  MethodCall       :: ToJSON a => Prim a -> Value -> JSONCall

-- | Internal type of our tags
type IDTag = Int

-- | Internal map of replies
type Replies = HM.HashMap IDTag Value

-- | The non-GADT version of MethodCall
mkMethodCall :: Prim a -> IDTag -> JSONCall
mkMethodCall (Method nm args) tag = MethodCall (Method nm args :: Prim Value) (Number (fromIntegral tag))
mkMethodCall (Notification nm args) tag = MethodCall (Method nm args :: Prim ()) (Number (fromIntegral tag))

-- | parseReply parses the reply JSON Value into Map of IDTag
-- to specific result from remote method call.
-- This function supports both singleton and batch results
parseReply :: Monad m => Value -> m Replies
parseReply v =  case fromJSON v of
                  Success (rs :: [Value]) -> return $ results rs
                  _ ->  return $ results [v]

                 where
                   results :: [Value] -> HM.HashMap IDTag Value
                   results rs = foldl (\ acc v1 ->
                                 case fromJSON v1 of
                                    Success (Response v2 tag) ->
                                          case fromJSON tag of
                                            Success t -> HM.insert t v2 acc
                                            _         -> error "ParseReply : Unable to obtain tag "
                                    Success (ErrorResponse msg tag) ->
                                          case fromJSON tag of
                                            Success t -> HM.insert t (error $ show msg) acc
                                            _         -> error "ParseReply : Unable to obtain error tag "
                                    Error s  -> error $ "ParseReply: An error occured: " ++ s

                              ) HM.empty rs

-- | parseMethodResult looks up a result in the finite map created from the result.
parseMethodResult :: (Monad m) => Prim a -> IDTag -> Replies -> m a
parseMethodResult (Method {}) tag hm = case HM.lookup tag hm of
                      Just x ->  case fromJSON x of
                                   (Success  v) -> return v
                                   _            -> fail $ "bad packet in parseMethodResult:" ++ show x
                      Nothing -> fail $ "Invalid id lookup in parseMethodResult:" ++ show tag
parseMethodResult (Notification{}) _tag _hm = return ()


instance Show JSONCall where
   show (MethodCall (Method nm args) tag)         = unpack nm ++ show args ++ "#" ++ LT.unpack (decodeUtf8 (encode tag))
   show (NotificationCall (Notification nm args)) = unpack nm ++ show args

instance ToJSON JSONCall where
  toJSON (MethodCall (Method nm args) tag) = object $
          [ "jsonrpc" .= ("2.0" :: Text)
          , "method" .= nm
          , "id" .= tag
          ] ++ case args of
                 None -> []
                 _    -> [ "params" .= args ]
  toJSON (NotificationCall (Notification nm args)) = object $
          [ "jsonrpc" .= ("2.0" :: Text)
          , "method" .= nm
          ] ++ case args of
                 None -> []
                 _    -> [ "params" .= args ]
instance FromJSON JSONCall where
  -- This douple-parses the params, an can be fixed
  parseJSON (Object o) =
    ((\ nm args tag -> MethodCall (Method nm args :: Prim Value) tag)
        <$> o .: "method"
        <*> (o .: "params" <|> return None)
        <*> o .: "id") <|>
    ((\ nm args -> NotificationCall (Notification nm args))
        <$> o .: "method"
        <*> (o .: "params" <|> return None))

  parseJSON _ = fail "not an Object when parsing a JSONCall Value"

-- | The JSON RPC remote monad
newtype RPC a = RPC (RemoteMonad Prim a)
  deriving (Functor, Applicative, Monad)

-- | The client-side send function API.
-- The user provides a way of dispatching this, to implement a client.
-- An example of this using wreq is found in remote-json-client
--
--   * For 'Sync', a JSON Value is send, and a JSON Value is received back as a reply.
--   * For 'Async', a JSON Value is send, and the reply, if any, is ignored.
data SendAPI :: * -> * where
    Sync  :: Value -> SendAPI Value
    Async :: Value -> SendAPI ()

deriving instance Show (SendAPI a)

-- | The server-side recieived API.
-- The user provides a way of dispatching this, to implement a server.
-- An example of this using scotty is found in remote-json-server
data ReceiveAPI :: * -> * where
    Receive :: Value -> ReceiveAPI (Maybe Value)

deriving instance Show (ReceiveAPI a)

-- | Session is a handle used for where to send a sequence of monadic commands.
newtype Session = Session (RemoteMonad Prim :~> IO)


-- | 'Args' follows the JSON-RPC spec: either a list of values,
-- or an (unordered) list of named fields, or none.
data Args where
    List :: [Value]         -> Args
    Named :: [(Text,Value)] -> Args
    None  ::                   Args

instance Show Args where
   show (List args) =
           if  null args
           then "()"
           else  concat [ t : LT.unpack (decodeUtf8 (encode x))
                        | (t,x) <- ('(':repeat ',') `zip` args
                        ] ++ ")"
   show (Named args) =
           if  null args
           then "{}"
           else  concat [ t : show i ++ ":" ++ LT.unpack (decodeUtf8 (encode v))
                        | (t,(i,v)) <- ('{':repeat ',') `zip` args
                        ] ++ "}"

   show None = ""

instance ToJSON Args where
  toJSON (List a)    = Array (V.fromList a)
  toJSON (Named ivs) = object [ i .= v | (i,v) <- ivs ]
  toJSON None       = Null

instance FromJSON Args where
  parseJSON (Array a)   = return $ List (V.toList a)
  parseJSON (Object fm) = return $ Named (HM.toList fm)
  parseJSON Null        = return $ None
  parseJSON _           = fail "parsing Args"

newtype Tag = Tag Value deriving Show

instance FromJSON Tag where
  parseJSON (Object o) = Tag <$> o .: "id"
  parseJSON _ = fail "not an Object when parsing a Tag"

-- | internal. Used for error message.
data ErrorMessage = ErrorMessage Int Text
  deriving Show

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage code msg) = object
          [ "code"  .= code
          , "message" .= msg
          ]

instance FromJSON ErrorMessage where
  parseJSON (Object o) = ErrorMessage
                          <$> o .: "code"
                          <*> o .: "message"
  parseJSON _ = fail "not an Object when parsing an ErrorMessage"

-- | internal. Used for responses.
data Response
        = Response Value             Value
        | ErrorResponse ErrorMessage Value
  deriving Show

instance ToJSON Response where
  toJSON (Response r theId) = object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "result"  .= r
                , "id"      .= theId
                ]
  toJSON (ErrorResponse msg theId) = object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "error"   .= msg
                , "id"      .= theId
                ]

instance FromJSON Response where
  parseJSON (Object o) =
          pure Response   <* (o .: "jsonrpc" :: Parser String)   -- TODO: check this returns "2.0"
                          <*> o .: "result"
                          <*> o .: "id"
      <|> ErrorResponse   <$> o .: "error"
                          <*> o .: "id"
  parseJSON _ = fail "not an Object when parsing an Response"


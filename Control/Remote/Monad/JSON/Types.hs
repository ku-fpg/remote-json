{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

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
  , Notification(..)
  , Method(..)
  , Args(..)
    -- * Non-GADT combination of 'Notification' and 'Method'
  , JSONCall(..)
  , mkMethodCall
  , IDTag
    -- * Sending and Receiving APIs
  , SendAPI(..)
  , ReceiveAPI(..)
    -- * Session abstraction
  , Session(..)
    -- * Internal datatypes
  , ErrorMessage(..)
  , Response(..)
    -- * Parsing Result
  , parseReply
  , parseMethodResult
  ) where

import           Control.Applicative
import           Control.Remote.Monad(RemoteMonad)

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import           Data.Text(Text, unpack)

import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding(decodeUtf8)
import qualified Data.Vector as V


-- The basic command type
data Notification :: * where
    Notification :: Text -> Args -> Notification

deriving instance Show Notification

-- The basic procedure type
data Method :: * -> * where
    Method     :: Text -> Args -> Method Value

deriving instance Show (Method a)

-- | This is the non-GADT, JSON-serializable version of Notification and Method.
data JSONCall :: * where
    NotificationCall :: Notification          -> JSONCall
    MethodCall       :: Method Value -> Value -> JSONCall

type IDTag = Int

-- | The GADT version of MethodCall
mkMethodCall :: Method a -> IDTag -> JSONCall
mkMethodCall m@(Method {}) tag = MethodCall m (Number (fromIntegral tag))

-- parseReply parses the reply JSON Value into Map of IDTag
-- to specific result from remote method call.
-- This function supports both singleton and batch results
parseReply :: Monad m => Value -> m (HM.HashMap IDTag Value)
parseReply v =  case fromJSON v of
                  Success (rs :: [Value]) -> return $ results rs
                  _ ->  return $ results [v]
                      
                 where
                   results :: [Value] -> HM.HashMap IDTag Value
                   results rs =  foldl (\ acc v ->
                                 case fromJSON v of
                                    Success (Response v tag) -> 
                                          case fromJSON tag of
                                            Success t -> HM.insert t v acc
                                            _    -> error "ParseReply : Unable to obtain tag "
                                    _ -> error "Bad response in parseReply"
                                                                       ) HM.empty rs

parseMethodResult :: Monad m => Method a -> IDTag -> HM.HashMap IDTag Value -> m a
parseMethodResult (Method {}) tag hm = case HM.lookup tag hm of
                      Just x ->  case fromJSON x of
                                   (Success  v) -> return v
                                   _            -> error $ "bad packet in parseMethodResult:" ++ show x 
                      Nothing -> error $ "Invalid id lookup in parseMethodResult:" ++ show tag


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
    ((\ nm args tag -> MethodCall (Method nm args) tag)
        <$> o .: "method"
        <*> (o .: "params" <|> return None)
        <*> o .: "id") <|>
    ((\ nm args -> NotificationCall (Notification nm args))
        <$> o .: "method"
        <*> (o .: "params" <|> return None))
        
  parseJSON _ = fail "not an Object when parsing a JSONCall Value"  

-- The JSON RPC Monad
newtype RPC a = RPC (RemoteMonad Notification Method a)
  deriving (Functor, Applicative, Monad)
  
-- | The client-side send function API.
-- The user provides a way of dispatching this, to implement a client.
-- An example of this using wreq is found in remote-json-client
data SendAPI :: * -> * where
    Sync  :: Value -> SendAPI Value
    Async :: Value -> SendAPI ()

deriving instance Show (SendAPI a)

-- } The server-side recieived API.
-- The user provides a way of dispatching this, to implement a server.
-- An example of this using scotty is found in remote-json-server
data ReceiveAPI :: * -> * where
    Receive :: Value -> ReceiveAPI (Maybe Value)

deriving instance Show (ReceiveAPI a)

--(##) :: forall (c :: (* -> *) -> Constraint) f g m . (c m) => (f ~> m) -> (g ~> m)
--(##) = undefined
  
newtype Session = Session (forall a . RemoteMonad Notification Method a -> IO a) 


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


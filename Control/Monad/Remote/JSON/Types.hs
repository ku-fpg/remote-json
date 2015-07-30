{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module:      Control.Monad.Remote.JSON where
Copyright:   (C) 2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Monad.Remote.JSON.Types where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import           Data.Text(Text, append, pack, unpack)

import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding(decodeUtf8)
import           Data.Typeable
import qualified Data.Vector as V

data SessionAPI :: * -> * where
   Sync  :: Value -> SessionAPI Value
   Async :: Value -> SessionAPI ()

data TransportAPI :: * -> * where
   Send :: Value -> TransportAPI (Maybe Value)
     
transport :: (Functor f) => (forall a . TransportAPI a -> f a) -> (SessionAPI a -> f a)
transport f (Sync v)  = maybe (error "no returned value") id <$> f (Send v) 
transport f (Async v) = const ()                             <$> f (Send v)

data RemoteType = Strong | Weak
   deriving (Eq,Ord,Show)

data Call :: * -> * where
  Method         :: Text -> Args -> Value -> Call Value
  Notification   :: Text -> Args          -> Call ()

instance Show (Call a) where
   show (Method nm args tag) = unpack nm ++ show args ++ "#" ++ show tag
   show (Notification nm args) = unpack nm ++ show args

instance ToJSON (Call Value) where
  toJSON (Method nm args tag) = object $
          [ "jsonrpc" .= ("2.0" :: Text)
          , "method" .= nm
          , "id" .= tag
          ] ++ case args of
                 None -> []
                 _    -> [ "params" .= args ]


instance ToJSON (Call ()) where
  toJSON (Notification nm args) = object $
          [ "jsonrpc" .= ("2.0" :: Text)
          , "method" .= nm
          ] ++ case args of
                 None -> []
                 _    -> [ "params" .= args ]

instance FromJSON (Call Value) where           
  parseJSON (Object o) = Method <$> o .: "method"
                                <*> (o .: "params" <|> return None)
                                <*> o .: "id"
  parseJSON _ = fail "not an Object when parsing a Call Value"  

instance FromJSON (Call ()) where           
  parseJSON (Object o) = Notification <$> o .: "method"
                                      <*> (o .: "params" <|> return None)
  parseJSON _ = fail "not an Object when parsing a Call ()"  


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

data ErrorResponse = ErrorResponse Int Text Value

instance ToJSON ErrorResponse where
  toJSON (ErrorResponse code msg theId) = object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "error" .= object [ "code"  .= code
                                    , "message" .= msg
                                    ]
                , "id" .= theId
                ]

instance FromJSON ErrorResponse where
--  parseJSON (Object o) = ErrorResponse <$> o .: "method"
--                                       <*> (o .: "params" <|> return None)
  parseJSON _ = fail "not an Object when parsing a Call ()"  
        
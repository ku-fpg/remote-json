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

data Session = Session 
        { remoteMonad       :: RemoteType
        , remoteApplicative :: RemoteType
        , remoteSession     :: forall a. SessionAPI a -> IO a
        }
   
session :: (forall a . SessionAPI a -> IO a) -> Session
session = Session Weak Weak

data Call :: * -> * where
  Method         :: Text -> [Value] -> Value -> Call Value
  Notification   :: Text -> [Value] -> Call ()

instance Show (Call a) where
   show (Method nm args tag) = show (Notification nm args) ++ "#" ++ show tag
   show (Notification nm args) = unpack nm ++ 
           if  null args 
           then "()"
           else  concat [ t : LT.unpack (decodeUtf8 (encode x))
                        | (t,x) <- ('(':repeat ',') `zip` args 
                        ] ++ ")"

instance ToJSON (Call Value) where
  toJSON (Method nm args tag) = object $
          [ "jsonrpc" .= ("2.0" :: Text)
          , "method" .= nm
          , "params" .= args
          , "id" .= tag
          ]

instance ToJSON (Call ()) where
  toJSON (Notification nm args) = object $
          [ "jsonrpc" .= ("2.0" :: Text)
          , "method" .= nm
          , "params" .= args
          ]

instance FromJSON (Call Value) where           
  parseJSON (Object o) = Method <$> o .: "method"
                                <*> o .: "params"
                                <*> o .: "id"
  parseJSON _ = fail "not an Object when parsing a Call Value"  

instance FromJSON (Call ()) where           
  parseJSON (Object o) = Notification <$> o .: "method"
                                      <*> o .: "params"
  parseJSON _ = fail "not an Object when parsing a Call ()"  

newtype Tag = Tag Value deriving Show

instance FromJSON Tag where           
  parseJSON (Object o) = Tag <$> o .: "id"
  parseJSON _ = fail "not an Object when parsing a Tag"

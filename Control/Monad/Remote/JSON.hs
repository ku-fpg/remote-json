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

module Control.Monad.Remote.JSON where

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

method :: Text -> [Value] -> RPC Value
method = Method

notification :: Text -> [Value] -> RPC ()
notification = Notification

data Session = Session
  { sync  :: Value -> IO Value
  , async :: Value -> IO ()
  }

send :: Session -> RPC a -> IO a
send s (Pure a)   = return a
send s (Bind f k) = send s f >>= send s . k
send s (Ap f a)   = send s f <*> send s a
send s (Method nm args) = do
     let m = object [ "jsonrpc" .= ("2.0" :: Text)
     	       	    , "method" .= nm
		    , "args" .= args
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
		    , "args" .= args
		    ]
     async s m
     return ()

-- | '#' is a generic alias for 'send'.
instance Transformation RPC IO Session where
   (#) = send

server :: [ (Text, [Value] -> IO Value) ] -> Value -> IO (Maybe Value)
server db (Object o) = case parseMaybe p o of
        Just ("2.0",nm,Just (Array args),theId) -> call nm (V.toList args) theId
        _ -> return $ Just $ invalidRequest
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
                     <*> o .:? "args"
                     <*> o .:? "id"

server db _  = return $ Just $ invalidRequest

errorResponse :: Int -> Text -> Value -> Value
errorResponse code msg theId = object 
        [ "jsonrpc" .= ("2.0" :: Text)
        , "error" .= object [ "code"  .= code
                            , "message" .= msg
                            ]
        , "id" .= theId
        ]


invalidRequest :: Value
invalidRequest = errorResponse (-32600) "Invalid Request" Null

methodNotFound :: Value -> Value
methodNotFound = errorResponse (-32601) "Method not found"
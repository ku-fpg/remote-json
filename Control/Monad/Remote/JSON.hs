{-# LANGUAGE DeriveDataTypeable #-}
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

module Control.Monad.Remote.JSON(
        -- * JSON-RPC DSL
        RPC,
        command,
        procedure,
        result,
        -- * Invoke the JSON RPC Remote Monad
        send,
        Session(..),
        defaultSession,
        -- * Route the server-side JSON RPC calls
        router
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Transformation
import           Control.Concurrent.MVar

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text(Text, append, pack, unpack)
import           Data.Typeable
import qualified Data.Vector as V

data RPC :: * -> * where
    Pure         :: a ->                              RPC a
    Bind         :: RPC a -> (a -> RPC b) ->          RPC b
    Ap           :: RPC (a -> b) -> RPC a ->          RPC b
    Procedure       :: Text -> [Value] -> RPC Value
    Command :: Text -> [Value] ->                RPC ()
    Fail         :: String ->                         RPC a
  deriving Typeable

instance Functor RPC where
  fmap f m = pure f <*> m 

instance Applicative RPC where
  pure = Pure
  (<*>) = Ap

instance Monad RPC where
  return = pure
  (>>=) = Bind
  fail  = Fail

command :: Text -> [Value] -> RPC ()
command nm args = Command nm args 

procedure :: Text -> [Value] -> RPC Value
procedure nm args = Procedure nm args  

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
  , queue :: MVar [Value]
  , session_id :: MVar Value -- Int
  } deriving Typeable

defaultSession :: (Value->IO Value) -> (Value->IO ())-> IO(Session)
defaultSession synch asynch= do id <- newMVar (Number 0)
                                q  <- newMVar []
                                return Session {sync=synch,async=asynch,queue=q, session_id= id}

send :: Session ->RPC a -> IO a
send s x = do val <- send' s x
              flush s
              return val

flush :: Session -> IO ()
flush s = do let qmvar = queue s
             q <- takeMVar qmvar
             sequence_ $ map (async s) q
             putMVar qmvar []


-- 'send' the JSON-RPC call, using a weak remote monad.
send' :: Session -> RPC a -> IO a
send' s (Pure a)   = do flush s
                        return a
send' s (Bind f k) = send' s f >>= send' s . k
send' s (Ap f a)   = send' s f <*> send' s a

send' s (Procedure nm args) = do
     -- Send Queue of async
     flush s
     let mvar = session_id s
     (Number tmp) <- takeMVar mvar
     let sessionId = Number (tmp +1)
     putMVar mvar sessionId

     let m = object [ "jsonrpc" .= ("2.0" :: Text)
                    , "method" .= nm
                    , "params" .= args
                    , "id" .= sessionId
                    ]
     v <- sync s m
     let p :: Object -> Parser (Text,Value, Maybe Value)
         p o =  (,,) <$> o .: "jsonrpc"
                    <*> o .: "result"
                    <*> o .: "id"

     let p2 :: Object -> Parser (Text,Value)
         p2 o =  (,) <$> o .: "jsonrpc"
                     <*> o .: "error"

     let p3 :: Object -> Parser (Value,Value)
         p3 o =  (,) <$> o .: "code"
                     <*> o .: "message"
     let package = (append . (flip append (" ")) . pack . show)
     let handleError:: Object -> IO Value
         handleError o = do putStr "Error: "
                            case parseMaybe p3 o of
                              Just ((Number code),(String mesg)) -> 
                                  -- Create error message
                                  return $ String $ package code mesg
                              _  -> return Null
     case v of
       Object o -> case parseMaybe p o of
                 Just ("2.0",v', (Just retId)) -> do
                                              print o
                                              if retId == sessionId then
                                                 return v'
                                              else
                                                 fail "ID's didn't match"
                 _               -> case parseMaybe p2 o of
                                     Just("2.0",(Object v')) -> handleError v'  
                                      
                                     _              -> do return Null
       _ -> return Null
send' s (Command nm args) = do
       let mvar = queue s
       q <- takeMVar mvar

       let m = object [ "jsonrpc" .= ("2.0" :: Text)
                    , "method" .= nm
                    , "params" .= args
                    ]
       putMVar mvar (q++[m])

-- | Allow 'Session' to use '(#)' as a generic alias for 'send'.
instance Transformation RPC IO Session where
   (#) = send'

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
               Nothing -> do fail $ "Method: "++(unpack nm)++" not found"
                             return $ Nothing

        p :: Object -> Parser (Text,Text,Maybe Value,Maybe Value)
        p o' =  (,,,) <$> o' .:  "jsonrpc" 
                      <*> o' .:  "method"
                      <*> o' .:? "params"
                      -- We parse "id" directly, because "id":null is
                      -- not the same as having no "id" tag in JSON-RPC.
                      <*> optional (o' .: "id")

-- server db _  = return $ Just $ invalidRequest

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

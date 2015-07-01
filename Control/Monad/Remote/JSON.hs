{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

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
        method,
        notification,
        result,
        -- * Invoke the JSON RPC Remote Monad
        send,
        Session(..),
        RemoteType(..),
        defaultSession,
        -- * Route the server-side JSON RPC calls
        router,
        routerDebug
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State

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

-- We use the terms method and notification because this is the terminology
-- used by JSON-RPC. They *are* remote monad procedures and commands.

method :: Text -> [Value] -> RPC Value
method nm args = Procedure nm args

notification :: Text -> [Value] -> RPC ()
notification nm args = Command nm args

-- | Utility for parsing the result, or failing
result :: (Monad m, FromJSON a) => m Value -> m a
result m = do
        Success r <- liftM fromJSON m
        return r

data SessionAPI :: * -> * where
   Sync  :: Value -> SessionAPI Value
   Async :: Value -> SessionAPI ()

type CommandList = [Value]
type SessionID = Int
type MyState = (CommandList, SessionID)

data RemoteType = Strong | Weak
   deriving Eq

data Session = Session RemoteType (forall a. (SessionAPI a) -> IO a)

defaultSession :: RemoteType -> (Value->IO Value) -> (Value->IO ())-> Session
defaultSession t sync async = do
      let interp :: SessionAPI a -> IO a
          interp (Sync v) = sync v
          interp (Async vs) = async vs

      (Session t interp)

send :: Session -> RPC a -> IO a
send (Session t interp) v = do
         case t of
           Weak   -> evalStateT (send' (Session t interp) v) ([],1)
           Strong -> do (a,s)<-runStateT (send' (Session t interp) v) ([],1)
                        case s of
                          ([],_) -> return a
                          (xs,_) -> do interp (Async (toJSON xs))
                                       return a


send' :: Session -> RPC a -> StateT MyState IO a
send' _ (Pure a) = return a
send' s (Bind f k) = send' s f >>= send' s . k
send' s (Ap f a) = send' s f <*> send' s a
send' (Session t interp) (Procedure nm args) = do

      (q,sessionId) <- get
      when ((q /= []) && t == Strong) $ liftIO $ interp (Async (toJSON q))

      put ([],sessionId + 1)

      let m = object [ "jsonrpc" .= ("2.0" :: Text)
                     , "method" .= nm
                     , "params" .= args
                     , "id" .= Just (toJSON sessionId)
                     ]

      v <- liftIO $ interp (Sync m)

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
                                               if retId == (toJSON sessionId) then
                                                 return v'
                                               else
                                                 fail "ID's didn't match"
                  _               -> case parseMaybe p2 o of
                                      Just("2.0",(Object v')) -> liftIO $ handleError v'

                                      _              -> do return Null
        _ -> return Null


send' (Session t interp) (Command nm args) = do
      let m = object [ "jsonrpc" .= ("2.0" :: Text)
                     , "method" .= nm
                     , "params" .= args
                     ]
      case t of
         Strong -> do (list, id') <-get
                      put (list ++ [m], id')
         Weak -> liftIO $ interp (Async m)


-- | 'router' takes a list of name/function pairs,
-- and dispatches them, using the JSON-RPC protocol.
router :: [ (Text, [Value] -> IO Value) ] -> Value -> IO (Maybe Value)
router = routerDebug False

-- | Like 'router', but with the ability to configure whether debug output is
-- printed to the screen.
routerDebug :: Bool -> [ (Text, [Value] -> IO Value) ] -> Value -> IO (Maybe Value)
routerDebug debug db (Array a) = do
    let cmds = V.toList a
    when debug $ print cmds
    res <- sequence $ map (routerDebug debug db) cmds
    return $ Just $ object
           [ "jsonrpc" .= ("2.0" :: Text)
           , "result" .= (toJSON res)
           ]

routerDebug debug db (Object o) = do
     when debug $ print (o,parseMaybe p o)
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
               Nothing -> do _ <- fail $ "Method: "++(unpack nm)++" not found"
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

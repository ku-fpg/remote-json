{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

module Control.Monad.Remote.JSON.Router where
        
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Remote.JSON.Types
import           Control.Monad.State

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text(Text, append, pack, unpack)
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding(decodeUtf8)
import           Data.Typeable
import qualified Data.Vector as V


-- | "The Server MAY process a batch rpc call as a set of concurrent tasks,
--    processing them in any order and with any width of parallelism."
--   We control this using the first argument.         
router :: MonadThrow m 
       => (forall a. [m a] -> m [a])
       -> (forall a . Call a -> m a) 
       -> TransportAPI a -> m a
router s f (Send v@(Object {})) = simpleRouter f v
router s f (Send v@(Array a)) = do
        rs <- s (map (simpleRouter f) $ V.toList a)
        case [ v | Just v <- rs ] of
          [] -> return Nothing -- If there are no Response objects contained within the
                               -- Response array as it is to be sent to the client,
                               -- the server MUST NOT return an empty Array and should
                               -- return nothing at all.
          vs -> return (Just (toJSON vs))
rounter s f _ = return $ Just $ invalidRequest
        
simpleRouter :: forall m . MonadThrow m 
       => (forall a . Call a -> m a) 
       -> Value -> m (Maybe Value)
simpleRouter f v = case parser of
    Success m -> m
    Error e1 ->  return $ Just $ errorResponse (-32601) "Method not found" v
  where
        -- This does not support all the error messages (yet)
        parser :: Result (m (Maybe Value))
        parser = fmap Just            <$> meth <$> (fromJSON v :: Result (Call Value))
             <|> fmap (const Nothing) <$> f    <$> (fromJSON v :: Result (Call ()))

        meth :: Call Value -> m Value
        meth (Method nm args tag) = do
                v <- f (Method nm args tag)
                return $ object
                       [ "jsonrpc" .= ("2.0" :: Text)
                       , "result" .= v
                       , "id" .= tag
                       ]
                


{-
router' opts call v@(Array a) = do
        rs <- mapM (simpleRouter opts call) $ V.toList a
        case [ v | Just v <- rs ] of
          [] -> return Nothing
          vs -> return (Just (toJSON vs))
router' opts call _ = error "router'" -- TODO fill with the correct value to return



-- | 'router' takes a list of name/function pairs,
-- and dispatches them, using the JSON-RPC protocol.
router :: MonadIO io => [ (Text, [Value] -> io Value) ] -> Value -> io (Maybe Value)
router = routerDebug False

-- | Like 'router', but with the ability to configure whether debug output is
-- printed to the screen.
routerDebug :: forall io. MonadIO io => Bool -> [ (Text, [Value] -> io Value) ] -> Value -> io (Maybe Value)
routerDebug debug db (Array a) = do
    let cmds = V.toList a
    when debug . liftIO $ print cmds
    res <- sequence $ map (routerDebug debug db) cmds
    return $ Just $ object
           [ "jsonrpc" .= ("2.0" :: Text)
           , "result" .= (toJSON res)
           ]

routerDebug debug db (Object o) = do
     when debug . liftIO $ print (o,parseMaybe p o)
     case parseMaybe p o of
        Just ("2.0",nm,Just (Array args),theId) -> call nm (V.toList args) theId
        Just (_,_,_,theId) -> return $ Just $ invalidRequest theId
        _ -> return $ Just $ invalidRequest Nothing
  where
        -- Handles both method and notification, depending on the id value.
        call :: Text -> [Value] -> Maybe Value -> io (Maybe Value)
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
-}
     
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

-- For use when parsing to a Send fails
parseError :: Value
parseError = errorResponse (-32700) "Parse error" Null

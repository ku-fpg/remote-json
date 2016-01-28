{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Remote.Fresh 
(  method
 , notification
 , weakSession
 , strongSession
 , send
) where

import           Control.Remote.Monad
import qualified Control.Remote.Monad.Packet.Weak as WP
import qualified Control.Remote.Monad.Packet.Strong as SP
import           Control.Monad.Remote.JSON.Types (Args(..), SessionAPI(..))

import  qualified         Data.Text as Text
import           Data.Aeson 
import           Control.Monad.State


data Command :: * where
   Notification ::Text.Text -> Args -> Command

data Procedure :: * -> * where
   Method     :: Text.Text -> Args -> Procedure Value

procedureToJSON :: Procedure a -> Value -> Value
procedureToJSON (Method nm args) tag  = object $      
 [ "jsonrpc" .= ("2.0" :: Text.Text)      
 , "method" .= nm                    
 , "id" .= tag                       
 ] ++ case args of                   
        None -> []                   
        _    -> [ "params" .= args ] 


commandToJSON ::Command ->  Value
commandToJSON (Notification nm args) = object $    
 [ "jsonrpc" .= ("2.0" :: Text.Text)      
 , "method" .= nm                    
 ] ++ case args of                   
        None -> []                   
        _    -> [ "params" .= args ] 
                                     
parseProcedureResult :: Procedure a -> Value -> a
parseProcedureResult (Method {}) v = v 

method :: Text.Text -> Args -> RemoteMonad Command Procedure Value
method nm args = procedure (Method nm args)


notification :: Text.Text -> Args -> RemoteMonad Command Procedure ()
notification nm args = command (Notification nm args)

runWeakRPC :: (forall a . SessionAPI a -> IO a) -> WP.WeakPacket Command Procedure a -> IO a
runWeakRPC f (WP.Command n)   = f (Async (commandToJSON n))
runWeakRPC f (WP.Procedure m) = do
          v<- f (Sync (procedureToJSON m (Number 1)))
          return (parseProcedureResult m v)


runStrongRPC :: (forall a . SessionAPI a -> IO a) -> SP.StrongPacket Command Procedure a ->  IO a
runStrongRPC f packet =evalStateT (go f packet) []
      where
            go :: (forall a . SessionAPI a -> IO a) -> SP.StrongPacket Command Procedure a -> StateT [Command] IO a
            go f (SP.Command n cs) = do 
                                      modify $ \st -> st ++ [n]
                                      go f cs
            go _ (SP.Done) =  liftIO $ return ()
            go  f (SP.Procedure m) = do 
                            st <- get
                            put []
                            let toSend = (map commandToJSON st) ++ [procedureToJSON m (Number 1)]
                            v <- liftIO $ f (Sync $ toJSON toSend)
                            return (parseProcedureResult m v)



newtype Session = Session (forall a . RemoteMonad Command Procedure a -> IO a) 

weakSession :: (forall a . SessionAPI a -> IO a) -> Session
weakSession f = Session $ \ m -> runMonad (runWeakRPC f) m

strongSession :: (forall a . SessionAPI a -> IO a) -> Session
strongSession f = Session $ \ m -> (runMonad (runStrongRPC f) m)

send :: Session -> RemoteMonad Command Procedure a -> IO a
send (Session f) m = f m

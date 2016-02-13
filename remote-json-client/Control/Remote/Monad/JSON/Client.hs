{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Control.Remote.Monad.JSON.Client
        ( clientSendAPI
        ) where

import Control.Lens ((^.))
import Control.Monad (void)
import Control.Natural
import Control.Remote.Monad.JSON (SendAPI(..))
import qualified Control.Remote.Monad.JSON.Transport  as T
import Data.ByteString.Lazy (toStrict)
import Network.Wreq

-- Public APIs
-- http://www.raboof.com/projects/jayrock/demo.ashx
-- https://s1.ripple.com:51234/ 

-- | A way of building client 'SendAPI' support, using wreq.
clientSendAPI :: String -> (SendAPI :~> IO)
clientSendAPI url = T.transport f
   where 
      f = nat $ \ case
        (T.Sync_bs v) -> do
                    r <-  post url v
                    return $ toStrict $ r ^. responseBody
        (T.Async_bs v) -> do
                    void $ post url v



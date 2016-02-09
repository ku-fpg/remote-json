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
import Data.Aeson
import Network.Wreq

-- Public APIs
-- http://www.raboof.com/projects/jayrock/demo.ashx
-- https://s1.ripple.com:51234/ 

-- | A way of building client 'SendAPI' support, using wreq.
clientSendAPI :: String -> (SendAPI :~> IO)
clientSendAPI url = nat $ \ case
  Sync v -> do
          r <- asJSON =<< post url (toJSON v)
          return $ r ^. responseBody
  Async v -> do
          void $ post url (toJSON v)

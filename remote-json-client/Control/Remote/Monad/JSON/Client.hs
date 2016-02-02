{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Remote.Monad.JSON.Client
        ( clientSendAPI
        ) where

import Control.Lens ((^.))
import Control.Monad (void)
import Control.Remote.Monad.JSON.Types (SendAPI(..))
import Data.Aeson
import Network.Wreq

-- Public APIs
-- http://www.raboof.com/projects/jayrock/demo.ashx
-- https://s1.ripple.com:51234/ 

-- | A way of building client 'SendAPI' support, using wreq.
clientSendAPI :: String -> (forall a . SendAPI a -> IO a)
clientSendAPI url (Sync v) = do
          r <- asJSON =<< post url (toJSON v)
          return $ r ^. responseBody
clientSendAPI url (Async v) = do
          void $ post url (toJSON v)

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Remote.JSON.Client
        ( clientSessionAPI
        ) where

import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.Remote.JSON.Types (SessionAPI(..))
import Data.Aeson
import Network.Wreq

-- Public APIs
-- http://www.raboof.com/projects/jayrock/demo.ashx
-- https://s1.ripple.com:51234/ 

clientSessionAPI :: String -> (forall a . SessionAPI a -> IO a)
clientSessionAPI url (Sync v) = do
          r <- asJSON =<< post url (toJSON v)
          return $ r ^. responseBody
clientSessionAPI url (Async v) = do
          void $ post url (toJSON v)

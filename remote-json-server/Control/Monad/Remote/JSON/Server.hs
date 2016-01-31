{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Remote.JSON.Server
        ( serverReceiveAPI
        ) where

import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.Remote.JSON.Types (ReceiveAPI(..))
import Data.Aeson

-- | A way of building client 'SendAPI' support, using wreq.
serverReceiveAPI :: (forall a . ReceiveAPI a -> IO a) -> IO ()
serverReceiveAPI f = return ()

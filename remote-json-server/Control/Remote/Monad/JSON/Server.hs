{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Control.Remote.Monad.JSON.Server
        ( -- * Ways of building the actionable parts of a Scotty server
          scottyReceiveAPI
        , serverReceiveAPI
        ) where

import Control.Remote.Monad.JSON.Router (ReceiveAPI(..))
import           Control.Monad.IO.Class
import Control.Natural
import Data.Aeson()
import Data.Default.Class (def)
import Web.Scotty (scottyOpts)
import Web.Scotty.Trans as T
import Network.Wai.Handler.Warp (setPort)


-- | Blocking function that listens on a specific port, to a specific path.
serverReceiveAPI :: Int -> String -> (ReceiveAPI :~> IO) -> IO ()
serverReceiveAPI port path f = scottyOpts opts $ post (literal path) $ scottyReceiveAPI f
  where opts = def { verbose = 0, settings = setPort port (settings def) }

-- | Build the 'ActionT' action for Scotty.
scottyReceiveAPI :: (ScottyError e, MonadIO m)
                 => (ReceiveAPI :~> IO) -> ActionT e m ()
scottyReceiveAPI (NT f) = do
                d <- jsonData
                r <- liftIO $ f $ Receive $ d
                case r of
                  Nothing -> return ()
                  Just v -> T.json v

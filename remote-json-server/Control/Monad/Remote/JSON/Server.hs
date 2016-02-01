{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Remote.JSON.Server
        ( -- * Ways of building the actionable parts of a Scotty server
          scottyReceiveAPI
        , serverReceiveAPI
        ) where

import Control.Monad (void, forM_)
import Control.Monad.Remote.JSON.Types (ReceiveAPI(..))
import           Control.Monad.IO.Class
import Data.Aeson
import Data.Default.Class (def)
import Data.Text (Text)
import Web.Scotty (scottyOpts)
import Web.Scotty.Trans as T
import Network.Wai.Handler.Warp (setPort)


-- | A way of building client 'SendAPI' support, using wreq.
{-
      serverReceiveAPI :: forall e (m :: * -> *) (t :: * -> *) a.
                          (Foldable t, ToJSON a, MonadIO m, ScottyError e) =>
                          (ReceiveAPI (Maybe Value) -> IO (t a)) -> ActionT e m ()
-}
-- | Blocking function that listens on a specific port, to a specific path.
serverReceiveAPI :: Int -> String -> (forall a . ReceiveAPI a -> IO a) -> IO ()
serverReceiveAPI port path f = scottyOpts opts $ post (literal path) $ scottyReceiveAPI f
  where opts = def { verbose = 0, settings = setPort port (settings def) }

-- | Build the 'ActionT' action for Scotty.
scottyReceiveAPI :: (ScottyError e, MonadIO m)
                 => (forall a . ReceiveAPI a -> IO a) -> ActionT e m ()
scottyReceiveAPI f = do
                d <- jsonData
                r <- liftIO $ f $ Receive $ d
                case r of
                  Nothing -> return ()
                  Just v -> T.json v

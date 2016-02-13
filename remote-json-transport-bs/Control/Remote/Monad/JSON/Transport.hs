{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Remote.Monad.JSON.Transport
(
  SendAPI_bs (..)
, transport
)
where
import Control.Monad (void)
import Control.Natural
import Control.Remote.Monad.JSON (SendAPI(..))
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Internal (ByteString)
import Data.Aeson (decodeStrict, encode  )

data SendAPI_bs :: * -> * where
  Sync_bs  :: ByteString  -> SendAPI_bs ByteString
  Async_bs :: ByteString  -> SendAPI_bs ()
 
transport :: (Monad m) => (SendAPI_bs :~> m) -> (SendAPI :~> m)
transport (Nat f) = nat $ \ case
          (Sync v) -> do 
                       r <- f (Sync_bs (toStrict $ encode v))
                       case decodeStrict r of
                         Just v'    -> return v'
                         _          -> fail "Error in Transport"
          (Async v) -> void $ f (Async_bs (toStrict $ encode v))
                       


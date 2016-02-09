{-# LANGUAGE GADTs, OverloadedStrings, TypeOperators #-}

module Main where

import Control.Natural ((:~>), nat)
import Control.Remote.Monad.JSON
import Control.Remote.Monad.JSON.Router(transport,router,Call(..),methodNotFound)
import Data.Aeson
import Data.Text(Text)

-- Our small DSL

say :: Text -> RPC ()
say msg = notification "say" (List [String msg])

temperature :: RPC Int
temperature = method "temperature" None

-- Our remote program

main :: IO ()
main = do
  let s = weakSession network
  t <- send s $ do
                say "Hello, "
                say "World!"
                temperature
  print t                    

-- Simulate the JSON-RPC server

network :: SendAPI :~> IO
network = transport $ router sequence $ nat remote
  where
    remote :: Call a -> IO a
    remote (CallMethod "temperature" _)                 = return $ Number 42
    remote (CallNotification "say" (List [String msg])) = print msg
    remote _                                            = methodNotFound
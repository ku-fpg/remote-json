{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Session (sessionBuilders,routerBuilders) where

import Control.Remote.Monad.JSON
import Control.Remote.Monad.JSON.Trace
import Control.Remote.Monad.JSON.Router
import Data.Monoid
import Data.Aeson
import Data.Text (pack)
import qualified Data.Text.IO as IO
import System.Random
import Data.Scientific as Scientific
import Control.Natural

sessionBuilders :: [(SendAPI :~> IO) -> Session]
sessionBuilders =
  [ f . t
  | f :: (SendAPI :~> IO) -> Session <- [ weakSession, strongSession, applicativeSession ]
  , t <- [ id
         , traceSendAPI "send"
         ]
  ]

routerBuilders :: [(ReceiveAPI :~> IO)]
routerBuilders =
  [ t1 $ router sequence (t2 $ wrapNT remote)
  | t1 <- [ id, traceReceiveAPI "receive" ]
  , t2 <- [ id, traceCallAPI    "call"    ]
  ]

remote :: Call a -> IO a
remote (CallNotification "say" args) = case args of
    List [String txt] -> do
        IO.putStrLn $ "remote: " <> txt
        return ()
    _ ->  invalidParams
remote (CallMethod "temperature" _) = do
        t <- randomRIO (50, 100 :: Int)
        IO.putStrLn $ "temperature: " <> pack (show t)
        return $ toJSON t
remote (CallMethod "fib" args) = case args of
    List [Number n] -> do
        case toBoundedInteger n of
          Just i -> return $ Number $ fromIntegral $ fib $ i
          _ -> invalidParams
    _ ->  invalidParams
  where fib :: Int -> Int
        fib n = if n < 2 then 1 else fib(n-1)+fib(n-2)
remote _ = methodNotFound


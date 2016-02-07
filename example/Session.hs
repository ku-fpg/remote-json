{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Session (sessionBuilders,routerBuilders) where

import Control.Remote.Monad.JSON
import Control.Remote.Monad.JSON.Types -- TODO RM
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
  [ \ (api :: SendAPI :~> IO) -> f ((t api) $$)
  | f :: (SendAPI ~> IO) -> Session <- [ weakSession, strongSession, applicativeSession ]
  , t <- [ id 
         , \ api -> Nat $ traceSendAPI "send"  (api $$) 
         ]
  ]

routerBuilders :: [(ReceiveAPI :~> IO)]
routerBuilders = 
  [ Nat $ (t1 (Nat $ router sequence (t2 (Nat $ remote) $$)) $$)
  | t1 <- [ id,\ api -> Nat $ traceReceiveAPI "receive" (api $$) ]
  , t2 <- [ id, \ api -> Nat $ traceCallAPI     "call" (api $$) ]
  ]
  
remote :: Call a -> IO a
remote (CallNotification nm args)    = remoteCommand (Notification nm args)
remote (CallMethod nm args)          = remoteProcedure (Method nm args)

remoteCommand :: Notification -> IO ()
remoteCommand (Notification "say" args) = case args of
    List [String txt] -> do
        IO.putStrLn $ "remote: " <> txt
        return ()
    _ ->  invalidParams
remoteCommand _ = methodNotFound

remoteProcedure :: Method a -> IO a
remoteProcedure (Method "temperature" _) = do
        t <- randomRIO (50, 100 :: Int)
        IO.putStrLn $ "temperature: " <> pack (show t)
        return $ toJSON t
remoteProcedure (Method "fib" args) = case args of
    List [Number n] -> do
        case toBoundedInteger n of
          Just i -> return $ Number $ fromIntegral $ fib $ i
          _ -> invalidParams
    _ ->  invalidParams
  where fib :: Int -> Int
        fib n = if n < 2 then 1 else fib(n-1)+fib(n-2)
remoteProcedure _ = methodNotFound


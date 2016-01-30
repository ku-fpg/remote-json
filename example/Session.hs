{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Session (sessions) where

import Control.Monad        
import Control.Monad.Remote.JSON
import Control.Monad.Remote.JSON.Debug
import Control.Monad.Remote.JSON.Router
import Control.Monad.Remote.JSON.Types (transport)
import Control.Applicative
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as IO
import System.Random
import Data.Scientific as Scientific

sessions :: [Session]
sessions = concat
         [ [ s
           , s { remoteMonad = Strong }
           ]
         | s <- sessions'
         ]

sessions' :: [Session]
sessions' = 
  [ session $ transport $ router sequence $ remote 
  , session $ traceSendAPI "session"
            $ transport                     $ router sequence $ remote
  , session $ transport $ traceReceiveAPI "transport"
                        $ router sequence $ remote
  , session $ transport $ router sequence $ traceCallAPI "call"
                                          $ remote ]


remote :: Call a -> IO a
remote (Notification "say" args) = case args of
    List [String txt] -> do
        IO.putStrLn $ "remote: " <> txt
        return ()
    _ ->  invalidParams
remote (Method "temperature" _ _) = do
        t <- randomRIO (50, 100 :: Int)
        IO.putStr $ "temperature: "
        return $ toJSON t
remote (Method "fib" args _) = case args of
    List [Number n] -> do
        case toBoundedInteger n of
          Just i -> return $ Number $ fromIntegral $ fib $ i
          _ -> invalidParams
    _ ->  invalidParams
  where fib :: Int -> Int
        fib n = if n < 2 then 1 else fib(n-1)+fib(n-2)
remote _ = methodNotFound


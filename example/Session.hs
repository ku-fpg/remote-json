{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Session (rootSession) where

import Control.Monad        
import Control.Monad.Remote.JSON
import Control.Monad.Remote.JSON.Router
import Control.Applicative
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as IO
import System.Random
import Data.Scientific as Scientific

rootSession :: Session --   sync                                async
rootSession = undefined

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
  where fib :: Int -> Integer
        fib n = if n < 2 then 1 else fib(n-1)+fib(n-2)
f _ = methodNotFound

